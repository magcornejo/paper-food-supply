#===============================================================================
# DESCRIPTION: Transforms data and runs regressions  
#===============================================================================

# Clean up workspace
rm(list=ls())

#====================================================================================
# LOAD REQUIRED PACKAGES
#====================================================================================

wants <- c("here","dplyr","plm","ggplot2","readxl","DescTools","rworldmap",
           "urca","stargazer","tidyverse","gridExtra","slider","sf",
           "rnaturalearth","rnaturalearthdata","fixest","lpirfs","panelvar",
           "purrr","stringr","cowplot")

needs <- wants[!(wants %in% installed.packages()[,"Package"])]
if(length(needs)) install.packages(needs)
lapply(wants, function(i) require(i, character.only=TRUE))
rm(needs,wants)
require("parallel") # base package

#====================================================================================
# DIRECTORIES
#====================================================================================
dir <- list()


#====================================================================================
# LOAD AND TRANSFORM MAIN DATASET
#====================================================================================
data <- read.csv(file.path(paths$data, "initial_panel.csv"))


full_data <- data %>%
  ungroup() %>%
  complete(Area, Year = full_seq(Year, 1)) %>%  # fill all years by country
  arrange(Area, Year) %>%
  group_by(Area) %>%
  mutate(
    DlogQ = (log(Q)-dplyr::lag(log(Q),1)),
    L1_DlogQ = dplyr::lag(DlogQ,1),
    L2_DlogQ = dplyr::lag(DlogQ,2),
    L1_logQ = dplyr::lag(Q,1)
  ) %>%
  ungroup()

# Number of observations per country
obs_x_country <- full_data %>% group_by(Area) %>% count(); obs_x_country

# Global food production and contribution of major producers (US, China, India, Russia)

subset <- full_data %>%
  filter(a3 %in% c("USA","CHN","IND","RUS"), Year >= 1960 & Year <= 2022)

subset <- subset %>%
  group_by(Year) %>%
  summarise(Qsum = sum(Q))

Q_world <- read.csv(file.path(paths$data, "FAOSTAT_world.csv"), sep = ";")
Q_world <- Q_world %>% filter(Año < 2023)  
prop <- subset$Qsum / Q_world$Valor         
mean(prop)


#====================================================================================
# INCORPORATE USDA DATA
#====================================================================================

usda <- read.csv(file.path(paths$data, "controls_USDA.csv"))
usda$Year <- as.numeric(usda$Year)

usda <- usda %>%
  filter(!is.na(ISO3)) 

full_data <- left_join(full_data, usda, by = c("a3" = "ISO3", "Year" = "Year"))

full_data <- full_data %>%
  dplyr::select(
    Area, Year, Q, Q_index, a3, region, latitude, longitude,
    DlogQ, L1_DlogQ, L2_DlogQ, L1_logQ,
    Land, Labor, Capital, Irrig, Fertilizer
  ) %>%
  mutate(
    Irrig_ratio = Irrig / Land,
    Fert_ratio  = Fertilizer / Land
  )


#====================================================================================
# MERGE TEMPERATURE DATA
#====================================================================================

temp_crop <- readxl::read_xlsx("temperature/wide_temp_cropland.xlsx")
temp_unw <- readxl::read_xlsx("temperature/wide_temp_unweighted.xlsx")

temp_crop <- temp_crop %>%
  pivot_longer(
    cols = `1901`:`2022`,
    names_to = "year",
    values_to = "temp_crop"
  )


temp_unw <- temp_unw %>%
  pivot_longer(
    cols = `1901`:`2022`,
    names_to = "year",
    values_to = "temp_unw"
  )

temp_crop$year <- as.numeric(temp_crop$year)
temp_unw$year <- as.numeric(temp_unw$year)
full_data$Year <- as.numeric(full_data$Year)
full_data <- left_join(full_data, temp_crop %>% dplyr::select(country_code,year,temp_crop), by=c("a3"="country_code","Year"="year"))
full_data <- left_join(full_data, temp_unw %>% dplyr::select(country_code,year,temp_unw), by=c("a3"="country_code","Year"="year"))

#====================================================================================
# CALCULATE LOCAL TEMPERATURE ANOMALIES (30-YEAR MOVING AVERAGE)
#====================================================================================

temp_crop <- temp_crop %>%
  arrange(country, year) %>%  # sort by country and year
  group_by(country) %>%
  mutate(
    temp_crop_ma30 = slide_dbl(temp_crop, mean, .before = 30, .after = -1, .complete = TRUE),
    temp_crop_anom = temp_crop - temp_crop_ma30
  ) %>%
  ungroup() %>%
  filter(country != "REST")

temp_unw <- temp_unw %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    temp_unw_ma30 = slide_dbl(temp_unw, mean, .before = 30, .after = -1, .complete = TRUE),
    temp_unw_anom = temp_unw - temp_unw_ma30
  ) %>%
  ungroup() %>%
  filter(country != "REST")

# Merge temperature anomalies into main dataset
full_data <- left_join(full_data, temp_crop %>% dplyr::select(country_code, year, temp_crop_anom),
                       by = c("a3" = "country_code", "Year" = "year"))
full_data <- left_join(full_data, temp_unw %>% dplyr::select(country_code, year, temp_unw_anom),
                       by = c("a3" = "country_code", "Year" = "year"))

# Remove duplicates by country-year
full_data <- full_data[!duplicated(full_data[c("Area", "Year")]), ]

#====================================================================================
# MERGE PRECIPITATION DATA
#====================================================================================

prcp_crop <- readxl::read_xlsx("precipitation/wide_pre_cropland.xlsx")
prcp_unw <- readxl::read_xlsx("precipitation/wide_pre_unweighted.xlsx")

prcp_crop <- prcp_crop %>%
  pivot_longer(
    cols = `1901`:`2022`,
    names_to = "year",
    values_to = "prcp_crop"
  )

prcp_unw <- prcp_unw %>%
  pivot_longer(
    cols = `1901`:`2022`,
    names_to = "year",
    values_to = "prcp_unw"
  )

prcp_crop$year <- as.numeric(prcp_crop$year)
prcp_unw$year <- as.numeric(prcp_unw$year)
full_data$Year <- as.numeric(full_data$Year)

# Merge precipitation data with main dataset
full_data <- left_join(full_data, prcp_crop %>% dplyr::select(country_code, year, prcp_crop),
                       by = c("a3" = "country_code", "Year" = "year"))
full_data <- left_join(full_data, prcp_unw %>% dplyr::select(country_code, year, prcp_unw),
                       by = c("a3" = "country_code", "Year" = "year"))

#====================================================================================
# CALCULATE LOCAL PRECIPITATION ANOMALIES (30-YEAR MOVING AVERAGE)
#====================================================================================

prcp_crop <- prcp_crop %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    prcp_crop_ma30 = slide_dbl(prcp_crop, mean, .before = 30, .after = -1, .complete = TRUE),
    prcp_crop_anom = prcp_crop - prcp_crop_ma30
  ) %>%
  ungroup() %>%
  filter(country != "REST")

prcp_unw <- prcp_unw %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    prcp_unw_ma30 = slide_dbl(prcp_unw, mean, .before = 30, .after = -1, .complete = TRUE),
    prcp_unw_anom = prcp_unw - prcp_unw_ma30
  ) %>%
  ungroup() %>%
  filter(country != "REST")

full_data <- left_join(full_data, prcp_crop %>% dplyr::select(country_code, year, prcp_crop_anom),
                       by = c("a3" = "country_code", "Year" = "year"))
full_data <- left_join(full_data, prcp_unw %>% dplyr::select(country_code, year, prcp_unw_anom),
                       by = c("a3" = "country_code", "Year" = "year"))

#====================================================================================
# FIGURE 1A: MEDIAN ANNUAL GROWTH RATE IN FOOD PRODUCTION (1961–2022)
#====================================================================================

# Compute median annual growth rate by country
data_growth_rate <- full_data %>%
  filter(Year >= 1961 & Year <= 2022) %>%
  group_by(a3) %>%
  arrange(a3, Year) %>%
  mutate(growth_rate = (log(Q_index) - dplyr::lag(log(Q_index))) * 100)

data_avg_growth_rate <- data_growth_rate %>%
  group_by(a3) %>%
  summarize(avg_growth_rate = median(growth_rate, na.rm = TRUE)) %>%
  mutate(
    growth_category = cut(
      avg_growth_rate,
      breaks = c(-Inf, 0, 1, 2, 3, Inf),
      labels = c("< 0", "0 – 1", "1 – 2", "2 – 3", "> 3"),
      right = FALSE
    ),
    growth_category = factor(
      growth_category,
      levels = c("< 0", "0 – 1", "1 – 2", "2 – 3", "> 3")
    )
  )

# Load world map and merge with growth data
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(continent != "Antarctica")  # exclude Antarctica

world <- world %>%
  mutate(iso_a3 = ifelse(name == "France", "FRA", iso_a3),
         iso_a3 = ifelse(name == "Norway", "NOR", iso_a3))

world_data_growth_rate <- left_join(world, data_avg_growth_rate, by = c("iso_a3" = "a3"))

# Custom color scale
custom_colors <- c(
  "< 0" = "red",
  "0 – 1" = "#f2d99b",
  "1 – 2" = "#a1d96b",
  "2 – 3" = "#41ab5d",
  "> 3" = "#238b45",
  "NA" = "gray90"
)

world_data_growth_rate$growth_category <-
  factor(world_data_growth_rate$growth_category,
         levels = c("< 0", "0 – 1", "1 – 2", "2 – 3", "> 3", "NA"))

# Assign 'NA' to missing categories
world_data_growth_rate$growth_category[is.na(world_data_growth_rate$growth_category)] <- "NA"


#====================================================================================
# MERGE EXTREME EVENTS: DROUGHTS AND FLOODS (from SPEI)
#====================================================================================

drought_crop <- readxl::read_xlsx("SPEI/wide_drought_cropland.xlsx")
drought_unw <- readxl::read_xlsx("SPEI/wide_drought_unweighted.xlsx")


drought_crop <- drought_crop %>%
  pivot_longer(
    cols = `1960`:`2020`,
    names_to = "year",
    values_to = "drought_crop"
  )
drought_unw <- drought_unw %>%
  pivot_longer(
    cols = `1960`:`2020`,
    names_to = "year",
    values_to = "drought_unw"
  )

drought_crop$year <- as.numeric(drought_crop$year)
drought_unw$year <- as.numeric(drought_unw$year)
full_data <- left_join(full_data, drought_crop %>% dplyr::select(country_code, year, drought_crop),
                       by = c("a3" = "country_code", "Year" = "year"))
full_data <- left_join(full_data, drought_unw %>% dplyr::select(country_code, year, drought_unw),
                       by = c("a3" = "country_code", "Year" = "year"))

flood_crop <- readxl::read_xlsx("SPEI/wide_flood_cropland.xlsx")
flood_unw <- readxl::read_xlsx("SPEI/wide_flood_unweighted.xlsx")


flood_crop <- flood_crop %>%
  pivot_longer(
    cols = `1960`:`2020`,
    names_to = "year",
    values_to = "flood_crop"
  )
flood_unw <- flood_unw %>%
  pivot_longer(
    cols = `1960`:`2020`,
    names_to = "year",
    values_to = "flood_unw"
  )

flood_crop$year <- as.numeric(flood_crop$year)
flood_unw$year <- as.numeric(flood_unw$year)

full_data <- left_join(full_data, flood_crop %>% dplyr::select(country_code, year, flood_crop),
                       by = c("a3" = "country_code", "Year" = "year"))
full_data <- left_join(full_data, flood_unw %>% dplyr::select(country_code, year, flood_unw),
                       by = c("a3" = "country_code", "Year" = "year"))


#====================================================================================
# FILTER DATSET
#====================================================================================

data_filtered <- full_data %>%
  filter(!is.na(Q)) # drop all observations with Q = NA

# Keep countries with at least 30 years of data
data_filtered <- data_filtered %>%
  group_by(a3) %>%
  filter(n() >= 30) %>%
  ungroup()

# Count number of countries
obs_x_country <- data_filtered %>% group_by(Area) %>% count(); obs_x_country  # 152 countries

# Identify countries with fertilizer data
countries_with_fert <- data_filtered %>%
  group_by(Area) %>%
  summarise(has_fert = any(!is.na(Fertilizer))) %>%
  filter(has_fert) %>%
  pull(Area)
countries_with_fert  # 140 countries with USDA control data

# Export filtered dataset
write.csv(data_filtered, file.path(paths$processed, "data_filtered.csv"), row.names = FALSE)

#====================================================================================
# CREATE VARIABLES FOR TEMPERATURE ANOMALY DECOMPOSITION
#====================================================================================

options(scipen = 999) 

data_filtered$temp_crop_anom_pos <- data_filtered$temp_crop_anom*ifelse(data_filtered$temp_crop_anom>=0,1,0)
data_filtered$temp_crop_anom_neg <- data_filtered$temp_crop_anom*ifelse(data_filtered$temp_crop_anom<0,1,0)

data_filtered$prcp_crop_anom_pos <- data_filtered$prcp_crop_anom*ifelse(data_filtered$prcp_crop_anom>=0,1,0)
data_filtered$prcp_crop_anom_neg <- data_filtered$prcp_crop_anom*ifelse(data_filtered$prcp_crop_anom<0,1,0)


# CROSS-SECTION DEPENDENCE TEST

pcdtest(
  DlogQ ~ plm::lag(DlogQ) + plm::lag(log(Q),1) + plm::lag(log(Labor),1) +
    plm::lag(log(Capital),1) + plm::lag(log(Land),1) +
    I(temp_crop_anom_pos - plm::lag(temp_crop_anom_pos,1)) +
    I(temp_crop_anom_neg - plm::lag(temp_crop_anom_neg,1)) +
    I(log(Fert_ratio) - plm::lag(log(Fert_ratio),1)) +
    I(log(Capital) - plm::lag(log(Capital),1)) +
    I(log(Labor) - plm::lag(log(Labor),1)) +
    I(log(Land) - plm::lag(log(Land),1)) + drought_crop,
  data = data_filtered,
  index = c("a3", "Year"),
  effect = "twoways"
)

#====================================================================================
# ESTIMATE BASELINE TWO-WAY FIXED-EFFECTS MODEL
#====================================================================================

reg_crop_full2 <- feols(
  DlogQ ~ l(DlogQ,1) + l(log(Q),1) + l(log(Labor),1) + l(log(Capital),1) +
    l(log(Land),1) +
    I(temp_crop_anom_pos - l(temp_crop_anom_pos,1)) +
    I(temp_crop_anom_neg - l(temp_crop_anom_neg,1)) +
    I(log(Fert_ratio) - l(log(Fert_ratio),1)) +
    I(log(Capital) - l(log(Capital),1)) +
    I(log(Labor) - l(log(Labor),1)) +
    I(log(Land) - l(log(Land),1)) + drought_crop | a3 + Year,
  data_filtered,
  panel.id = ~ a3 + Year,
  DK ~ Year
)  

# Extract fixed effects and compute average panel length
fe_crop_full2 <- fixef(reg_crop_full2)
estimation_sample <- fixest_data(reg_crop_full2, sample = "estimation")

panel_stats <- estimation_sample %>%
  group_by(a3) %>%
  summarise(T = n_distinct(Year), .groups = "drop")

average_T <- mean(panel_stats$T)
cat("Average T (years per country):", average_T, "\n")

#====================================================================================
# GLOBAL TEMPERATURE DEVIATIONS AND PANEL REGRESSIONS
#====================================================================================

global_tempanom <- read_xlsx("annual_global_tempanom.xlsx")
summary(ur.df(global_tempanom$global_tempanom, type = "trend", selectlags = "BIC")) 

# Hansen et al. (2010) report a 14 °C baseline mean
global_tempanom$global_temp <- global_tempanom$global_tempanom + 14  

# Compute 30-year moving average and deviations
global_tempanom <- global_tempanom %>%
  arrange(year) %>%
  mutate(
    temp_ma_30 = slide_dbl(global_temp, mean, .before = 30, .complete = TRUE),
    temp_ma_30 = lag(temp_ma_30),            # exclude current year
    global_temp_dev = global_temp - temp_ma_30
  )

# Restrict to the study period
subset <- global_tempanom %>% filter(year >= 1960)

summary(ur.df(subset$global_temp_dev, type = "trend", selectlags = "BIC")) #es I(0)

data_filtered <- data_filtered %>%
  filter(Year >= 1960 & Year <= 2022) %>%
  mutate(Year = factor(Year, levels = sort(unique(Year))))

#====================================================================================
# FIGURE 1: GLOBAL AND LOCAL PATTERNS FOOD PRODUCTION GROWTH AND WARMING (1961-2022)
#====================================================================================

plot1A <- ggplot(data = world_data_growth_rate) +
  geom_sf(aes(fill = growth_category), color = "white", size = 0.1) +
  scale_fill_manual(
    values = custom_colors,
    name = "Median food production growth (% yr\u207B\u00B9)",
    na.value = "gray90"
  ) +
  labs(title = NULL) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "left",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.title = element_blank(),
    plot.margin = margin(6, 6, 6, 6)
  )

plot1B <- ggplot() +
  geom_boxplot(
    data = data_filtered,
    aes(x = Year, y = temp_crop_anom),
    color = "black",
    fill = "lightgrey",
    width = 0.5,
    outlier.size = 1
  ) +
  geom_line(
    data = global_tempanom %>% filter(year >= 1961 & year <= 2022),
    aes(
      x = factor(year, levels = sort(unique(data_filtered$Year))),
      y = global_temp_dev, group = 1
    ),
    color = "red",
    size = 1
  ) +
  geom_hline(yintercept = 0, color = "black") +
  labs(
    title = NULL,
    x = "Year",
    y = "Temperature deviations (\u00B0C)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.ticks = element_line(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6),
    plot.title = element_blank(),
    plot.margin = margin(6, 6, 6, 6)
  ) +
  ylim(-2, 3)

plot1A <- plot1A + labs(title = NULL) + theme(plot.title = element_blank())
plot1B <- plot1B + labs(title = NULL) + theme(plot.title = element_blank())

tagA <- textGrob("A", x = 0, y = 1, just = c("left","top"),
                 gp = gpar(fontface = "bold", fontsize = 12))
tagB <- textGrob("B", x = 0, y = 1, just = c("left","top"),
                 gp = gpar(fontface = "bold", fontsize = 12))

rowA <- arrangeGrob(tagA, plot1A, ncol = 2, widths = unit.c(unit(0.03, "npc"), unit(0.97, "npc")))
rowB <- arrangeGrob(tagB, plot1B, ncol = 2, widths = unit.c(unit(0.03, "npc"), unit(0.97, "npc")))

grid.arrange(rowA, rowB, ncol = 1, heights = c(1, 1.05))

postscript(
  file = file.path(paths$figures, "Figure1.eps"),
  width = 8,      
  height = 8,    
  horizontal = FALSE,
  onefile = FALSE,
  paper = "special"
)
grid.arrange(rowA, rowB, ncol = 1, heights = c(1, 1.05))
dev.off()


#------------------------------------------------------------------------------------
# Merge global temperature deviations into panel
#------------------------------------------------------------------------------------
data_filtered <- data_filtered %>%
  mutate(Year = as.numeric(as.character(Year)))  # primero a character, luego a numeric

data_filtered <- left_join(data_filtered, global_tempanom, by = c("Year" = "year"))

data_filtered <- data_filtered %>%
  group_by(Area) %>%
  mutate(trend = Year - min(Year) + 1) %>%
  ungroup()

#------------------------------------------------------------------------------------
# Cross-sectional dependence test with global temperature
#------------------------------------------------------------------------------------
pcdtest(DlogQ~plm::lag(DlogQ)+plm::lag(log(Q),1)+plm::lag(log(Labor),1)+plm::lag(log(Capital),1)+plm::lag(log(Land),1)+global_temp_dev+I(temp_crop_anom_pos-plm::lag(temp_crop_anom_pos,1))+I(temp_crop_anom_neg-plm::lag(temp_crop_anom_neg,1))+I(log(Fert_ratio)-plm::lag(log(Fert_ratio),1))+I(log(Capital)-plm::lag(log(Capital),1))+I(log(Labor)-plm::lag(log(Labor),1))+I(log(Land)-plm::lag(log(Land),1))+drought_crop+trend, data = data_filtered,
        index = c("a3", "Year"), effect = "individual")

#------------------------------------------------------------------------------------
# One-way FE model including global temperature and trend
#------------------------------------------------------------------------------------
reg_crop_full4 <- feols(DlogQ~l(DlogQ,1)+l(log(Q),1)+l(log(Labor),1)+l(log(Capital),1)+l(log(Land),1)+global_temp_dev+I(temp_crop_anom_pos-l(temp_crop_anom_pos,1))+I(temp_crop_anom_neg-l(temp_crop_anom_neg,1))+I(log(Fert_ratio)-l(log(Fert_ratio),1))+I(log(Capital)-l(log(Capital),1))+I(log(Labor)-l(log(Labor),1))+I(log(Land)-l(log(Land),1)) + drought_crop + trend  | a3 , data_filtered, panel.id = ~ a3 + Year, DK ~ Year) #one-way FE

# Compute average number of years per country
estimation_sample <- fixest_data(reg_crop_full4, sample = "estimation")
panel_stats <- estimation_sample %>%
  group_by(a3) %>%
  summarise(T = n_distinct(Year), .groups = "drop")
average_T <- mean(panel_stats$T)
cat("Average T (years per country):", average_T, "\n")


#------------------------------------------------------------------------------------
# TABLE 2: COMPARISON OF TWO-WAY AND ONE-WAY FIXED-EFFECT MODELS
#------------------------------------------------------------------------------------
etable(reg_crop_full2,reg_crop_full4, digits = 3, se.below = T, tex = T)


#long-run solutions
model <- reg_crop_full2 #replace with the corresponding model: reg_crop_full2 or reg_crop_full4
alpha <- abs(model$coefficients[2])
coefficients <- model$coefficients
new_coefficients1 <- coefficients[3:5] / alpha
cov_matrix <- vcov(model)
var_alpha <- cov_matrix[2, 2]

new_se <- numeric(length(coefficients))

# Compute the new standard errors for coefficients 3 to 5
for (i in 3:5) {
  coef_i <- coefficients[i]
  var_coef_i <- cov_matrix[i, i]
  cov_alpha_coef_i <- cov_matrix[i, 2] 
  new_se[i] <- sqrt(
    (1 / alpha)^2 * var_coef_i +
      (coef_i / (alpha^2))^2 * var_alpha +
      2 * (1 / -alpha) * (coef_i / (alpha^2)) * cov_alpha_coef_i
  )
}
new_coefficients1 
new_se[3:5]

#====================================================================================
# LOCAL PROJECTIONS
#====================================================================================

reg_crop_full4 <- feols(
  DlogQ ~ l(DlogQ,1) + l(log(Q),1) + l(log(Labor),1) +
    l(log(Capital),1) + l(log(Land),1) + global_temp_dev +
    I(temp_crop_anom_pos - l(temp_crop_anom_pos,1)) +
    I(temp_crop_anom_neg - l(temp_crop_anom_neg,1)) +
    I(log(Fert_ratio) - l(log(Fert_ratio),1)) +
    I(log(Capital) - l(log(Capital),1)) +
    I(log(Labor) - l(log(Labor),1)) +
    I(log(Land) - l(log(Land),1)) +
    drought_crop + trend | a3,
  data_filtered,
  panel.id = ~ a3 + Year,
  DK ~ a3
)  # One-way FE

data_filtered <- data_filtered %>%
  mutate(
    logQ = log(Q),
    logFert = log(Fertilizer),
    logLab = log(Labor),
    logCap = log(Capital),
    logLand = log(Land)
  ) %>%
  group_by(a3) %>%
  mutate(
    Dtempanom_pos = temp_crop_anom_pos - dplyr::lag(temp_crop_anom_pos, 1),
    Dtempanom_neg = temp_crop_anom_neg - dplyr::lag(temp_crop_anom_neg, 1),
    L1.logQ  = dplyr::lag(log(Q), 1),
    L1.DlogQ = dplyr::lag(DlogQ, 1),
    L1.logFert = dplyr::lag(logFert, 1),
    L1.logLab  = dplyr::lag(logLab, 1),
    L1.logCap  = dplyr::lag(logCap, 1),
    L1.logLand = dplyr::lag(logLand, 1)
  ) %>%
  mutate(
    DlogFert = logFert - L1.logFert,
    DlogLab  = logLab - L1.logLab,
    DlogCap  = logCap - L1.logCap,
    DlogLand = logLand - logLand   # note: same variable (unchanged)
  ) %>%
  ungroup()

#------------------------------------------------------------------------------------
# Local Projection: Response to GLOBAL temperature anomalies
#------------------------------------------------------------------------------------
shock_global <- lp_lin_panel(
  data_set = data_filtered,
  data_sample = "Full",
  endog_data = "logQ",
  cumul_mult = T,
  shock = "global_temp_dev",
  diff_shock = FALSE,
  panel_model = "within",
  panel_effect = "individual",
  robust_cov = "vcovSCC",
  c_exog_data = c("L1.DlogQ","L1.logQ","L1.logLab","L1.logCap","L1.logLand","DlogFert","DlogLab","DlogCap","DlogLand","drought_crop","Dtempanom_pos","Dtempanom_neg","trend"),
  confint = 1.65,
  hor = 8
)

# Scale impulse responses as percentage points
mean_global <- mean(data_filtered$global_temp_dev, na.rm = TRUE)
mean_local  <- mean(data_filtered$Dtempanom_pos, na.rm = TRUE)

# Convert IRF output to a cumulative response (%)
cshock <- rbind(
  cumsum(shock_global$irf_panel_mean * 100),
  cumsum(shock_global$irf_panel_low * 100),
  cumsum(shock_global$irf_panel_up * 100)
)
cshock <- t(cshock) %>%
  as.data.frame() %>%
  mutate(Time = seq(1, 8))
colnames(cshock) <- c("mean", "low", "up", "Time")

theme_ncc <- theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(color = "black"),
    axis.text  = element_text(color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 0.3),
    plot.margin = margin(6, 8, 6, 8)
  )
col_global <- "#0072B2"

# Main IRF plot
p1 <- ggplot(cshock, aes(x = Time)) +
  geom_ribbon(aes(ymin = low, ymax = up),
              fill = col_global, alpha = 0.25) +
  geom_line(aes(y = mean),
            color = col_global, linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey30") +
  scale_y_continuous(name = expression(log("Food Production") %*% 100)) +
  scale_x_continuous(name = "Horizon (years ahead)", breaks = 1:8) +
  theme_ncc +
  theme(plot.margin = margin(6, 8, 0, 8))

# Histogram of global temperature anomalies
p2 <- ggplot(global_tempanom %>% filter(year > 1960),
             aes(x = global_temp_dev)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 30,
    fill = col_global,
    color = "grey20",
    alpha = 0.6,
    linewidth = 0.3
  ) +
  scale_x_continuous(name = "Global temperature anomalies (°C)") +
  scale_y_continuous(name = "Density") +
  theme_ncc +
  theme(plot.margin = margin(0, 8, 6, 8))

#------------------------------------------------------------------------------------
# Local Projection: Response to LOCAL (positive) temperature anomalies
#------------------------------------------------------------------------------------
shock_local <- lp_lin_panel(
  data_set = data_filtered,
  data_sample = "Full",
  endog_data = "logQ",
  cumul_mult = FALSE,
  shock = "Dtempanom_pos",
  diff_shock = FALSE,
  panel_model = "within",
  panel_effect = "individual",
  robust_cov = "vcovSCC",
  c_exog_data = c(
    "L1.DlogQ","L1.logQ","L1.logLab","L1.logCap","L1.logLand",
    "DlogFert","DlogLab","DlogCap","DlogLand",
    "drought_crop","global_temp_dev","Dtempanom_neg","trend"
  ),
  confint = 1.645,
  hor = 8
)

# Compute cumulative response (%)
cshock <- rbind(
  cumsum(shock_local$irf_panel_mean * 100),
  cumsum(shock_local$irf_panel_low * 100),
  cumsum(shock_local$irf_panel_up * 100)
)
cshock <- t(cshock) %>%
  as.data.frame() %>%
  mutate(Time = seq(1, 8))
colnames(cshock) <- c("mean", "low", "up", "Time")

col_local <- "#D55E00"

p3 <- ggplot(cshock, aes(x = Time)) +
  geom_ribbon(aes(ymin = low, ymax = up),
              fill = col_local, alpha = 0.25) +
  geom_line(aes(y = mean),
            color = col_local, linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey30") +
  scale_y_continuous(name = expression(log("Food Production") %*% 100)) +
  scale_x_continuous(name = "Horizon (years ahead)", breaks = 1:8) +
  theme_ncc +
  theme(plot.margin = margin(6, 8, 0, 8))

# Histogram of local anomalies
p4 <- ggplot(data_filtered, aes(x = Dtempanom_pos)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 30,
    fill = col_local,
    color = "grey20",
    alpha = 0.6,
    linewidth = 0.3
  ) +
  scale_x_continuous(name = "Positive local temperature anomalies (Δ, °C)") +
  scale_y_continuous(name = "Density") +
  theme_ncc +
  theme(plot.margin = margin(0, 8, 6, 8))

p2 <- p2 + labs(subtitle = paste0("n = ", sum(global_tempanom$year > 1960)))
p4 <- p4 + labs(subtitle = paste0("n = ", sum(!is.na(data_filtered$Dtempanom_pos))))

#------------------------------------------------------------------------------------
# FIGURE 2: Combined IRFs for GLOBAL and LOCAL temperature shocks
#------------------------------------------------------------------------------------
tagA <- textGrob("A", x = 0, y = 1, just = c("left","top"),
                 gp = gpar(fontface = "bold", fontsize = 12))
tagB <- textGrob("B", x = 0, y = 1, just = c("left","top"),
                 gp = gpar(fontface = "bold", fontsize = 12))
tagC <- textGrob("C", x = 0, y = 1, just = c("left","top"),
                 gp = gpar(fontface = "bold", fontsize = 12))
tagD <- textGrob("D", x = 0, y = 1, just = c("left","top"),
                 gp = gpar(fontface = "bold", fontsize = 12))

rowA <- arrangeGrob(tagA, p1, ncol = 2, widths = unit.c(unit(0.03, "npc"), unit(0.97, "npc")))
rowC <- arrangeGrob(tagC, p2, ncol = 2, widths = unit.c(unit(0.03, "npc"), unit(0.97, "npc")))
col1 <- arrangeGrob(rowA, rowC, ncol = 1, heights = c(2.5, 1))

rowB <- arrangeGrob(tagB, p3, ncol = 2, widths = unit.c(unit(0.03, "npc"), unit(0.97, "npc")))
rowD <- arrangeGrob(tagD, p4, ncol = 2, widths = unit.c(unit(0.03, "npc"), unit(0.97, "npc")))
col2 <- arrangeGrob(rowB, rowD, ncol = 1, heights = c(2.5, 1))


cairo_ps(file.path(paths$figures, "Figure2.eps"), width = 8, height = 8, onefile = FALSE)
grid.arrange(col1, col2, ncol = 2)
dev.off()



#====================================================================================
# GRANGER CAUSALITY TESTS (PVAR FRAMEWORK)
#====================================================================================

# Prepare data for panel VAR (first differences)
data_prepared <- data_filtered %>%
  arrange(a3, Year) %>%
  group_by(a3) %>%
  mutate(
    DlogQ          = log(Q_index) - dplyr::lag(log(Q_index)),
    Dtemp          = temp_crop_anom - dplyr::lag(temp_crop_anom),
    DlogFertratio  = log(Fert_ratio) - dplyr::lag(log(Fert_ratio)),
    DlogK          = log(Capital) - dplyr::lag(log(Capital)),
    DlogL          = log(Labor) - dplyr::lag(log(Labor)),
    DlogLand       = log(Land)   - dplyr::lag(log(Land))
  ) %>%
  ungroup()

# Select variables for the panel VAR
panel_data <- data_prepared %>%
  dplyr::select(a3, Year, DlogQ, Dtemp, DlogFertratio, DlogK, DlogL, DlogLand, drought_crop) %>%
  na.omit() %>%
  dplyr::mutate(
    a3   = as.character(a3),
    Year = as.integer(Year)
  )

# Encode country identifiers numerically for PVAR estimation
panel_data$a3_int <- as.integer(factor(panel_data$a3))
panel_data <- as.data.frame(panel_data)

# Estimate PVAR with two lags and fixed effects removed by demeaning
pvar_model <- pvarfeols(
  dependent_vars = c("DlogQ", "Dtemp"),
  lags = 2,
  exog_vars = c("DlogFertratio", "DlogK", "DlogL", "DlogLand", "drought_crop"),
  transformation = "demean",
  data = panel_data,
  panel_identifier = c("a3_int", "Year")
)
summary(pvar_model)

# Export dataset used for Granger causality analysis and panel cointegration in Stata
write.csv(panel_data, file.path(paths$processed, "for_granger_causality.csv"), row.names = FALSE)

#====================================================================================
# ROBUSTNESS CHECKS
#====================================================================================

#------------------------------------------------------------------------------------
# (a) Excluding major producers: China, USA, India, and Russia
#------------------------------------------------------------------------------------
data_filtered_new <- data_filtered %>%
  filter(a3 != "CHN" & a3 != "USA" & a3 != "IND" & a3 != "RUS")

reg_crop_full2a <- feols(
  DlogQ ~ l(DlogQ,1) + l(log(Q),1) + l(log(Labor),1) +
    l(log(Capital),1) + l(log(Land),1) +
    I(temp_crop_anom_pos - l(temp_crop_anom_pos,1)) +
    I(temp_crop_anom_neg - l(temp_crop_anom_neg,1)) +
    I(log(Fertilizer) - l(log(Fertilizer),1)) +
    I(log(Capital) - l(log(Capital),1)) +
    I(log(Labor) - l(log(Labor),1)) +
    I(log(Land) - l(log(Land),1)) +
    drought_crop | a3 + Year,
  data_filtered_new,
  panel.id = ~ a3 + Year,
  DK ~ Year
)  # Two-way FE

reg_crop_full4a <- feols(
  DlogQ ~ l(DlogQ,1) + l(log(Q),1) + l(log(Labor),1) +
    l(log(Capital),1) + l(log(Land),1) + global_temp_dev +
    I(temp_crop_anom_pos - l(temp_crop_anom_pos,1)) +
    I(temp_crop_anom_neg - l(temp_crop_anom_neg,1)) +
    I(log(Fertilizer) - l(log(Fertilizer),1)) +
    I(log(Capital) - l(log(Capital),1)) +
    I(log(Labor) - l(log(Labor),1)) +
    I(log(Land) - l(log(Land),1)) +
    drought_crop + trend | a3,
  data_filtered_new,
  panel.id = ~ a3 + Year,
  DK ~ Year
)  # One-way FE

etable(reg_crop_full2a, reg_crop_full4a, digits = 3, se.below = TRUE, tex = TRUE)


#long-run solutions
model <- reg_crop_full4a #replace with the corresponding model
alpha <- abs(model$coefficients[2])
coefficients <- model$coefficients
cov_matrix <- vcov(model)
var_alpha <- cov_matrix[2, 2]

new_se <- numeric(length(coefficients))
for (i in c(3:5)) {
  coef_i <- coefficients[i]
  var_coef_i <- cov_matrix[i, i]
  cov_alpha_coef_i <- cov_matrix[i, 2] 
  new_se[i] <- sqrt(
    (1 / alpha)^2 * var_coef_i +
      (coef_i / (alpha^2))^2 * var_alpha +
      2 * (1 / -alpha) * (coef_i / (alpha^2)) * cov_alpha_coef_i
  )
}
coefficients/ alpha 
new_se


estimation_sample <- fixest_data(reg_crop_full2a, sample = "estimation")
panel_stats <- estimation_sample %>%
  group_by(a3) %>%
  summarise(T = n_distinct(Year), .groups = "drop")
average_T <- mean(panel_stats$T)
cat("Average T (years per country):", average_T, "\n")

estimation_sample <- fixest_data(reg_crop_full4a, sample = "estimation")
panel_stats <- estimation_sample %>%
  group_by(a3) %>%
  summarise(T = n_distinct(Year), .groups = "drop")
average_T <- mean(panel_stats$T)
cat("Average T (years per country):", average_T, "\n")


#------------------------------------------------------------------------------------
# (b) Filtering outliers beyond ±3 standard deviations of residuals
#------------------------------------------------------------------------------------
used_obs2 <- which(!is.na(resid(reg_crop_full2)))
used_obs4 <- which(!is.na(resid(reg_crop_full4)))

data_used2 <- data_filtered[used_obs2, ]
data_used4 <- data_filtered[used_obs4, ]

data_used2$residuals <- resid(reg_crop_full2)
data_used4$residuals <- resid(reg_crop_full4)

sd_resid2 <- sd(data_used2$residuals, na.rm = TRUE)
sd_resid4 <- sd(data_used4$residuals, na.rm = TRUE)

threshold2 <- 3 * sd_resid2
threshold4 <- 3 * sd_resid4

data_clean2 <- data_used2[abs(data_used2$residuals) <= threshold2, ]
data_clean4 <- data_used4[abs(data_used4$residuals) <= threshold4, ]

reg_crop_full2b <- feols(
  DlogQ ~ l(DlogQ,1) + l(log(Q),1) + l(log(Labor),1) +
    l(log(Capital),1) + l(log(Land),1) +
    I(temp_crop_anom_pos - l(temp_crop_anom_pos,1)) +
    I(temp_crop_anom_neg - l(temp_crop_anom_neg,1)) +
    I(log(Fertilizer) - l(log(Fertilizer),1)) +
    I(log(Capital) - l(log(Capital),1)) +
    I(log(Labor) - l(log(Labor),1)) +
    I(log(Land) - l(log(Land),1)) +
    drought_crop | a3 + Year,
  data_clean2,
  panel.id = ~ a3 + Year,
  DK ~ Year
)

reg_crop_full4b <- feols(
  DlogQ ~ l(DlogQ,1) + l(log(Q),1) + l(log(Labor),1) +
    l(log(Capital),1) + l(log(Land),1) + global_temp_dev +
    I(temp_crop_anom_pos - l(temp_crop_anom_pos,1)) +
    I(temp_crop_anom_neg - l(temp_crop_anom_neg,1)) +
    I(log(Fertilizer) - l(log(Fertilizer),1)) +
    I(log(Capital) - l(log(Capital),1)) +
    I(log(Labor) - l(log(Labor),1)) +
    I(log(Land) - l(log(Land),1)) +
    drought_crop + trend | a3,
  data_clean4,
  panel.id = ~ a3 + Year,
  DK ~ Year
)

etable(reg_crop_full2b, reg_crop_full4b, digits = 3, se.below = TRUE, tex = FALSE)


#------------------------------------------------------------------------------------
# (c) Using unweighted local temperature anomalies
#------------------------------------------------------------------------------------
data_filtered$temp_unw_anom_pos <- data_filtered$temp_unw_anom*ifelse(data_filtered$temp_unw_anom>=0,1,0)
data_filtered$temp_unw_anom_neg <- data_filtered$temp_unw_anom*ifelse(data_filtered$temp_unw_anom<0,1,0)

reg_crop_full2c <- feols(
  DlogQ ~ l(DlogQ,1) + l(log(Q),1) + l(log(Labor),1) +
    l(log(Capital),1) + l(log(Land),1) +
    I(temp_unw_anom_pos - l(temp_unw_anom_pos,1)) +
    I(temp_unw_anom_neg - l(temp_unw_anom_neg,1)) +
    I(log(Fertilizer) - l(log(Fertilizer),1)) +
    I(log(Capital) - l(log(Capital),1)) +
    I(log(Labor) - l(log(Labor),1)) +
    I(log(Land) - l(log(Land),1)) +
    drought_unw | a3 + Year,
  data_filtered,
  panel.id = ~ a3 + Year,
  DK ~ Year
)  # Two-way FE

reg_crop_full4c <- feols(
  DlogQ ~ l(DlogQ,1) + l(log(Q),1) + l(log(Labor),1) +
    l(log(Capital),1) + l(log(Land),1) + global_temp_dev +
    I(temp_unw_anom_pos - l(temp_unw_anom_pos,1)) +
    I(temp_unw_anom_neg - l(temp_unw_anom_neg,1)) +
    I(log(Fertilizer) - l(log(Fertilizer),1)) +
    I(log(Capital) - l(log(Capital),1)) +
    I(log(Labor) - l(log(Labor),1)) +
    I(log(Land) - l(log(Land),1)) +
    drought_unw + trend | a3,
  data_filtered,
  panel.id = ~ a3 + Year,
  DK ~ Year
)  # One-way FE

etable(reg_crop_full2c, reg_crop_full4c, digits = 3, se.below = TRUE, tex = FALSE)

#------------------------------------------------------------------------------------
# Compare cleaned and unweighted results (used in paper robustness table)
#------------------------------------------------------------------------------------
etable(reg_crop_full4c, reg_crop_full4b, digits = 3, se.below = TRUE, tex = TRUE)

# Compute average T (years per country) for clean model
estimation_sample <- fixest_data(reg_crop_full4b, sample = "estimation")
panel_stats <- estimation_sample %>%
  group_by(a3) %>%
  summarise(T = n_distinct(Year), .groups = "drop")
average_T <- mean(panel_stats$T)
cat("Average T (years per country):", average_T, "\n")

#====================================================================================
# INSTRUMENTAL VARIABLES (IV) ESTIMATION
#====================================================================================

#------------------------------------------------------------------------------------
# Load Effective Radiative Forcing (ERF) Data and Compute Deviations
#------------------------------------------------------------------------------------
ERF <- readxl::read_xlsx(file.path(paths$instr, "ERF.xlsx"))

#====================================================================================
# INSTRUMENTAL VARIABLES (IV) ESTIMATION
#====================================================================================

#------------------------------------------------------------------------------------
# Load Effective Radiative Forcing (ERF) Data and Compute Deviations
#------------------------------------------------------------------------------------
ERF <- readxl::read_xlsx(file.path(paths$instr, "ERF.xlsx"))

# Compute 30-year moving averages and deviations for GHGs and aerosols
ERF <- ERF %>%
  arrange(Year) %>%
  mutate(
    # Greenhouse gases (GHG)
    ERF_GHG_ma_30 = slide_dbl(WMGHG, mean, .before = 30, .complete = TRUE),
    ERF_GHG_ma_30 = lag(ERF_GHG_ma_30),
    ERF_GHG_dev   = WMGHG - ERF_GHG_ma_30
  ) %>%
  arrange(Year) %>%
  mutate(
    # Aerosols
    ERF_Aero_ma_30 = slide_dbl(Aerosols, mean, .before = 30, .complete = TRUE),
    ERF_Aero_ma_30 = lag(ERF_Aero_ma_30),
    ERF_Aero_dev   = Aerosols - ERF_Aero_ma_30
  )

# Keep observations from 1960–2023
ERF <- ERF %>% filter(Year >= 1960 & Year <= 2023)

# Merge with main dataset
data_filtered <- left_join(data_filtered, ERF, by = c("Year" = "Year"))

#====================================================================================
# SPATIAL INSTRUMENTS: TEMPERATURE IN NEIGHBORING COUNTRIES
#====================================================================================

# Read and prepare world shapefile
world_sf <- st_read("instruments/world-administrative-boundaries.geojson")
world_sf <- world_sf %>%
  rename(a3 = iso3) %>%
  st_make_valid() %>%
  dplyr::select(a3, geometry)

# Compute pairwise geographic distances (in meters)
dist_matrix <- st_distance(world_sf)

# Identify 5 nearest neighbors for each country (excluding itself)
neighbors_k5 <- lapply(seq_len(nrow(dist_matrix)), function(i) {
  order(dist_matrix[i, ])[2:6]
})

# Build tibble of neighbors
neighbor_df <- tibble(
  a3 = world_sf$a3,
  neighbors = map(neighbors_k5, ~ world_sf$a3[.x])
)

# Compute average temperature anomaly among neighboring countries
avg_neighbors_by_year <- data_filtered %>%
  dplyr::select(a3, Year, temp_crop_anom) %>%
  right_join(neighbor_df, by = "a3") %>%
  unnest(neighbors) %>%
  rename(neighbor_a3 = neighbors) %>%
  left_join(
    data_filtered %>%
      dplyr::select(neighbor_a3 = a3, Year, neighbor_anom = temp_crop_anom),
    by = c("neighbor_a3", "Year")
  ) %>%
  group_by(a3, Year) %>%
  summarise(avg_temp_neighbors = mean(neighbor_anom, na.rm = TRUE), .groups = "drop")

# Merge neighbor averages into main dataset
data_filtered <- data_filtered %>%
  left_join(avg_neighbors_by_year, by = c("a3", "Year")) %>%
  mutate(
    avg_temp_neighbors_pos = avg_temp_neighbors * ifelse(avg_temp_neighbors > 0, 1, 0),
    avg_temp_neighbors_neg = avg_temp_neighbors * ifelse(avg_temp_neighbors < 0, 1, 0)
  )

#====================================================================================
# IV ESTIMATION: GLOBAL AND LOCAL TEMPERATURE SHOCKS
#====================================================================================

# (1) One-way FE with local temperature anomalies instrumented by neighbors
iv2 <- feols(DlogQ~l(DlogQ,1)+l(log(Q),1)+l(log(Labor),1)+l(log(Capital),1)+l(log(Land),1)+I(log(Fert_ratio)-l(log(Fert_ratio),1))+I(log(Capital)-l(log(Capital),1))+I(log(Labor)-l(log(Labor),1))+I(log(Land)-l(log(Land),1)) + drought_crop | a3 + Year | I(temp_crop_anom_pos-l(temp_crop_anom_pos,1))+I(temp_crop_anom_neg-l(temp_crop_anom_neg,1)) ~   I(avg_temp_neighbors_pos-l(avg_temp_neighbors_pos,1) ) + I(avg_temp_neighbors_neg-l(avg_temp_neighbors_neg,1)) , panel.id = ~ a3 + Year, DK ~ Year, data = data_filtered) #one-way FE
summary(iv2, stage = 1)
summary(iv2)

# (2) One-way FE with global + local anomalies instrumented by ERF and spatial lags
iv4 <- feols(DlogQ~l(DlogQ,1)+l(log(Q),1)+l(log(Labor),1)+l(log(Capital),1)+l(log(Land),1)+I(log(Fert_ratio)-l(log(Fert_ratio),1))+I(log(Capital)-l(log(Capital),1))+I(log(Labor)-l(log(Labor),1))+I(log(Land)-l(log(Land),1)) + drought_crop + trend| a3 | global_temp_dev + I(temp_crop_anom_pos-l(temp_crop_anom_pos,1))+I(temp_crop_anom_neg-l(temp_crop_anom_neg,1)) ~   l(ERF_GHG_dev,1) +l(ERF_Aero_dev) + I(avg_temp_neighbors_pos-l(avg_temp_neighbors_pos,1) ) + I(avg_temp_neighbors_neg-l(avg_temp_neighbors_neg,1)) , panel.id = ~ a3 + Year, DK ~ Year, data = data_filtered) #one-way FE
summary(iv4, stage = 1)
summary(iv4)

etable(iv2,iv4, digits = 3, se.below = T, tex = T)


#long-run solutions
model <- iv4 #replace with the corresponding model
alpha <- abs(model$coefficients[5]) #change to 4 for model 2
coefficients <- model$coefficients
cov_matrix <- vcov(model)
var_alpha <- cov_matrix[2, 2]


new_se <- numeric(length(coefficients))
for (i in c(6,7,8)) { # change to (5,6,7) for model 2 and (6,7,8) for model 4
  coef_i <- coefficients[i]
  var_coef_i <- cov_matrix[i, i]
  cov_alpha_coef_i <- cov_matrix[i, 2] 
  new_se[i] <- sqrt(
    (1 / alpha)^2 * var_coef_i +
      (coef_i / (alpha^2))^2 * var_alpha +
      2 * (1 / -alpha) * (coef_i / (alpha^2)) * cov_alpha_coef_i
  )
}
coefficients/ alpha
new_se


new_se <- numeric(length(coefficients))
for (i in c(1,14)) {
  coef_i <- coefficients[i]
  var_coef_i <- cov_matrix[i, i]
  cov_alpha_coef_i <- cov_matrix[i, 2] 
  new_se[i] <- sqrt(
    (1 / alpha)^2 * var_coef_i +
      (coef_i / (alpha^2))^2 * var_alpha +
      2 * (1 / -alpha) * (coef_i / (alpha^2)) * cov_alpha_coef_i
  )
}


estimation_sample <- fixest_data(iv2, sample = "estimation")
panel_stats <- estimation_sample %>%
  group_by(a3) %>%
  summarise(T = n_distinct(Year), .groups = "drop")
average_T <- mean(panel_stats$T)
cat("Average T (years per country):", average_T, "\n")


#====================================================================================
# INTERACTIONS WITH TECHNOLOGICAL CLUSTERS
#====================================================================================

cluster_all <- read.csv(file.path(paths$clusters, "clusters_final.csv"))
cluster_all$cluster_irrig_low <- ifelse(cluster_all$irrig=="low",1,0) 
cluster_all$cluster_irrig_med <- ifelse(cluster_all$irrig=="med",1,0) 
cluster_all$cluster_irrig_high <- ifelse(cluster_all$irrig=="high",1,0) 
cluster_all$cluster_fert_low <- ifelse(cluster_all$fert=="low",1,0) 
cluster_all$cluster_fert_med <- ifelse(cluster_all$fert=="med",1,0) 
cluster_all$cluster_fert_high <- ifelse(cluster_all$fert=="high",1,0) 
cluster_all$cluster_cap_low <- ifelse(cluster_all$cap=="low",1,0) 
cluster_all$cluster_cap_med <- ifelse(cluster_all$cap=="med",1,0) 
cluster_all$cluster_cap_high <- ifelse(cluster_all$cap=="high",1,0) 

data_filtered <- left_join(data_filtered,cluster_all, by = c("a3"="a3"))

# One-way FE with interactions by technological cluster
reg_crop_full5_interact_irrig <- feols(DlogQ~l(DlogQ,1)+l(log(Q),1)+l(log(Labor),1)+l(log(Capital),1)+l(log(Land),1)+as.numeric(global_temp_dev):cluster_irrig_low+as.numeric(global_temp_dev):cluster_irrig_med+as.numeric(global_temp_dev):cluster_irrig_high+I(temp_crop_anom_pos-l(temp_crop_anom_pos,1)):cluster_irrig_low+I(temp_crop_anom_pos-l(temp_crop_anom_pos,1)):cluster_irrig_med+I(temp_crop_anom_pos-l(temp_crop_anom_pos,1)):cluster_irrig_high+I(temp_crop_anom_neg-l(temp_crop_anom_neg,1))+I(log(Fert_ratio)-l(log(Fert_ratio),1))+I(log(Capital)-l(log(Capital),1))+I(log(Labor)-l(log(Labor),1))+I(log(Land)-l(log(Land),1)) + drought_crop+ trend | a3, data_filtered, panel.id = ~ a3 + Year, DK ~ Year) #one-way FE
reg_crop_full5_interact_fert <- feols(DlogQ~l(DlogQ,1)+l(log(Q),1)+l(log(Labor),1)+l(log(Capital),1)+l(log(Land),1)+as.numeric(global_temp_dev):cluster_fert_low+as.numeric(global_temp_dev):cluster_fert_med+as.numeric(global_temp_dev):cluster_fert_high+I(temp_crop_anom_pos-l(temp_crop_anom_pos,1)):cluster_fert_low+I(temp_crop_anom_pos-l(temp_crop_anom_pos,1)):cluster_fert_med+I(temp_crop_anom_pos-l(temp_crop_anom_pos,1)):cluster_fert_high+I(temp_crop_anom_neg-l(temp_crop_anom_neg,1))+I(log(Fert_ratio)-l(log(Fert_ratio),1))+I(log(Capital)-l(log(Capital),1))+I(log(Labor)-l(log(Labor),1))+I(log(Land)-l(log(Land),1)) + drought_crop+ trend | a3, data_filtered, panel.id = ~ a3 + Year, DK ~ Year) #one-way FE
reg_crop_full5_interact_cap <- feols(DlogQ~l(DlogQ,1)+l(log(Q),1)+l(log(Labor),1)+l(log(Capital),1)+l(log(Land),1)+as.numeric(global_temp_dev):cluster_cap_low+as.numeric(global_temp_dev):cluster_cap_med+as.numeric(global_temp_dev):cluster_cap_high+I(temp_crop_anom_pos-l(temp_crop_anom_pos,1)):cluster_cap_low+I(temp_crop_anom_pos-l(temp_crop_anom_pos,1)):cluster_cap_med+I(temp_crop_anom_pos-l(temp_crop_anom_pos,1)):cluster_cap_high+I(temp_crop_anom_neg-l(temp_crop_anom_neg,1))+I(log(Fert_ratio)-l(log(Fert_ratio),1))+I(log(Capital)-l(log(Capital),1))+I(log(Labor)-l(log(Labor),1))+I(log(Land)-l(log(Land),1)) + drought_crop+ trend | a3, data_filtered, panel.id = ~ a3 + Year, DK ~ Year) #one-way FE

etable(
  reg_crop_full5_interact_irrig,
  reg_crop_full5_interact_fert,
  reg_crop_full5_interact_cap,
  se = "twoway",
  tex = TRUE
)


#---------------------------
# Helper: find X:Y term with simple 'contains'
#---------------------------

find_term_simple <- function(model, left_token, right_token) {
  nm <- names(coef(model))
  idx <- which(grepl(left_token, nm, fixed = TRUE) & grepl(right_token, nm, fixed = TRUE))
  idx
}

extract_ci_one <- function(model, left_token, right_token, cluster_lab, indicator_lab) {
  idx <- find_term_simple(model, left_token, right_token)
  if (length(idx) != 1) {
    # Helpful warning to debug unmatched terms
    message(sprintf("[WARN] Could not find term with '%s' & '%s' in %s.",
                    left_token, right_token, indicator_lab))
    return(tibble(
      indicator = indicator_lab, cluster = cluster_lab,
      estimate = NA_real_, lower = NA_real_, upper = NA_real_
    ))
  }
  cf  <- coef(model)[idx]
  V   <- vcov(model)
  se  <- sqrt(V[idx, idx])
  tibble(
    indicator = indicator_lab, cluster = cluster_lab,
    estimate = cf, lower = cf - 1.96 * se, upper = cf + 1.96 * se
  )
}

extract_ci_three <- function(model, left_token, cluster_prefix, indicator_lab) {
  bind_rows(
    extract_ci_one(model, left_token, paste0(cluster_prefix, "_low"),  "low",  indicator_lab),
    extract_ci_one(model, left_token, paste0(cluster_prefix, "_med"),  "med",  indicator_lab),
    extract_ci_one(model, left_token, paste0(cluster_prefix, "_high"), "high", indicator_lab)
  )
}


#========================
# Tokens to use
#========================
# In fixest, 'as.numeric(global_temp_dev)' is typically labeled as 'global_temp_dev'
term_global <- "global_temp_dev"

# This one already worked; keep as is.
term_local  <- "I(temp_crop_anom_pos - l(temp_crop_anom_pos, 1))"


#========================
# Extract CIs (global and local)
#========================

theme_nature <- function(base_size = 12, base_family = "sans") {
  theme_classic(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.title.position = "plot",
      plot.title = element_text(face = "bold", size = base_size + 2, hjust = 0),
      plot.subtitle = element_text(size = base_size, margin = margin(b = 6)),
      axis.title.x = element_text(margin = margin(t = 6)),
      axis.title.y = element_text(margin = margin(r = 8)),
      axis.text = element_text(color = "black"),
      axis.line = element_line(linewidth = 0.4, color = "black"),
      axis.ticks = element_line(linewidth = 0.35, color = "black"),
      axis.ticks.length = unit(2.2, "pt"),
      
      # remove grid (classic); keep a very light horizontal reference if you want:
      panel.grid = element_blank(),
      
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = base_size - 1),
      legend.key.size = unit(10, "pt"),
      legend.spacing.x = unit(10, "pt"),
      plot.margin = margin(8, 10, 6, 10)
    )
}
pal_ind <- c(
  "Irrigation" = "#0072B2",  # blue
  "Fertilizer" = "#009E73",  # green
  "Capital"    = "#D55E00"   # vermillion
)
m_irrig <- reg_crop_full5_interact_irrig
m_fert  <- reg_crop_full5_interact_fert
m_cap   <- reg_crop_full5_interact_cap

df_global <- bind_rows(
  extract_ci_three(m_irrig, term_global, "cluster_irrig", "Irrigation"),
  extract_ci_three(m_fert,  term_global, "cluster_fert",  "Fertilizer"),
  extract_ci_three(m_cap,   term_global, "cluster_cap",   "Capital")
)

df_local <- bind_rows(
  extract_ci_three(m_irrig, term_local, "cluster_irrig", "Irrigation"),
  extract_ci_three(m_fert,  term_local, "cluster_fert",  "Fertilizer"),
  extract_ci_three(m_cap,   term_local, "cluster_cap",   "Capital")
)

# Express in percentage points (%)
df_global <- df_global %>% mutate(across(c(estimate, lower, upper), ~ .x * 100))
df_local  <- df_local  %>% mutate(across(c(estimate, lower, upper), ~ .x * 100))


df_global <- df_global %>%
  mutate(
    cluster = factor(cluster, levels = c("high","med","low"),
                     labels = c("High", "Medium", "Low")),
    indicator = factor(indicator, levels = c("Irrigation","Fertilizer","Capital"))
  )
df_local <- df_local %>%
  mutate(
    cluster = factor(cluster, levels = c("high","med","low"),
                     labels = c("High", "Medium", "Low")),
    indicator = factor(indicator, levels = c("Irrigation","Fertilizer","Capital"))
  )



#========================
# Plots
#========================

ylim_common <- range(
  c(df_global$lower, df_global$upper, df_local$lower, df_local$upper),
  na.rm = TRUE
)

pd <- position_dodge(width = 0.55)

axis_y_style <- theme(
  axis.title.y = element_text(
    angle = 90,
    vjust = 0.5,
    margin = margin(r = 10)
  )
)

ylab_txt <- "Effect on food production growth (pp)"

p_global <- ggplot(df_global,
                   aes(x = cluster, y = estimate,
                       ymin = lower, ymax = upper, color = indicator)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             linewidth = 0.35, color = "grey55") +
  geom_pointrange(position = pd, linewidth = 0.55, fatten = 2.1) +
  scale_color_manual(values = pal_ind) +
  coord_cartesian(ylim = ylim_common, expand = FALSE) +
  scale_y_continuous(breaks = pretty(ylim_common, 5)) +
  labs(
    x = NULL,
    y = ylab_txt,
    title = "Global temperature anomaly"
  ) +
  theme_nature(base_size = 12) +
  axis_y_style +
  theme(plot.margin = margin(8, 10, 6, 10))

p_local <- ggplot(df_local,
                  aes(x = cluster, y = estimate,
                      ymin = lower, ymax = upper, color = indicator)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             linewidth = 0.35, color = "grey55") +
  geom_pointrange(position = pd, linewidth = 0.55, fatten = 2.1) +
  scale_color_manual(values = pal_ind) +
  coord_cartesian(ylim = ylim_common, expand = FALSE) +
  scale_y_continuous(breaks = pretty(ylim_common, 5)) +
  labs(
    x = NULL,
    y = ylab_txt,   # ← SE REPITE
    title = "Local positive temperature anomaly"
  ) +
  theme_nature(base_size = 12) +
  axis_y_style +
  theme(plot.margin = margin(8, 10, 6, 10))


p_global <- p_global + theme(plot.margin = margin(8, 10, 6, 10))
p_local  <- p_local  + theme(plot.margin = margin(8, 10, 6, 28))

leg <- get_legend(
  p_global +
    theme(legend.position = "bottom") +
    guides(color = guide_legend(nrow = 1))
)

combined <- plot_grid(
  p_global + theme(legend.position = "none"),
  p_local  + theme(legend.position = "none"),
  ncol = 2, align = "hv", axis = "tb",
  labels = c("A", "B"),
  label_fontface = "bold",
  label_size = 13,
  label_x = 0.02, label_y = 0.98,
  hjust = 0, vjust = 1
)

final_fig3 <- plot_grid(combined, leg, ncol = 1, rel_heights = c(1, 0.06))


cairo_ps(file.path(paths$figures, "Figure3.eps"), width = 8, height = 4, onefile = FALSE)
final_fig3
dev.off()


#====================================================================================
# INTERACTIONS WITH CLUSTERS (HIERARCHICAL CLUSTERING)
#====================================================================================

cluster_all <- read.csv("clusters/clusters_final_hierarchical.csv")
cluster_all$cluster_irrig_low <- ifelse(cluster_all$irrig=="low",1,0) 
cluster_all$cluster_irrig_med <- ifelse(cluster_all$irrig=="med",1,0) 
cluster_all$cluster_irrig_high <- ifelse(cluster_all$irrig=="high",1,0) 
cluster_all$cluster_fert_low <- ifelse(cluster_all$fert=="low",1,0) 
cluster_all$cluster_fert_med <- ifelse(cluster_all$fert=="med",1,0) 
cluster_all$cluster_fert_high <- ifelse(cluster_all$fert=="high",1,0) 
cluster_all$cluster_cap_low <- ifelse(cluster_all$cap=="low",1,0) 
cluster_all$cluster_cap_med <- ifelse(cluster_all$cap=="med",1,0) 
cluster_all$cluster_cap_high <- ifelse(cluster_all$cap=="high",1,0) 

data_filtered <- data_filtered %>%
  select(-starts_with("cluster_"))
data_filtered <- left_join(data_filtered,cluster_all, by = c("a3"="a3"))


reg_crop_full5_interact_irrig <- feols(DlogQ~l(DlogQ,1)+l(log(Q),1)+l(log(Labor),1)+l(log(Capital),1)+l(log(Land),1)+as.numeric(global_temp_dev):cluster_irrig_low+as.numeric(global_temp_dev):cluster_irrig_med+as.numeric(global_temp_dev):cluster_irrig_high+I(temp_crop_anom_pos-l(temp_crop_anom_pos,1)):cluster_irrig_low+I(temp_crop_anom_pos-l(temp_crop_anom_pos,1)):cluster_irrig_med+I(temp_crop_anom_pos-l(temp_crop_anom_pos,1)):cluster_irrig_high+I(temp_crop_anom_neg-l(temp_crop_anom_neg,1))+I(log(Fert_ratio)-l(log(Fert_ratio),1))+I(log(Capital)-l(log(Capital),1))+I(log(Labor)-l(log(Labor),1))+I(log(Land)-l(log(Land),1)) + drought_crop+ trend | a3, data_filtered, panel.id = ~ a3 + Year, DK ~ Year) #one-way FE
reg_crop_full5_interact_fert <- feols(DlogQ~l(DlogQ,1)+l(log(Q),1)+l(log(Labor),1)+l(log(Capital),1)+l(log(Land),1)+as.numeric(global_temp_dev):cluster_fert_low+as.numeric(global_temp_dev):cluster_fert_med+as.numeric(global_temp_dev):cluster_fert_high+I(temp_crop_anom_pos-l(temp_crop_anom_pos,1)):cluster_fert_low+I(temp_crop_anom_pos-l(temp_crop_anom_pos,1)):cluster_fert_med+I(temp_crop_anom_pos-l(temp_crop_anom_pos,1)):cluster_fert_high+I(temp_crop_anom_neg-l(temp_crop_anom_neg,1))+I(log(Fert_ratio)-l(log(Fert_ratio),1))+I(log(Capital)-l(log(Capital),1))+I(log(Labor)-l(log(Labor),1))+I(log(Land)-l(log(Land),1)) + drought_crop+ trend | a3, data_filtered, panel.id = ~ a3 + Year, DK ~ Year) #one-way FE
reg_crop_full5_interact_cap <- feols(DlogQ~l(DlogQ,1)+l(log(Q),1)+l(log(Labor),1)+l(log(Capital),1)+l(log(Land),1)+as.numeric(global_temp_dev):cluster_cap_low+as.numeric(global_temp_dev):cluster_cap_med+as.numeric(global_temp_dev):cluster_cap_high+I(temp_crop_anom_pos-l(temp_crop_anom_pos,1)):cluster_cap_low+I(temp_crop_anom_pos-l(temp_crop_anom_pos,1)):cluster_cap_med+I(temp_crop_anom_pos-l(temp_crop_anom_pos,1)):cluster_cap_high+I(temp_crop_anom_neg-l(temp_crop_anom_neg,1))+I(log(Fert_ratio)-l(log(Fert_ratio),1))+I(log(Capital)-l(log(Capital),1))+I(log(Labor)-l(log(Labor),1))+I(log(Land)-l(log(Land),1)) + drought_crop+ trend | a3, data_filtered, panel.id = ~ a3 + Year, DK ~ Year) #one-way FE

etable(
  reg_crop_full5_interact_irrig,
  reg_crop_full5_interact_fert,
  reg_crop_full5_interact_cap,
  se = "twoway",
  tex = TRUE
)

#====================================================================================
# LOW-TECH CLUSTER: LOCAL PROJECTIONS (APPENDIX FIGURES)
#====================================================================================

#------------------------------------------------------------------------------------
# GLOBAL SHOCK • Low-tech by irrigation
#------------------------------------------------------------------------------------
df_bajo_irrig <- data_filtered %>%
  filter(cluster_irrig_low == 1) %>%
  ungroup() %>%                     
  mutate(a3 = as.character(a3),     
         Year = as.integer(Year))

lp_bajo_global_irrig <- lp_lin_panel(
  data_set = df_bajo_irrig,
  data_sample = "Full",
  endog_data = "logQ",
  cumul_mult = TRUE,
  shock = "global_temp_dev",
  diff_shock = FALSE,
  panel_model = "within",
  panel_effect = "individual",
  robust_cov = "vcovSCC",
  c_exog_data = c("L1.DlogQ","L1.logQ","L1.logLab","L1.logCap","L1.logLand",
                  "DlogFert","DlogLab","DlogCap","DlogLand","drought_crop",
                  "Dtempanom_pos","Dtempanom_neg","trend"),
  confint = 1.645,
  hor = 10
)

#------------------------------------------------------------------------------------
# GLOBAL SHOCK • Low-tech by fertilizer
#------------------------------------------------------------------------------------
df_bajo_fert <- data_filtered %>%
  filter(cluster_fert_low == 1) %>%
  ungroup() %>%                     # quitar agrupación heredada
  mutate(a3 = as.character(a3),     # asegurarse de que sea string
         Year = as.integer(Year))

lp_bajo_global_fert <- lp_lin_panel(
  data_set = df_bajo_fert,
  data_sample = "Full",
  endog_data = "logQ",
  cumul_mult = TRUE,
  shock = "global_temp_dev",
  diff_shock = FALSE,
  panel_model = "within",
  panel_effect = "individual",
  robust_cov = "vcovSCC",
  c_exog_data = c("L1.DlogQ","L1.logQ","L1.logLab","L1.logCap","L1.logLand",
                  "DlogFert","DlogLab","DlogCap","DlogLand","drought_crop",
                  "Dtempanom_pos","Dtempanom_neg","trend"),
  confint = 1.645,
  hor = 10
)


#------------------------------------------------------------------------------------
# GLOBAL SHOCK • Low-tech by capital
#------------------------------------------------------------------------------------
df_bajo_cap <- data_filtered %>%
  filter(cluster_cap_low == 1) %>%
  ungroup() %>%                     # quitar agrupación heredada
  mutate(a3 = as.character(a3),     # asegurarse de que sea string
         Year = as.integer(Year))

lp_bajo_global_cap <- lp_lin_panel(
  data_set = df_bajo_cap,
  data_sample = "Full",
  endog_data = "logQ",
  cumul_mult = TRUE,
  shock = "global_temp_dev",
  diff_shock = FALSE,
  panel_model = "within",
  panel_effect = "individual",
  robust_cov = "vcovSCC",
  c_exog_data = c("L1.DlogQ","L1.logQ","L1.logLab","L1.logCap","L1.logLand",
                  "DlogFert","DlogLab","DlogCap","DlogLand","drought_crop",
                  "Dtempanom_pos","Dtempanom_neg","trend"),
  confint = 1.645,
  hor = 10
)

#------------------------------------------------------------------------------------
# LOCAL SHOCK • Low-tech by irrigation
#------------------------------------------------------------------------------------
lp_bajo_local_irrig <- lp_lin_panel(
  data_set = df_bajo_irrig,
  data_sample = "Full",
  endog_data = "logQ",
  cumul_mult = TRUE,
  shock = "Dtempanom_pos",
  diff_shock = FALSE,
  panel_model = "within",
  panel_effect = "individual",
  robust_cov = "vcovSCC",
  c_exog_data = c("L1.DlogQ","L1.logQ","L1.logLab","L1.logCap","L1.logLand",
                  "DlogFert","DlogLab","DlogCap","DlogLand","drought_crop",
                  "global_temp_dev","Dtempanom_neg","trend"),
  confint = 1.645,
  hor = 10
)

#------------------------------------------------------------------------------------
# LOCAL SHOCK • Low-tech by fertilizer
#------------------------------------------------------------------------------------
lp_bajo_local_fert <- lp_lin_panel(
  data_set = df_bajo_fert,
  data_sample = "Full",
  endog_data = "logQ",
  cumul_mult = TRUE,
  shock = "Dtempanom_pos",
  diff_shock = FALSE,
  panel_model = "within",
  panel_effect = "individual",
  robust_cov = "vcovSCC",
  c_exog_data = c("L1.DlogQ","L1.logQ","L1.logLab","L1.logCap","L1.logLand",
                  "DlogFert","DlogLab","DlogCap","DlogLand","drought_crop",
                  "global_temp_dev","Dtempanom_neg","trend"),
  confint = 1.645,
  hor = 10
)

#------------------------------------------------------------------------------------
# LOCAL SHOCK • Low-tech by capital
#------------------------------------------------------------------------------------
lp_bajo_local_cap <- lp_lin_panel(
  data_set = df_bajo_cap,
  data_sample = "Full",
  endog_data = "logQ",
  cumul_mult = TRUE,
  shock = "Dtempanom_pos",
  diff_shock = FALSE,
  panel_model = "within",
  panel_effect = "individual",
  robust_cov = "vcovSCC",
  c_exog_data = c("L1.DlogQ","L1.logQ","L1.logLab","L1.logCap","L1.logLand",
                  "DlogFert","DlogLab","DlogCap","DlogLand","drought_crop",
                  "global_temp_dev","Dtempanom_neg","trend"),
  confint = 1.645,
  hor = 10
)


irf_to_df <- function(lp_obj) {
  mu  <- as.numeric(lp_obj$irf_panel_mean)
  lo  <- lp_obj$irf_panel_low
  hi  <- lp_obj$irf_panel_up
  
  if (is.null(lo)) lo <- lp_obj$irf_panel_lo
  if (is.null(hi)) hi <- lp_obj$irf_panel_hi
  
  if (is.null(lo) || is.null(hi)) {
    se <- NULL
    if (!is.null(lp_obj$irf_panel_se)) se <- as.numeric(lp_obj$irf_panel_se)
    if (is.null(se) && !is.null(lp_obj$irf_panel_sd)) se <- as.numeric(lp_obj$irf_panel_sd)
    
    crit <- if (!is.null(lp_obj$confint)) as.numeric(lp_obj$confint) else 1.645
    
    if (!is.null(se)) {
      lo <- mu - crit * se
      hi <- mu + crit * se
    } else {
      lo <- rep(NA_real_, length(mu))
      hi <- rep(NA_real_, length(mu))
    }
  }
  
  H <- length(mu)
  data.frame(
    Time = seq_len(H),
    mean = cumsum(mu) * 100,
    low  = cumsum(as.numeric(lo)) * 100,
    up   = cumsum(as.numeric(hi)) * 100
  )
}

# Single-panel IRF plot with consistent styling
my_cols <- c(
  "Irrigation" = "#1F78B4",
  "Fertilizer" = "#33A02C",
  "Capital"    = "#FF7F00"
)

plot_irf <- function(df, subtitle, cluster) {
  if (!cluster %in% names(my_cols)) {
    stop("cluster debe ser uno de: ", paste(names(my_cols), collapse = ", "))
  }
  col_scalar <- unname(my_cols[[cluster]])  # asegura longitud 1
  
  ggplot(df, aes(x = Time)) +
    geom_ribbon(aes(ymin = low, ymax = up),
                fill = scales::alpha(col_scalar, 0.3)) +
    geom_line(aes(y = mean), color = col_scalar, linewidth = 1.1) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
    scale_y_continuous(name = expression(log("Food production") ~ "\u00D7" ~ 100)) +
    scale_x_continuous(name = "Horizon", breaks = pretty(df$Time)) +
    labs(subtitle = subtitle) +
    theme_minimal(base_size = 13) +
    theme(
      axis.title.y.left = element_text(color = "black"),
      axis.text = element_text(color = "black"),
      plot.subtitle = element_text(face = "bold", margin = margin(b = 5)),
      panel.grid = element_blank(),
      plot.margin = margin(8, 8, 8, 8),
      axis.title.x = element_text(size = 11, face = "bold"),  
      axis.title.y = element_text(size = 11, face = "bold"),  
      axis.text.x  = element_text(size = 12, color = "black"), 
      axis.text.y  = element_text(size = 12, color = "black")
    )
}

#------------------------------------------------------------------------------------
# BUILD IRF DATA FOR EACH LOW-TECH GROUP
#------------------------------------------------------------------------------------
df_g_irrig <- irf_to_df(lp_bajo_global_irrig)
df_g_fert  <- irf_to_df(lp_bajo_global_fert)
df_g_cap   <- irf_to_df(lp_bajo_global_cap)

df_l_irrig <- irf_to_df(lp_bajo_local_irrig)
df_l_fert  <- irf_to_df(lp_bajo_local_fert)
df_l_cap   <- irf_to_df(lp_bajo_local_cap)

#------------------------------------------------------------------------------------
# PLOTS: Global vs Local shocks (low-tech groups)
#------------------------------------------------------------------------------------
p_g_irrig <- plot_irf(df_g_irrig, "Global shock • Low-tech by irrigation", "Irrigation")
p_l_irrig <- plot_irf(df_l_irrig, "Local shock • Low-tech by irrigation",  "Irrigation")

p_g_fert  <- plot_irf(df_g_fert,  "Global shock • Low-tech by fertilizer", "Fertilizer")
p_l_fert  <- plot_irf(df_l_fert,  "Local shock • Low-tech by fertilizer",  "Fertilizer")

p_g_cap   <- plot_irf(df_g_cap,   "Global shock • Low-tech by capital",    "Capital")
p_l_cap   <- plot_irf(df_l_cap,   "Local shock • Low-tech by capital",     "Capital")

#------------------------------------------------------------------------------------
# APPENDIX FIGURE (3x2 grid): Global (left) and Local (right) by indicator
#------------------------------------------------------------------------------------
grid_3x2 <- plot_grid(
  p_g_irrig, p_l_irrig,
  p_g_fert,  p_l_fert,
  p_g_cap,   p_l_cap,
  ncol = 2, align = "hv", axis = "tblr",
#  labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
  label_size = 12, label_fontface = "bold"
)

final_plot <- plot_grid(
  grid_3x2,
  ncol = 1
)

#------------------------------------------------------------------------------------
# FIGURE A1: Low-tech clusters’ cumulative IRFs (Appendix)
#------------------------------------------------------------------------------------
final_plot