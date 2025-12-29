#===============================================================================
# CLUSTERING ANALYSIS
#===============================================================================


library(here)
library(readr)          
library(dplyr)          
library(ggplot2)        
library(sf)             
library(rnaturalearth)  
library(rnaturalearthdata)
library(tibble)
library(cowplot)
library(tidyr)

# ========================
# 0) Load and prepare
# ========================
data <- read_csv(here::here("data", "data_filtered_irrig_fert_cap_no_indices.csv"))

data <- data %>%
  select(Area, Year, a3, Irrig, Fertilizer, Capital) %>%
  drop_na()

# Annual global means
data <- data %>%
  group_by(Year) %>%
  mutate(
    irri_total = mean(Irrig, na.rm = TRUE),
    fert_total = mean(Fertilizer, na.rm = TRUE),
    cap_total  = mean(Capital, na.rm = TRUE)
  ) %>%
  ungroup()

# Relative indices (country / global mean in the same year)
data <- data %>%
  mutate(
    indice_irri = Irrig / irri_total,
    indice_fert = Fertilizer / fert_total,
    indice_cap  = Capital / cap_total
  )

# ==============================================================================
# 1) Collapse by country (median log for robustness)
# ==============================================================================
data_summary <- data %>%
  group_by(a3) %>%
  summarise(
    Irrig_mediana      = median(indice_irri, na.rm = TRUE),
    Fertilizer_mediana = median(indice_fert, na.rm = TRUE),
    Capital_mediana    = median(indice_cap,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Irrig_mediana_log      = log(Irrig_mediana),
    Fertilizer_mediana_log = log(Fertilizer_mediana),
    Capital_mediana_log    = log(Capital_mediana)
  )

# ==============================================================================
# 2) Helpers
# ==============================================================================

relabel_clusters <- function(x, cl_raw) {
  df <- tibble(x = x, cl = cl_raw)
  ord <- df %>%
    group_by(cl) %>%
    summarise(mu = mean(x, na.rm = TRUE), .groups = "drop") %>%
    arrange(mu)
  ord$rank <- seq_len(nrow(ord))    # 1=Low, 2=Med, 3=High
  map_code <- setNames(c(2, 1, 3), ord$rank)   # 1->2, 2->1, 3->3
  rank_by_cl <- ord$rank[match(cl_raw, ord$cl)]
  unname(map_code[as.character(rank_by_cl)])
}

mk_labels <- function(vec_codes, base_label) {
  N <- table(factor(vec_codes, levels = c(1,2,3)))
  c(
    "1" = sprintf("Med %s (N=%d)",  base_label, N[["1"]]),
    "2" = sprintf("Low %s (N=%d)",  base_label, N[["2"]]),
    "3" = sprintf("High %s (N=%d)", base_label, N[["3"]])
  )
}

paleta <- c("1" = "#F4A582", "2" = "#92C5DE", "3" = "lightgreen")  # 1=Med, 2=Low, 3=High
breaks_order <- c("3","1","2")  # High, Med, Low

make_map <- function(world, df, var_cluster, labels_named, titulo = NULL) {
  map_data <- world %>% left_join(df, by = c("adm0_a3" = "a3"))
  map_data[[var_cluster]] <- factor(as.character(map_data[[var_cluster]]),
                                    levels = c("1","2","3"))
  ggplot(map_data) +
    geom_sf(data = map_data[is.na(map_data[[var_cluster]]), ],
            fill = "lightgray", color = "white") +
    geom_sf(data = map_data[!is.na(map_data[[var_cluster]]), ],
            aes_string(fill = var_cluster), color = "white") +
    scale_fill_manual(values = paleta, labels = labels_named, breaks = breaks_order) +
    coord_sf(ylim = c(-60, 90)) +
    labs(fill = NULL, title = titulo) +
    theme_void() +
    theme(
      legend.position = "bottom",
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold")
    )
}

code_to_label <- function(x) {
  dplyr::case_when(
    x == 3 ~ "high",
    x == 1 ~ "med",
    x == 2 ~ "low",
    TRUE   ~ NA_character_
  )
}

# ==============================================================================
# 3) K-means clustering + relabeling
# ==============================================================================
set.seed(123)

# IRRIGATION
km_irrig <- kmeans(scale(data_summary$Irrig_mediana_log), centers = 3, nstart = 50)
data_summary$cluster_irrig <- relabel_clusters(data_summary$Irrig_mediana_log, km_irrig$cluster)

# FERTILIZER
km_fert <- kmeans(scale(data_summary$Fertilizer_mediana_log), centers = 3, nstart = 50)
data_summary$cluster_fert <- relabel_clusters(data_summary$Fertilizer_mediana_log, km_fert$cluster)

# CAPITAL
km_cap <- kmeans(scale(data_summary$Capital_mediana_log), centers = 3, nstart = 50)
data_summary$cluster_cap <- relabel_clusters(data_summary$Capital_mediana_log, km_cap$cluster)

data_summary %>% count(cluster_irrig) %>% print()
data_summary %>% count(cluster_fert)  %>% print()
data_summary %>% count(cluster_cap)   %>% print()

# ==============================================================================
# 4) Maps
# ==============================================================================
world <- ne_countries(scale = "medium", returnclass = "sf")

lab_irri <- mk_labels(data_summary$cluster_irrig, "Irrig")
lab_fert <- mk_labels(data_summary$cluster_fert,  "Fert")
lab_cap  <- mk_labels(data_summary$cluster_cap,   "Cap")

p_irri <- make_map(world, data_summary, "cluster_irrig", lab_irri, titulo = "(a) Irrigation") +
  theme(
    plot.title = element_text(hjust = 0, size = 12, face = "bold")  
  )
p_fert <- make_map(world, data_summary, "cluster_fert",  lab_fert, titulo = "(b) Fertilizer") +
  theme(
    plot.title = element_text(hjust = 0, size = 12, face = "bold")  
  )
p_cap <- make_map(world, data_summary, "cluster_cap", lab_cap, titulo = "(c) Capital") +
  theme(
    plot.title = element_text(hjust = 0, size = 12, face = "bold")  
  )

top_row <- plot_grid(
  p_irri + theme(plot.title = element_text(hjust = 0, size = 12, face = "bold")),
  p_fert + theme(plot.title = element_text(hjust = 0, size = 12, face = "bold")),
  labels = NULL,
  ncol = 2,
  align = "h"
)

plot_row <- plot_grid(
  top_row,
  p_cap,
  ncol = 1,
  rel_heights = c(1, 0.8)
)

ggsave(
  filename = "map_clusters.eps",
  plot = plot_row,
  device = "eps",
  width = 10,   # ajustá a tu gusto
  height = 8,   # idem
  dpi = 300
)

# ==============================================================================
# 5) Export .csv 
# ==============================================================================
label_map <- c("1" = "med", "2" = "low", "3" = "high")

clusters_final_labels_only <- data_resumida %>%
  transmute(
    a3,
    irrig = unname(label_map[as.character(cluster_irrig)]),
    fert  = unname(label_map[as.character(cluster_fert)]),
    cap   = unname(label_map[as.character(cluster_cap)])
  )


write_csv(clusters_final_labels_only, here::here("clusters", "clusters_final.csv"))



##### HIERARCHICAL CLUSTERING

# ==============================================================================
# 1) Univariate clustering + relabeling
# ==============================================================================

hc_univar_codes <- function(x, k = 3, method = "ward.D2", scale_it = TRUE) {
  z <- if (scale_it) scale(x) else matrix(x, ncol = 1)
  # Distancia euclídea (equivalente en 1D)
  d <- dist(z, method = "euclidean")
  fit <- hclust(d, method = method)
  raw_groups <- cutree(fit, k = k)
  # Re-etiquetar a {1=Med, 2=Low, 3=High} según media de x (no de z)
  relabel_clusters(x, raw_groups)
}

# IRRIGATION
data_summary$cluster_irrig <- hc_univar_codes(data_summary$Irrig_mediana_log, k = 3)

# FERTILIZER
data_summary$cluster_fert  <- hc_univar_codes(data_summary$Fertilizer_mediana_log, k = 3)

# CAPITAL
data_summary$cluster_cap   <- hc_univar_codes(data_summary$Capital_mediana_log, k = 3)

data_resumida %>% count(cluster_irrig) %>% print()
data_resumida %>% count(cluster_fert)  %>% print()
data_resumida %>% count(cluster_cap)   %>% print()


# ==============================================================================
# 2) Maps
# ==============================================================================
world <- ne_countries(scale = "medium", returnclass = "sf")

lab_irri <- mk_labels(data_summary$cluster_irrig, "Irrig")
lab_fert <- mk_labels(data_summary$cluster_fert,  "Fert")
lab_cap  <- mk_labels(data_summary$cluster_cap,   "Cap")

p_irri <- make_map(world, data_summary, "cluster_irrig", lab_irri, titulo = "Irrigation")
p_fert <- make_map(world, data_summary, "cluster_fert",  lab_fert, titulo = "Fertilizer")
p_cap  <- make_map(world, data_summary, "cluster_cap",   lab_cap,  titulo = "Capital")


top_row <- plot_grid(
  p_irri, p_fert,
  # labels = c("A", "B"),
  ncol = 2,
  align = "h"
)

plot_row <- plot_grid(
  top_row,
  p_cap,
  # labels = c("", "C"),
  ncol = 1,
  rel_heights = c(1, 0.8)  
)
plot_row


# ==============================================================================
# 3) Export .csv 
# ==============================================================================
label_map <- c("1" = "med", "2" = "low", "3" = "high")

clusters_final_labels_only <- data_summary %>%
  transmute(
    a3,
    irrig = unname(label_map[as.character(cluster_irrig)]),
    fert  = unname(label_map[as.character(cluster_fert)]),
    cap   = unname(label_map[as.character(cluster_cap)])
  )


write_csv(
  clusters_final_labels_only,
  "/Data/Clusters/clusters_final_jerarquico.csv"
)
