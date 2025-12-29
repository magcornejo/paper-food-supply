# Paper Food Supply — Replication Repository

This repository contains the data and code required to replicate the empirical results of the paper:

**“Technological adaptation reduces global and local warming losses in food production”**

The repository is organized to ensure transparency, portability, and computational reproducibility.

---

## Repository structure

paper-food-supply/

- code/
    - 1_main.R
    - 2_clustering.R
    - 3_granger_causality.do

- data/
    - raw/
      - FAOSTAT_world.csv
      - annual_global_tempanom.xlsx
      - controls_USDA.csv

- processed/
    - initial_panel.csv
    - data_filtered.csv
    - for_granger_causality.csv

- climate/
    - temperature/
      - wide_temp_cropland.xlsx
      - wide_temp_unweighted.xlsx
    - precipitation/
      - wide_pre_cropland.xlsx
      - wide_pre_unweighted.xlsx
    - SPEI/
      - wide_drought_cropland.xlsx
      - wide_drought_unweighted.xlsx
      - wide_flood_cropland.xlsx
      - wide_flood_unweighted.xlsx

- instruments/
    - ERF.xlsx
    - world-administrative-boundaries.geojson

- output/
    - figures/
    - tables/

- README.md

---

## Description of scripts

- **`1_main.R`**  
  Main script.  
  - Constructs the panel dataset.
  - Merges climate, production, and control variables.
  - Estimates baseline econometric models.
  - Produces core figures and intermediate datasets used in subsequent analyses.

- **`2_clustering.R`**  
  Implements the clustering strategy used to group countries according to agricultural technology indicators.

- **`3_granger_causality.do`**  
  Stata script performing panel Granger causality tests using the dataset generated in `1_main.R`.

All scripts rely exclusively on **relative paths** and can be executed from the project root directory.

---

## Data sources

The empirical analysis combines data from the following publicly available sources:

- **FAOSTAT**: Food production indices at the country level.  
- **Global temperature anomalies**: Annual global temperature deviations.  
- **USDA**: Agricultural input controls.  
- **Gridded climate data** (temperature, precipitation, SPEI): Aggregated to the country level from publicly available climate datasets.

Processed datasets are included for transparency and to reduce computational burden during replication.

---

## Replication instructions

To replicate the results:

1. Open R and set the working directory to the project root:
   ```r
   setwd("path/to/paper-food-supply")
Run the main R script:

r
source("code/1_main.R")
Run the clustering analysis:

r
source("code/2_clustering.R")
Open Stata, set the working directory to the project root, and run:

stata
do code/3_granger_causality.do
All tables and figures are generated programmatically.

Software requirements
R (version ≥ 4.0)

Required R packages are installed automatically by the scripts.

Stata (version ≥ 16) for the Granger causality analysis.

Notes on reproducibility
Output files (figures and tables) are intentionally excluded from version control to ensure results are generated directly from the code.

Minor numerical differences may arise across systems due to differences in operating systems or package versions.

The repository reflects the version of the data and code used for the submitted manuscript.

Contact
For questions regarding replication or data construction, please contact the authors.
