# R/99_run/run_paper_outputs.R — Update paper figures/tables only (fast)
# Assumes data/derived/*.rds exist; runs only 02–05 figures/tables

if (!dir.exists("config")) { if (file.exists("snap_project.Rproj")) setwd(".") else setwd("..") }
if (!dir.exists("config")) stop("Run from project root.")
ROOT <- getwd()
setwd(ROOT)

source("config/paths.R", local = new.env())
source("R/00_utils/packages.R", local = new.env())

source("R/02_abawd/03_figures_tables.R", local = new.env())
source("R/03_income/03_figures_tables.R", local = new.env())
source("R/04_ea/04_figures_tables.R", local = new.env())
source("R/05_forecast_2026/03_outputs_maps.R", local = new.env())

message("run_paper_outputs done.")
