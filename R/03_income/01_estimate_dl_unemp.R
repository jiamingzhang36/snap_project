# R/03_income/01_estimate_dl_unemp.R
# Input:  data/derived/panel_analysis.rds
# Output: outputs/tables/income_dl_main.csv, data/derived/unemp_dl_model.rds

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)

# Stub: distributed lag / dynamic response; read panel_analysis, regress log_cases or y_per1k on du_yoy/du_mom
panel <- readRDS(file.path(DIR_DERIVED, "panel_analysis.rds"))
if (!dir.exists(DIR_OUT_TABLES)) dir.create(DIR_OUT_TABLES, recursive = TRUE)
message("03_income/01: stub. Implement DL model and write income_dl_main.csv + unemp_dl_model.rds")
