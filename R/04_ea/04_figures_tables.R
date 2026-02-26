# R/04_ea/04_figures_tables.R
# Input:  data/derived/ea_avgpp_es.rds, data/derived/ea_participation_es.rds
# Output: outputs/tables/ea_es_avgpp.csv, outputs/tables/ea_es_participation.csv (summary tables for paper)

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)

if (!dir.exists(DIR_OUT_TABLES)) dir.create(DIR_OUT_TABLES, recursive = TRUE)

if (file.exists(file.path(DIR_DERIVED, "ea_avgpp_es.rds"))) {
  ea_avgpp <- readRDS(file.path(DIR_DERIVED, "ea_avgpp_es.rds"))
  readr::write_csv(ea_avgpp$es_df, file.path(DIR_OUT_TABLES, "ea_es_avgpp.csv"))
  message("Wrote outputs/tables/ea_es_avgpp.csv")
}

if (file.exists(file.path(DIR_DERIVED, "ea_participation_es.rds"))) {
  ea_part <- readRDS(file.path(DIR_DERIVED, "ea_participation_es.rds"))
  readr::write_csv(ea_part$es_df, file.path(DIR_OUT_TABLES, "ea_es_participation.csv"))
  message("Wrote outputs/tables/ea_es_participation.csv")
}
