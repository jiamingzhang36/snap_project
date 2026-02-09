# R/02_abawd/02_estimate_event_study.R
# Input:  data/derived/panel_analysis.rds (or panel_abawd_event.rds)
# Output: outputs/tables/abawd_es_main.csv, outputs/figures/abawd_es_main.png, data/derived/abawd_att.rds

# Main ABAWD estimation lives in R/01_did_cs.R; this script is a stub that can be switched to read panel_analysis.rds and call did + fixest
# Migration: source("R/01_did_cs.R") or move 01_did_cs.R logic here and read data/derived/panel_analysis.rds

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)

panel <- readRDS(file.path(DIR_DERIVED, "panel_analysis.rds"))
if (!file.exists(DIR_OUT_FIGURES)) dir.create(DIR_OUT_FIGURES, recursive = TRUE)
if (!file.exists(DIR_OUT_TABLES)) dir.create(DIR_OUT_TABLES, recursive = TRUE)

# Stub: call run_cs_did + DDD from 01_did_cs.R, or reimplement panel-based version here
message("ABAWD event-study: use R/01_did_cs.R for full run. This stub writes output paths.")
write.csv(data.frame(note = "Run R/01_did_cs.R for ABAWD + DDD outputs"), file.path(DIR_OUT_TABLES, "abawd_es_main.csv"), row.names = FALSE)
