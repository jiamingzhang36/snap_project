# R/02_abawd/03_figures_tables.R
# Input:  data/derived/abawd_att.rds or outputs/step1_did/*.csv
# Output: outputs/figures/abawd_es_adult.png etc (unified naming)

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)

# Stub: copy or regenerate figures from step1_did to outputs/figures/ with names abawd_*.png
step1 <- file.path(ROOT, "outputs", "step1_did")
if (dir.exists(step1)) {
  if (file.exists(file.path(step1, "fig_es_main.png"))) {
    file.copy(file.path(step1, "fig_es_main.png"), file.path(DIR_OUT_FIGURES, "abawd_es_main.png"), overwrite = TRUE)
    message("Copied abawd_es_main.png to outputs/figures/")
  }
  if (file.exists(file.path(step1, "fig_es_ddd_adult_child.png"))) {
    file.copy(file.path(step1, "fig_es_ddd_adult_child.png"), file.path(DIR_OUT_FIGURES, "abawd_es_ddd_adult_child.png"), overwrite = TRUE)
    message("Copied abawd_es_ddd_adult_child.png to outputs/figures/")
  }
}
