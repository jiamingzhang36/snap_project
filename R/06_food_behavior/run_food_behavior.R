# R/06_food_behavior/run_food_behavior.R
# Run the food behavior pipeline (build panel + ABAWD event study)

source("R/06_food_behavior/01_build_behavior_panel.R", local = new.env())
source("R/06_food_behavior/02_event_study_abawd_behavior.R", local = new.env())
source("R/06_food_behavior/03_robustness_abawd_behavior.R", local = new.env())
source("R/06_food_behavior/04_heterogeneity_urban_rural.R", local = new.env())
source("R/06_food_behavior/05_ea_event_study_behavior.R", local = new.env())
source("R/06_food_behavior/06_unemployment_dl_behavior.R", local = new.env())

message("food behavior pipeline done")
