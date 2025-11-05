# Step 3: 2025 OBBBA forecast (skeleton)
# Comments are in English only
source("R/utils.R")

# 1) Inputs: Step2 tau_i (county-specific effects), county features, 2025 scenarios
out_dir <- safe_dir("outputs/step3_forecast")

# 2) TODO:
#    - Load Step2 outputs (e.g., step2_tau_i_pp.csv, step2_county_effects.csv)
#    - Build baseline 2025 caseload per county
#    - Apply county-specific treatment effects to construct forecasts
#    - Create scenario variants (e.g., unemployment +/- 5pp)
#    - Save: step3_tau_anchored.csv, step3_post_outputs.csv, optional map PNGs

message("[Step3] Forecast skeleton loaded. TODO: implement.")
