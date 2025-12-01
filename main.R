# Pipeline driver 
source("R/utils.R")

message("== STEP 1: DID (Callaway–Sant'Anna) ==")
source("R/01_did_cs.R")   

message("== STEP 2: DML + Orthogonal BLP ==")
if (file.exists("R/02_dml_blp.R"))     source("R/02_dml_blp.R")   

message("== STEP 3: 2025 Forecast (OBBBA) ==")
if (file.exists("R/03_forecast_2025.R")) source("R/03_forecast_2025.R")  

message("== STEP 4: Individual Risk Scoring ==")
if (file.exists("R/04_individual_risk.R")) source("R/04_individual_risk.R")  

message("Pipeline finished.")
