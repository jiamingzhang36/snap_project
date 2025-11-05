# Pipeline driver (comments in English)
source("R/utils.R")

message("== STEP 1: DID (Callaway–Sant'Anna) ==")
# If you还有预清洗脚本，会在 01_did_cs.R 内部或之前使用
source("R/01_did_cs.R")   # 主DID与诊断，输出到 outputs/step1_did/

message("== STEP 2: DML + Orthogonal BLP ==")
if (file.exists("R/02_dml_blp.R"))     source("R/02_dml_blp.R")   # 输出到 outputs/step2_dml/

message("== STEP 3: 2025 Forecast (OBBBA) ==")
if (file.exists("R/03_forecast_2025.R")) source("R/03_forecast_2025.R")  # 输出到 outputs/step3_forecast/

message("== STEP 4: Individual Risk Scoring ==")
if (file.exists("R/04_individual_risk.R")) source("R/04_individual_risk.R")  # 输出到 outputs/step4_risk/

message("Pipeline finished.")
