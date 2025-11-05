source("R/utils.R")
message("== STEP 1: CLEAN ==")
if (file.exists("R/01_clean_acs.R"))   source("R/01_clean_acs.R")
if (file.exists("R/01_clean_other.R")) source("R/01_clean_other.R")

message("== STEP 2: DML + BLP ==")
if (file.exists("R/02_dml_blp.R"))     source("R/02_dml_blp.R")

message("== STEP 3: DID (CS) ==")
source("R/01_did_cs.R")

message("== STEP 4: POST-PROCESS ==")
if (file.exists("R/04_postprocess.R")) source("R/04_postprocess.R")

message("Pipeline finished.")
