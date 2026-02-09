# config/globals.R — Constants, event months, variable name mapping
# Policy timeline lives here or in data/raw/*.csv, not hardcoded in scripts
if (!exists("DIR_RAW")) source("config/paths.R", local = parent.frame())

# ABAWD treatment timing: read from CSV if present; 0 = never-treated
PATH_ABAWD_TIMING <- file.path(DIR_RAW, "abawd_waiver_timing.csv")
if (file.exists(PATH_ABAWD_TIMING)) {
  abawd_timing <- read.csv(PATH_ABAWD_TIMING, stringsAsFactors = FALSE)
  # Expected cols: county, treat_month (YYYY-MM), source_note
} else {
  abawd_timing <- NULL
}

# EA policy dates
PATH_EA_POLICY <- file.path(DIR_RAW, "ea_policy_dates.csv")
if (file.exists(PATH_EA_POLICY)) {
  ea_policy_dates <- read.csv(PATH_EA_POLICY, stringsAsFactors = FALSE)
  # Expected cols: event, date
} else {
  ea_policy_dates <- data.frame(event = "EA_end", date = "2023-03-01", stringsAsFactors = FALSE)
}

# Variable name mapping (aligned with 01_clean and did scripts)
VAR_LOG_CASES     <- "ln_cases"
VAR_LOG_RECIPIENTS <- "ln_recipients"
VAR_LOG_ADULT     <- "log1p(adult_recipients)"
VAR_LOG_CHILD     <- "log1p(child_recipients)"
VAR_UNEMP         <- "unemployment_rate"
VAR_Y_PER1K       <- "y_per1k_18_49"

# Event-study reference period, DL lag length
EVENT_REF_PERIOD  <- -1L
LAG_MAX_DL        <- 12L
