# config/globals.R — Constants, event months, variable name mapping
# Policy timeline lives here or in data/raw/*.csv, not hardcoded in scripts
if (!exists("DIR_RAW")) source("config/paths.R", local = parent.frame())

# ABAWD treatment timing: canonical source is R/00_utils/helpers_policy.R
# (hardcoded waiver coverage vectors derived from FNS approval documents).
# data/raw/abawd_waiver_timing.csv exists but is currently empty (header only).

# 2026 OBBA (federal): ABAWD work requirements expand to age 55–64 (was 18–54); reduced state discretionary exemptions.
# 05_forecast_2026 = impact under this new policy, not simple time-series forecast.
OBBA_EFFECTIVE_DATE   <- as.Date("2026-01-01")
OBBA_AGE_EXPANSION    <- c(55L, 64L)   # newly subject to work requirement (was 18–54)
PATH_OBBA_EXEMPTIONS  <- file.path(DIR_RAW, "abawd_exemptions_fy26.csv")  # optional: state/county exemptions FY26

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
