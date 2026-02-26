# R/05_forecast_2026/02_scenarios.R
# Purpose: 2026 OBBA policy impact — ABAWD expanded to 55–64, reduced exemptions. Not simple extrapolation.
# Input:  data/derived/forecast_baseline.rds, config/globals.R (OBBA_EFFECTIVE_DATE, OBBA_AGE_EXPANSION)
# Output: data/derived/forecast_scenarios.rds, outputs/tables/forecast_2026_scenarios.csv

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)
if (!exists("OBBA_EFFECTIVE_DATE")) source("config/globals.R", local = TRUE)

path_baseline <- file.path(DIR_DERIVED, "forecast_baseline.rds")
out_rds       <- file.path(DIR_DERIVED, "forecast_scenarios.rds")
out_csv       <- file.path(DIR_OUT_TABLES, "forecast_2026_scenarios.csv")

stopifnot(file.exists(path_baseline))
baseline <- readRDS(path_baseline)
county_baseline <- baseline$county_baseline
did_att <- baseline$did_att

# OBBA: newly affected = 55–64. We don't have county 55–64 share; use state-level assumption.
# Share of ABAWD-eligible (or adult SNAP) who are 55–64 when OBBA takes effect (can be overwritten by data).
SHARE_55_64_NEWLY_AFFECTED <- 0.15   # 15% of current ABAWD-aged recipients in 55–64 bracket (placeholder)

# Optional: read county-level share from external file
path_obba_exempt <- file.path(DIR_RAW, "abawd_exemptions_fy26.csv")
obba_exempt <- NULL
if (exists("PATH_OBBA_EXEMPTIONS") && file.exists(PATH_OBBA_EXEMPTIONS)) {
  obba_exempt <- readr::read_csv(PATH_OBBA_EXEMPTIONS, show_col_types = FALSE)
  message("Loaded OBBA exemptions: ", nrow(obba_exempt), " rows")
}

# Scenario: apply DID ATT to "newly affected" share (55–64). Outcome is in log scale (outcome_final = log1p(per1k)).
# Delta_log = ATT * share_55_64  =>  obba_outcome_log = baseline_outcome_log + ATT * share_55_64
# Level: y_per1k_new = expm1(obba_outcome_log),  y_raw_approx = y_per1k_new * (pop_18_49/1000) for scaling
scenarios <- county_baseline %>%
  dplyr::mutate(
    scenario        = "OBBA_2026",
    share_55_64     = SHARE_55_64_NEWLY_AFFECTED,
    delta_log       = did_att * .data$share_55_64,
    obba_outcome_log = .data$baseline_outcome_log + .data$delta_log,
    baseline_y_per1k_18_49 = .data$baseline_y_per1k_18_49,
    obba_y_per1k_18_49    = expm1(.data$obba_outcome_log),
    # Approx change in level (adult recipients) using per-1k and pop
    pop_18_49       = .data$pop_18_49,
    baseline_y_raw  = .data$baseline_y_raw,
    obba_y_raw_approx = ifelse(!is.na(.data$pop_18_49) & .data$pop_18_49 > 0,
                               .data$obba_y_per1k_18_49 * .data$pop_18_49 / 1000, NA_real_),
    pct_change_log  = 100 * .data$delta_log,
    forecast_date    = OBBA_EFFECTIVE_DATE
  )

# Optional: merge exemption adjustment (e.g. reduce effective share in high-exemption counties)
if (!is.null(obba_exempt) && "county_id" %in% names(obba_exempt) && "exemption_share" %in% names(obba_exempt)) {
  scenarios <- scenarios %>%
    dplyr::left_join(
      obba_exempt %>% dplyr::select(.data$county_id, exemption_share = .data$exemption_share),
      by = c("id" = "county_id")
    ) %>%
    dplyr::mutate(
      share_55_64 = dplyr::if_else(!is.na(.data$exemption_share),
                                   .data$share_55_64 * (1 - .data$exemption_share),
                                   .data$share_55_64),
      delta_log   = did_att * .data$share_55_64,
      obba_outcome_log = .data$baseline_outcome_log + .data$delta_log,
      obba_y_per1k_18_49 = expm1(.data$obba_outcome_log)
    )
}

if (!dir.exists(DIR_OUT_TABLES)) dir.create(DIR_OUT_TABLES, recursive = TRUE)
readr::write_csv(scenarios, out_csv)
message("Wrote ", out_csv, "  n = ", nrow(scenarios))

forecast_scenarios <- list(
  scenarios   = scenarios,
  did_att     = did_att,
  share_55_64 = SHARE_55_64_NEWLY_AFFECTED,
  obba_date   = OBBA_EFFECTIVE_DATE,
  created     = Sys.time()
)
if (!dir.exists(DIR_DERIVED)) dir.create(DIR_DERIVED, recursive = TRUE)
saveRDS(forecast_scenarios, out_rds)
message("Wrote ", out_rds)
