# R/05_forecast_2026/02_scenarios.R
# Purpose: 2026 OBBA policy impact — ABAWD expanded to 55-64, reduced exemptions.
# Now includes: county-specific age shares, uncertainty intervals, labor-market scenarios.
# Input:  data/derived/forecast_baseline.rds, config/globals.R
#         (optional) data/external/acs_age_shares_MI.csv
# Output: data/derived/forecast_scenarios.rds, outputs/tables/forecast_2026_scenarios.csv,
#         outputs/tables/forecast_2026_summary.csv

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)
source("R/00_utils/helpers_forecast.R", local = TRUE)
if (!exists("OBBA_EFFECTIVE_DATE")) source("config/globals.R", local = TRUE)

path_baseline <- file.path(DIR_DERIVED, "forecast_baseline.rds")
out_rds       <- file.path(DIR_DERIVED, "forecast_scenarios.rds")
out_csv       <- file.path(DIR_OUT_TABLES, "forecast_2026_scenarios.csv")
out_summary   <- file.path(DIR_OUT_TABLES, "forecast_2026_summary.csv")

stopifnot(file.exists(path_baseline))
baseline <- readRDS(path_baseline)
county_baseline <- baseline$county_baseline
did_att <- baseline$did_att
did_se  <- baseline$did_se

# ---------------------------------------------------------------------------
# 1) County-specific 55-64 age shares (or state-average fallback)
# ---------------------------------------------------------------------------
STATE_AVG_SHARE_55_64 <- 0.15

path_age_shares <- file.path(ROOT, "data", "external", "acs_age_shares_MI.csv")
if (file.exists(path_age_shares)) {
  age_shares <- readr::read_csv(path_age_shares, show_col_types = FALSE)
  county_baseline <- county_baseline %>%
    dplyr::left_join(age_shares %>% dplyr::select(id, share_55_64), by = "id")
  county_baseline$share_55_64[is.na(county_baseline$share_55_64)] <- STATE_AVG_SHARE_55_64
  message("Loaded county-specific age shares (", sum(!is.na(age_shares$share_55_64)), " counties)")
} else {
  county_baseline$share_55_64 <- STATE_AVG_SHARE_55_64
  message("Using state-average share_55_64 = ", STATE_AVG_SHARE_55_64, " (no acs_age_shares_MI.csv)")
}

# Optional: OBBA exemptions
if (exists("PATH_OBBA_EXEMPTIONS") && file.exists(PATH_OBBA_EXEMPTIONS)) {
  obba_exempt <- readr::read_csv(PATH_OBBA_EXEMPTIONS, show_col_types = FALSE)
  if ("county_id" %in% names(obba_exempt) && "exemption_share" %in% names(obba_exempt)) {
    county_baseline <- county_baseline %>%
      dplyr::left_join(obba_exempt %>% dplyr::select(county_id, exemption_share),
                       by = c("id" = "county_id")) %>%
      dplyr::mutate(share_55_64 = dplyr::if_else(!is.na(exemption_share),
                                                   share_55_64 * (1 - exemption_share),
                                                   share_55_64)) %>%
      dplyr::select(-dplyr::any_of("exemption_share"))
    message("Applied OBBA exemptions.")
  }
}

# ---------------------------------------------------------------------------
# 2) Baseline scenario: point estimates
# ---------------------------------------------------------------------------
run_scenario <- function(county_bl, att, share_col = "share_55_64", scenario_name = "baseline") {
  county_bl %>%
    dplyr::mutate(
      scenario         = scenario_name,
      share_affected   = .data[[share_col]],
      delta_log        = att * .data$share_affected,
      obba_outcome_log = .data$baseline_outcome_log + .data$delta_log,
      obba_y_per1k     = expm1(.data$obba_outcome_log),
      obba_y_raw_approx = ifelse(!is.na(.data$pop_18_49) & .data$pop_18_49 > 0,
                                  .data$obba_y_per1k * .data$pop_18_49 / 1000, NA_real_),
      pct_change_log   = 100 * .data$delta_log,
      forecast_date    = OBBA_EFFECTIVE_DATE
    )
}

scenarios_baseline <- run_scenario(county_baseline, did_att, scenario_name = "baseline")

# ---------------------------------------------------------------------------
# 3) Alternative labor-market scenarios
# ---------------------------------------------------------------------------
# Recession: unemployment rises by 3 pp (calibrated to Michigan 2008-2009)
# Recovery: unemployment falls by 1.5 pp
# These affect county-level waiver eligibility thresholds, not the ATT itself.
# For simplicity, model as multiplier on effective share_55_64:
# - Higher unemployment => more counties may qualify for waivers => lower effective share
# - Lower unemployment => fewer waivers => higher effective share
RECESSION_UNEMP_SHIFT <- 3.0
RECOVERY_UNEMP_SHIFT  <- -1.5
WAIVER_THRESHOLD       <- 10.0  # Unemployment rate above which counties get ABAWD waivers

county_baseline_recession <- county_baseline %>%
  dplyr::mutate(
    unemp_scenario = unemp_last + RECESSION_UNEMP_SHIFT,
    share_55_64_scenario = dplyr::if_else(
      !is.na(unemp_scenario) & unemp_scenario >= WAIVER_THRESHOLD,
      share_55_64 * 0.3,  # Waiver-eligible: much smaller effective exposure
      share_55_64
    )
  )

county_baseline_recovery <- county_baseline %>%
  dplyr::mutate(
    unemp_scenario = unemp_last + RECOVERY_UNEMP_SHIFT,
    share_55_64_scenario = dplyr::if_else(
      !is.na(unemp_scenario) & unemp_scenario >= WAIVER_THRESHOLD,
      share_55_64 * 0.3,
      share_55_64
    )
  )

scenarios_recession <- run_scenario(county_baseline_recession, did_att,
                                     share_col = "share_55_64_scenario",
                                     scenario_name = "recession")
scenarios_recovery  <- run_scenario(county_baseline_recovery, did_att,
                                     share_col = "share_55_64_scenario",
                                     scenario_name = "recovery")

# Combine all scenarios
all_scenarios <- dplyr::bind_rows(scenarios_baseline, scenarios_recession, scenarios_recovery)

# ---------------------------------------------------------------------------
# 4) Uncertainty intervals (parametric bootstrap on ATT)
# ---------------------------------------------------------------------------
boot_result <- NULL
if (!is.na(did_att) && !is.na(did_se) && did_se > 0) {
  boot_result <- bootstrap_scenarios(
    county_baseline = county_baseline,
    did_att         = did_att,
    did_se          = did_se,
    share_55_64     = county_baseline$share_55_64,
    n_boot          = 2000,
    seed            = 42
  )

  # Attach CI and rank uncertainty to baseline scenario
  scenarios_baseline <- scenarios_baseline %>%
    dplyr::left_join(boot_result$county_ci, by = "id") %>%
    dplyr::left_join(boot_result$rank_ci,   by = "id")

  message("Bootstrap uncertainty: aggregate loss 90% CI = [",
          round(boot_result$aggregate_ci$value[2], 1), ", ",
          round(boot_result$aggregate_ci$value[3], 1), "]")
} else {
  message("Cannot compute uncertainty: ATT or SE is NA.")
}

# ---------------------------------------------------------------------------
# 5) Summary table
# ---------------------------------------------------------------------------
make_summary <- function(scen_df, label) {
  scen_df %>%
    dplyr::summarise(
      scenario            = label,
      n_counties          = dplyr::n(),
      mean_delta_log      = mean(delta_log, na.rm = TRUE),
      mean_pct_change     = mean(pct_change_log, na.rm = TRUE),
      sum_baseline_y_raw  = sum(baseline_y_raw, na.rm = TRUE),
      sum_obba_y_raw      = sum(obba_y_raw_approx, na.rm = TRUE),
      total_loss_approx   = sum(obba_y_raw_approx - baseline_y_raw, na.rm = TRUE),
      .groups = "drop"
    )
}

summary_all <- dplyr::bind_rows(
  make_summary(scenarios_baseline, "baseline"),
  make_summary(scenarios_recession, "recession"),
  make_summary(scenarios_recovery, "recovery")
)

# Add aggregate CI to summary
if (!is.null(boot_result)) {
  summary_all$loss_ci_lo <- NA_real_
  summary_all$loss_ci_hi <- NA_real_
  summary_all$loss_ci_lo[summary_all$scenario == "baseline"] <- boot_result$aggregate_ci$value[2]
  summary_all$loss_ci_hi[summary_all$scenario == "baseline"] <- boot_result$aggregate_ci$value[3]
}

if (!dir.exists(DIR_OUT_TABLES)) dir.create(DIR_OUT_TABLES, recursive = TRUE)
readr::write_csv(all_scenarios, out_csv)
readr::write_csv(summary_all, out_summary)
message("Wrote ", out_csv, " (", nrow(all_scenarios), " rows, 3 scenarios)")
message("Wrote ", out_summary)

# ---------------------------------------------------------------------------
# 6) Save
# ---------------------------------------------------------------------------
forecast_scenarios <- list(
  scenarios_baseline  = scenarios_baseline,
  scenarios_recession = scenarios_recession,
  scenarios_recovery  = scenarios_recovery,
  all_scenarios       = all_scenarios,
  summary             = summary_all,
  boot_result         = boot_result,
  did_att             = did_att,
  did_se              = did_se,
  obba_date           = OBBA_EFFECTIVE_DATE,
  created             = Sys.time()
)
if (!dir.exists(DIR_DERIVED)) dir.create(DIR_DERIVED, recursive = TRUE)
saveRDS(forecast_scenarios, out_rds)
message("Wrote ", out_rds)
