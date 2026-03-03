# R/05_forecast_2026/02_scenarios.R
# Purpose: 2026 OBBA policy impact — ABAWD expanded to 55-64, reduced exemptions.
# Now includes: county-specific age shares, county-specific ATT from heterogeneity model,
#               uncertainty intervals, labor-market scenarios.
# Input:  data/derived/forecast_baseline.rds, config/globals.R
#         (optional) data/external/acs_age_shares_MI.csv
#         (optional) outputs/tables/abawd_county_att_hat.csv (county-specific ATT)
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
# 0) County-specific ATT from heterogeneity interaction model (optional)
# ---------------------------------------------------------------------------
# If available, use county-specific ATT (from interaction model) instead of uniform ATT.
# This makes forecast projections differentiated across counties based on their characteristics.
path_county_att <- file.path(DIR_OUT_TABLES, "abawd_county_att_hat.csv")
use_het_att <- FALSE
if (file.exists(path_county_att)) {
  county_att <- readr::read_csv(path_county_att, show_col_types = FALSE)
  county_att$id <- as.character(county_att$id)
  if (nrow(county_att) > 0 && any(!is.na(county_att$att_hat))) {
    county_baseline$id <- as.character(county_baseline$id)
    county_baseline <- county_baseline %>%
      dplyr::left_join(county_att %>% dplyr::select(id, att_hat), by = "id")
    # Use county-specific ATT where available, uniform ATT as fallback
    county_baseline$att_county <- dplyr::if_else(
      !is.na(county_baseline$att_hat),
      county_baseline$att_hat,
      did_att
    )
    use_het_att <- TRUE
    message("Loaded county-specific ATT (range: ",
            round(min(county_baseline$att_county, na.rm = TRUE), 4), " to ",
            round(max(county_baseline$att_county, na.rm = TRUE), 4), ")")
  }
}
if (!use_het_att) {
  county_baseline$att_county <- did_att
  message("Using uniform ATT = ", round(did_att, 4), " for all counties")
}

# ---------------------------------------------------------------------------
# 1) County-specific 55-64 age shares (or state-average fallback)
# ---------------------------------------------------------------------------
STATE_AVG_SHARE_55_64 <- 0.15

path_age_shares <- file.path(ROOT, "data", "external", "acs_age_shares_MI.csv")
if (file.exists(path_age_shares)) {
  age_shares <- readr::read_csv(path_age_shares, show_col_types = FALSE)
  age_shares$id <- as.character(age_shares$id)
  county_baseline$id <- as.character(county_baseline$id)
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
run_scenario <- function(county_bl, att_col = "att_county", share_col = "share_55_64",
                         baseline_log_col = "baseline_outcome_log",
                         scenario_name = "baseline") {
  county_bl %>%
    dplyr::mutate(
      scenario         = scenario_name,
      share_affected   = .data[[share_col]],
      att_used         = .data[[att_col]],
      baseline_log_scen = .data[[baseline_log_col]],
      delta_log        = .data$att_used * .data$share_affected,
      obba_outcome_log = .data$baseline_log_scen + .data$delta_log,
      obba_y_per1k     = expm1(.data$obba_outcome_log),
      baseline_y_per1k_scen = expm1(.data$baseline_log_scen),
      obba_y_raw_approx = ifelse(!is.na(.data$pop_18_49) & .data$pop_18_49 > 0,
                                  .data$obba_y_per1k * .data$pop_18_49 / 1000, NA_real_),
      baseline_y_raw_scen = ifelse(!is.na(.data$pop_18_49) & .data$pop_18_49 > 0,
                                  .data$baseline_y_per1k_scen * .data$pop_18_49 / 1000, NA_real_),
      pct_change_log   = 100 * .data$delta_log,
      forecast_date    = OBBA_EFFECTIVE_DATE
    )
}

scenarios_baseline <- run_scenario(county_baseline, scenario_name = "baseline")

# ---------------------------------------------------------------------------
# 3) Alternative labor-market scenarios (multi-channel)
# ---------------------------------------------------------------------------
# Three channels through which labor-market conditions affect projections:
#   (a) Compliance channel: unemployment → difficulty meeting work requirements
#       Higher unemployment → fewer job opportunities → harder to comply →
#       larger (more negative) ATT.  Calibrated conservatively: 8% increase
#       in ATT magnitude per 1 pp increase in unemployment.
#   (b) Enrollment channel: unemployment → SNAP caseload size.
#       Uses pre-treatment SNAP-unemployment semi-elasticity from
#       the stabilizer analysis (mean = 0.033 log pts per 1 pp).
#   (c) Waiver channel: counties with unemployment >= 10% historically
#       receive ABAWD waivers, reducing the effective exposed population.
#       Calibrated to FNS waiver approval patterns (70% reduction).

RECESSION_UNEMP_SHIFT <- 3.0     # pp increase (calibrated to 2008-2009 MI)
RECOVERY_UNEMP_SHIFT  <- -2.0    # pp decrease (tight labor market)
WAIVER_THRESHOLD      <- 10.0    # FNS waiver eligibility threshold

# (a) Compliance channel: ATT magnitude scales with unemployment shift
#     Conservative: 8% per 1pp.  E.g., recession (+3pp) → 1.24x multiplier.
COMPLIANCE_ELASTICITY <- 0.08

# (b) Enrollment channel: SNAP-unemployment semi-elasticity
#     From stabilizer analysis: 1 pp unemployment → 0.033 log-point SNAP increase
SNAP_UNEMP_ELASTICITY <- 0.033

build_scenario <- function(county_bl, unemp_shift, scenario_name) {
  county_bl %>%
    dplyr::mutate(
      unemp_scenario = unemp_last + unemp_shift,

      # (a) Compliance channel: scale ATT magnitude by unemployment change
      compliance_mult = 1 + COMPLIANCE_ELASTICITY * unemp_shift,
      att_scenario = att_county * compliance_mult,

      # (b) Enrollment channel: adjust baseline caseload for SNAP enrollment response
      baseline_outcome_log_scenario = baseline_outcome_log +
        SNAP_UNEMP_ELASTICITY * unemp_shift,

      # (c) Waiver channel: counties crossing threshold get partial protection
      share_55_64_scenario = dplyr::if_else(
        !is.na(unemp_scenario) & unemp_scenario >= WAIVER_THRESHOLD,
        share_55_64 * 0.3,  # Waiver-eligible: much smaller effective exposure
        share_55_64
      )
    )
}

county_baseline_recession <- build_scenario(county_baseline, RECESSION_UNEMP_SHIFT, "recession")
county_baseline_recovery  <- build_scenario(county_baseline, RECOVERY_UNEMP_SHIFT,  "recovery")

scenarios_recession <- run_scenario(county_baseline_recession,
                                     att_col      = "att_scenario",
                                     share_col    = "share_55_64_scenario",
                                     baseline_log_col = "baseline_outcome_log_scenario",
                                     scenario_name = "recession")
scenarios_recovery  <- run_scenario(county_baseline_recovery,
                                     att_col      = "att_scenario",
                                     share_col    = "share_55_64_scenario",
                                     baseline_log_col = "baseline_outcome_log_scenario",
                                     scenario_name = "recovery")

# Log scenario diagnostics
n_waiver_rec <- sum(county_baseline_recession$unemp_scenario >= WAIVER_THRESHOLD, na.rm = TRUE)
n_waiver_rec_base <- sum(county_baseline$unemp_last >= WAIVER_THRESHOLD, na.rm = TRUE)
message("Recession scenario: ATT multiplier = ", round(1 + COMPLIANCE_ELASTICITY * RECESSION_UNEMP_SHIFT, 2),
        ", baseline log shift = +", round(SNAP_UNEMP_ELASTICITY * RECESSION_UNEMP_SHIFT, 3),
        ", waiver counties: ", n_waiver_rec_base, " -> ", n_waiver_rec)
message("Recovery scenario:  ATT multiplier = ", round(1 + COMPLIANCE_ELASTICITY * RECOVERY_UNEMP_SHIFT, 2),
        ", baseline log shift = ", round(SNAP_UNEMP_ELASTICITY * RECOVERY_UNEMP_SHIFT, 3),
        ", waiver counties: ", n_waiver_rec_base, " -> ",
        sum(county_baseline_recovery$unemp_scenario >= WAIVER_THRESHOLD, na.rm = TRUE))

# Combine all scenarios
all_scenarios <- dplyr::bind_rows(scenarios_baseline, scenarios_recession, scenarios_recovery)

# ---------------------------------------------------------------------------
# 4) Uncertainty intervals (parametric bootstrap on ATT)
# ---------------------------------------------------------------------------
boot_result <- NULL
if (!is.na(did_att) && !is.na(did_se) && did_se > 0) {
  # Load heterogeneity bootstrap info (gamma coefficients + VCOV) if available.
  # This allows county-specific ATT uncertainty (not just proportional scaling),
  # so that county ranks vary across bootstrap draws.
  path_het_boot <- file.path(DIR_OUT_TABLES, "het_boot_info.rds")
  het_boot_info <- if (file.exists(path_het_boot)) {
    message("Loaded het_boot_info for joint (ATT + gamma) bootstrap")
    readRDS(path_het_boot)
  } else {
    message("No het_boot_info found; bootstrap uses proportional scaling only")
    NULL
  }

  boot_result <- bootstrap_scenarios(
    county_baseline = county_baseline,
    did_att         = did_att,
    did_se          = did_se,
    share_55_64     = county_baseline$share_55_64,
    att_county      = county_baseline$att_county,
    het_boot_info   = het_boot_info,
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
  # Use scenario-specific baseline for computing losses
  bl_col <- if ("baseline_y_raw_scen" %in% names(scen_df)) "baseline_y_raw_scen" else "baseline_y_raw"
  scen_df %>%
    dplyr::mutate(.bl_raw = .data[[bl_col]]) %>%
    dplyr::summarise(
      scenario            = label,
      n_counties          = dplyr::n(),
      mean_delta_log      = mean(delta_log, na.rm = TRUE),
      mean_pct_change     = mean(pct_change_log, na.rm = TRUE),
      sum_baseline_y_raw  = sum(.bl_raw, na.rm = TRUE),
      sum_obba_y_raw      = sum(obba_y_raw_approx, na.rm = TRUE),
      total_loss_approx   = sum(obba_y_raw_approx - .bl_raw, na.rm = TRUE),
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
