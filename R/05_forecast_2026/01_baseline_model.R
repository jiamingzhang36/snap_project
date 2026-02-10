# R/05_forecast_2026/01_baseline_model.R
# Purpose: Baseline under current ABAWD rules (18–54). Used to compare vs 2026 OBBA in 02_scenarios.
# Input:  data/derived/panel_analysis.rds, outputs/step1_did/did_summary_results_final.csv, data/derived/unemp_dl_model.rds
# Output: data/derived/forecast_baseline.rds, outputs/tables/forecast_backtest.csv

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)

path_panel   <- file.path(DIR_DERIVED, "panel_analysis.rds")
path_did     <- file.path(ROOT, "outputs", "step1_did", "did_summary_results_final.csv")
path_dl      <- file.path(DIR_DERIVED, "unemp_dl_model.rds")
out_rds      <- file.path(DIR_DERIVED, "forecast_baseline.rds")
out_backtest <- file.path(DIR_OUT_TABLES, "forecast_backtest.csv")

stopifnot(file.exists(path_panel))
panel <- readRDS(path_panel)
panel <- panel %>%
  dplyr::mutate(
    date = as.Date(.data$date),
    id   = as.character(.data$id)
  )

# --- 1) DID ATT (for OBBA scenario in 02) ---
did_att <- NA_real_
did_se  <- NA_real_
if (file.exists(path_did)) {
  did_summary <- readr::read_csv(path_did, show_col_types = FALSE)
  main_row    <- did_summary %>% dplyr::filter(.data$spec == "main")
  if (nrow(main_row) > 0) {
    did_att <- main_row$att[1]
    did_se  <- main_row$se[1]
  }
}
message("DID ATT (main): ", round(did_att, 4), " (SE ", round(did_se, 4), ")")

# --- 2) DL model (unemployment → SNAP outcome) ---
dl_model <- NULL
if (file.exists(path_dl)) {
  dl_model <- readRDS(path_dl)
  message("DL model: lags 0-", dl_model$lag_max, ", n_obs = ", dl_model$n_obs)
}

# --- 3) County-level baseline (last 12 months mean of outcome_final and y_raw) ---
last_date <- max(panel$date, na.rm = TRUE)
cutoff    <- last_date - 12 * 30  # ~12 months
panel_tail <- panel %>%
  dplyr::filter(.data$date >= cutoff, .data$date <= last_date, !is.na(.data$outcome_final))

if (!"county" %in% names(panel_tail)) panel_tail$county <- panel_tail$id
county_baseline <- panel_tail %>%
  dplyr::group_by(.data$id, .data$county) %>%
  dplyr::summarise(
    baseline_outcome_log = mean(.data$outcome_final, na.rm = TRUE),
    baseline_y_raw       = mean(.data$y_raw, na.rm = TRUE),
    baseline_y_per1k_18_49 = mean(.data$y_per1k_18_49, na.rm = TRUE),
    n_months             = dplyr::n(),
    last_date            = max(.data$date, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::filter(.data$n_months >= 6)

# Attach last observed unemp and population for 02
last_month <- panel %>%
  dplyr::filter(.data$date == last_date) %>%
  dplyr::select(id, unemp_last = unemployment_rate, pop_18_49 = population_18_49)
county_baseline <- county_baseline %>%
  dplyr::left_join(last_month, by = "id")

# --- 4) Backtest: naive forecast vs actual (holdout last 6 months) ---
holdout_start <- last_date - 6 * 30
train <- panel %>% dplyr::filter(.data$date < holdout_start)
backtest_df <- panel %>%
  dplyr::filter(.data$date >= holdout_start, !is.na(.data$outcome_final)) %>%
  dplyr::left_join(
    train %>%
      dplyr::group_by(.data$id) %>%
      dplyr::summarise(pred_log = mean(.data$outcome_final, na.rm = TRUE), .groups = "drop"),
    by = "id"
  ) %>%
  dplyr::mutate(resid = .data$outcome_final - .data$pred_log)

backtest_metrics <- data.frame(
  metric = c("rmse_log", "mae_log", "n_obs"),
  value  = c(
    sqrt(mean(backtest_df$resid^2, na.rm = TRUE)),
    mean(abs(backtest_df$resid), na.rm = TRUE),
    sum(!is.na(backtest_df$resid))
  ),
  stringsAsFactors = FALSE
)
if (!dir.exists(DIR_OUT_TABLES)) dir.create(DIR_OUT_TABLES, recursive = TRUE)
readr::write_csv(backtest_metrics, out_backtest)
message("Backtest: RMSE(log) = ", round(backtest_metrics$value[1], 4), ", n_obs = ", backtest_metrics$value[3])

# --- 5) Save ---
forecast_baseline <- list(
  did_att         = did_att,
  did_se          = did_se,
  dl_model        = dl_model,
  county_baseline = county_baseline,
  backtest_metrics = backtest_metrics,
  last_date       = last_date,
  created        = Sys.time()
)
if (!dir.exists(DIR_DERIVED)) dir.create(DIR_DERIVED, recursive = TRUE)
saveRDS(forecast_baseline, out_rds)
message("Wrote ", out_rds, " and ", out_backtest)
