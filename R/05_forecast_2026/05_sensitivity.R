# R/05_forecast_2026/05_sensitivity.R
# Purpose: Sensitivity analysis for projection calibration parameters.
#   Varies compliance elasticity and SNAP-unemployment elasticity across a grid,
#   showing how aggregate projected losses change under recession/recovery scenarios.
#   The baseline scenario is invariant to these parameters (no unemployment shift).
# Input:  data/derived/forecast_baseline.rds, config/globals.R,
#         outputs/tables/abawd_county_att_hat.csv (optional)
# Output: outputs/tables/sensitivity_grid.csv
#         outputs/figures/sensitivity_heatmap.png

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)
if (!exists("OBBA_EFFECTIVE_DATE")) source("config/globals.R", local = TRUE)

path_baseline <- file.path(DIR_DERIVED, "forecast_baseline.rds")
stopifnot(file.exists(path_baseline))
baseline <- readRDS(path_baseline)
county_baseline <- baseline$county_baseline
did_att <- baseline$did_att
did_se  <- baseline$did_se

if (!dir.exists(DIR_OUT_TABLES))  dir.create(DIR_OUT_TABLES,  recursive = TRUE)
if (!dir.exists(DIR_OUT_FIGURES)) dir.create(DIR_OUT_FIGURES, recursive = TRUE)

# ---------------------------------------------------------------------------
# 0) Load county-specific ATT and age shares (same as 02_scenarios.R)
# ---------------------------------------------------------------------------
path_county_att <- file.path(DIR_OUT_TABLES, "abawd_county_att_hat.csv")
if (file.exists(path_county_att)) {
  county_att <- readr::read_csv(path_county_att, show_col_types = FALSE)
  county_att$id <- as.character(county_att$id)
  county_baseline$id <- as.character(county_baseline$id)
  county_baseline <- county_baseline %>%
    dplyr::left_join(county_att %>% dplyr::select(id, att_hat), by = "id")
  county_baseline$att_county <- dplyr::if_else(
    !is.na(county_baseline$att_hat), county_baseline$att_hat, did_att
  )
} else {
  county_baseline$att_county <- did_att
}

STATE_AVG_SHARE_55_64 <- 0.15
path_age_shares <- file.path(ROOT, "data", "external", "acs_age_shares_MI.csv")
if (file.exists(path_age_shares)) {
  age_shares <- readr::read_csv(path_age_shares, show_col_types = FALSE)
  age_shares$id <- as.character(age_shares$id)
  county_baseline$id <- as.character(county_baseline$id)
  county_baseline <- county_baseline %>%
    dplyr::left_join(age_shares %>% dplyr::select(id, share_55_64), by = "id")
  county_baseline$share_55_64[is.na(county_baseline$share_55_64)] <- STATE_AVG_SHARE_55_64
} else {
  county_baseline$share_55_64 <- STATE_AVG_SHARE_55_64
}

# ---------------------------------------------------------------------------
# 1) Define parameter grid
# ---------------------------------------------------------------------------
# Compliance elasticity: % increase in ATT magnitude per 1 pp unemployment increase
# Baseline calibration: 0.08 (8% per 1 pp)
# Range: 0 (no scaling) to 0.16 (double the baseline)
compliance_grid <- c(0.00, 0.04, 0.08, 0.12, 0.16)

# SNAP-unemployment semi-elasticity: log-point SNAP increase per 1 pp unemployment
# Baseline calibration: 0.033 (from stabilizer analysis)
# Range: 0.015 (half) to 0.060 (nearly double)
snap_unemp_grid <- c(0.015, 0.025, 0.033, 0.045, 0.060)

# Unemployment shifts (same as 02_scenarios.R)
RECESSION_UNEMP_SHIFT <- 3.0
RECOVERY_UNEMP_SHIFT  <- -2.0
WAIVER_THRESHOLD      <- 10.0

# ---------------------------------------------------------------------------
# 2) Run scenario for each parameter combination
# ---------------------------------------------------------------------------
compute_aggregate_loss <- function(county_bl, unemp_shift,
                                   compliance_elast, snap_unemp_elast) {
  # Build scenario-adjusted data
  scen <- county_bl %>%
    dplyr::mutate(
      unemp_scenario = unemp_last + unemp_shift,

      # (a) Compliance channel
      compliance_mult = 1 + compliance_elast * unemp_shift,
      att_scenario = att_county * compliance_mult,

      # (b) Enrollment channel
      baseline_outcome_log_scenario = baseline_outcome_log +
        snap_unemp_elast * unemp_shift,

      # (c) Waiver channel
      share_55_64_scenario = dplyr::if_else(
        !is.na(unemp_scenario) & unemp_scenario >= WAIVER_THRESHOLD,
        share_55_64 * 0.3,
        share_55_64
      ),

      # Compute projected outcome
      delta_log = att_scenario * share_55_64_scenario,
      obba_outcome_log = baseline_outcome_log_scenario + delta_log,
      obba_y_per1k     = expm1(obba_outcome_log),
      baseline_y_per1k_scen = expm1(baseline_outcome_log_scenario),
      obba_y_raw_approx = ifelse(!is.na(pop_18_49) & pop_18_49 > 0,
                                  obba_y_per1k * pop_18_49 / 1000, NA_real_),
      baseline_y_raw_scen = ifelse(!is.na(pop_18_49) & pop_18_49 > 0,
                                   baseline_y_per1k_scen * pop_18_49 / 1000, NA_real_)
    )

  total_loss <- sum(scen$obba_y_raw_approx - scen$baseline_y_raw_scen, na.rm = TRUE)
  mean_delta <- mean(scen$delta_log, na.rm = TRUE)
  mean_pct   <- mean(100 * scen$delta_log, na.rm = TRUE)

  data.frame(
    total_loss = total_loss,
    mean_delta_log = mean_delta,
    mean_pct_change = mean_pct
  )
}

# Build grid
grid <- expand.grid(
  compliance_elasticity = compliance_grid,
  snap_unemp_elasticity = snap_unemp_grid,
  stringsAsFactors = FALSE
)

message("Running sensitivity grid: ", nrow(grid), " parameter combinations x 2 scenarios")

# Recession scenario
recession_results <- lapply(seq_len(nrow(grid)), function(i) {
  res <- compute_aggregate_loss(
    county_baseline,
    RECESSION_UNEMP_SHIFT,
    grid$compliance_elasticity[i],
    grid$snap_unemp_elasticity[i]
  )
  data.frame(
    scenario = "recession",
    compliance_elasticity = grid$compliance_elasticity[i],
    snap_unemp_elasticity = grid$snap_unemp_elasticity[i],
    total_loss = res$total_loss,
    mean_delta_log = res$mean_delta_log,
    mean_pct_change = res$mean_pct_change
  )
})

# Recovery scenario
recovery_results <- lapply(seq_len(nrow(grid)), function(i) {
  res <- compute_aggregate_loss(
    county_baseline,
    RECOVERY_UNEMP_SHIFT,
    grid$compliance_elasticity[i],
    grid$snap_unemp_elasticity[i]
  )
  data.frame(
    scenario = "recovery",
    compliance_elasticity = grid$compliance_elasticity[i],
    snap_unemp_elasticity = grid$snap_unemp_elasticity[i],
    total_loss = res$total_loss,
    mean_delta_log = res$mean_delta_log,
    mean_pct_change = res$mean_pct_change
  )
})

# Also compute baseline (invariant to parameters) for reference
baseline_loss <- compute_aggregate_loss(county_baseline, 0, 0, 0)
baseline_row <- data.frame(
  scenario = "baseline",
  compliance_elasticity = NA_real_,
  snap_unemp_elasticity = NA_real_,
  total_loss = baseline_loss$total_loss,
  mean_delta_log = baseline_loss$mean_delta_log,
  mean_pct_change = baseline_loss$mean_pct_change
)

sensitivity_grid <- dplyr::bind_rows(
  baseline_row,
  dplyr::bind_rows(recession_results),
  dplyr::bind_rows(recovery_results)
)

# Write output
out_csv <- file.path(DIR_OUT_TABLES, "sensitivity_grid.csv")
readr::write_csv(sensitivity_grid, out_csv)
message("Wrote ", out_csv, " (", nrow(sensitivity_grid), " rows)")

# ---------------------------------------------------------------------------
# 3) Summary: range across grid
# ---------------------------------------------------------------------------
rec_range <- sensitivity_grid %>%
  dplyr::filter(scenario == "recession")
recov_range <- sensitivity_grid %>%
  dplyr::filter(scenario == "recovery")

message("\n=== Sensitivity Analysis Summary ===")
message("Baseline (invariant): ", round(baseline_loss$total_loss, 0))
message("Recession range: [", round(min(rec_range$total_loss), 0), ", ",
        round(max(rec_range$total_loss), 0), "]")
message("  At baseline calibration (0.08, 0.033): ",
        round(rec_range$total_loss[rec_range$compliance_elasticity == 0.08 &
                                     rec_range$snap_unemp_elasticity == 0.033], 0))
message("Recovery range:  [", round(min(recov_range$total_loss), 0), ", ",
        round(max(recov_range$total_loss), 0), "]")
message("  At baseline calibration (0.08, 0.033): ",
        round(recov_range$total_loss[recov_range$compliance_elasticity == 0.08 &
                                       recov_range$snap_unemp_elasticity == 0.033], 0))

# ---------------------------------------------------------------------------
# 4) Heatmap figure: recession scenario
# ---------------------------------------------------------------------------
rec_for_plot <- sensitivity_grid %>%
  dplyr::filter(scenario == "recession") %>%
  dplyr::mutate(
    total_loss_abs = abs(total_loss),
    compliance_label = paste0(100 * compliance_elasticity, "%"),
    snap_unemp_label = sprintf("%.3f", snap_unemp_elasticity),
    is_baseline_calib = (compliance_elasticity == 0.08 & snap_unemp_elasticity == 0.033)
  )

p_heat <- ggplot2::ggplot(rec_for_plot,
                           ggplot2::aes(x = compliance_label,
                                        y = snap_unemp_label,
                                        fill = total_loss_abs)) +
  ggplot2::geom_tile(color = "white", linewidth = 0.8) +
  ggplot2::geom_text(ggplot2::aes(label = format(round(total_loss), big.mark = ",")),
                     size = 3.5, color = "white", fontface = "bold") +
  # Mark baseline calibration
  ggplot2::geom_tile(data = rec_for_plot %>% dplyr::filter(is_baseline_calib),
                     color = "gold", linewidth = 2, fill = NA) +
  ggplot2::scale_fill_distiller(palette = "YlOrRd", direction = 1,
                                 name = "Projected\nloss") +
  ggplot2::labs(
    title = "Sensitivity of Recession Scenario to Calibration Parameters",
    subtitle = "Gold border = baseline calibration (8%, 0.033)",
    x = "Compliance elasticity (% ATT increase per 1 pp unemployment)",
    y = "SNAP-unemployment semi-elasticity (log pts per 1 pp)"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    panel.grid = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(size = 10)
  )

ggplot2::ggsave(file.path(DIR_OUT_FIGURES, "sensitivity_heatmap.png"),
                p_heat, width = 8, height = 6, dpi = 300)
message("Wrote outputs/figures/sensitivity_heatmap.png")

# ---------------------------------------------------------------------------
# 5) One-at-a-time sensitivity (for paper text: holding other param at baseline)
# ---------------------------------------------------------------------------
# Fix snap_unemp at 0.033, vary compliance
oat_compliance <- sensitivity_grid %>%
  dplyr::filter(scenario == "recession", snap_unemp_elasticity == 0.033) %>%
  dplyr::arrange(compliance_elasticity)

# Fix compliance at 0.08, vary snap_unemp
oat_snap <- sensitivity_grid %>%
  dplyr::filter(scenario == "recession", compliance_elasticity == 0.08) %>%
  dplyr::arrange(snap_unemp_elasticity)

message("\n--- One-at-a-time: Compliance elasticity (SNAP-unemp fixed at 0.033) ---")
for (i in seq_len(nrow(oat_compliance))) {
  message(sprintf("  %.0f%%: %s individuals",
                  100 * oat_compliance$compliance_elasticity[i],
                  format(round(oat_compliance$total_loss[i]), big.mark = ",")))
}

message("\n--- One-at-a-time: SNAP-unemp elasticity (Compliance fixed at 8%) ---")
for (i in seq_len(nrow(oat_snap))) {
  message(sprintf("  %.3f: %s individuals",
                  oat_snap$snap_unemp_elasticity[i],
                  format(round(oat_snap$total_loss[i]), big.mark = ",")))
}

# Write one-at-a-time summary for easy paper reference
oat_summary <- dplyr::bind_rows(
  oat_compliance %>% dplyr::mutate(varied_param = "compliance_elasticity",
                                     varied_value = compliance_elasticity,
                                     fixed_param = "snap_unemp_elasticity",
                                     fixed_value = 0.033),
  oat_snap %>% dplyr::mutate(varied_param = "snap_unemp_elasticity",
                               varied_value = snap_unemp_elasticity,
                               fixed_param = "compliance_elasticity",
                               fixed_value = 0.08)
) %>%
  dplyr::select(varied_param, varied_value, fixed_param, fixed_value,
                total_loss, mean_delta_log, mean_pct_change)

readr::write_csv(oat_summary, file.path(DIR_OUT_TABLES, "sensitivity_oat.csv"))
message("Wrote outputs/tables/sensitivity_oat.csv")

message("\nSensitivity analysis complete.")
