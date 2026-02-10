# R/05_forecast_2026/03_outputs_maps.R
# Purpose: Figures and maps for 2026 OBBA policy impact (participation change, newly affected 55–64).
# Input:  data/derived/forecast_scenarios.rds
# Output: outputs/figures/forecast_2026_scenarios.png, outputs/tables/forecast_2026_summary.csv

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)

path_scenarios <- file.path(DIR_DERIVED, "forecast_scenarios.rds")
path_fig       <- file.path(DIR_OUT_FIGURES, "forecast_2026_scenarios.png")
path_summary   <- file.path(DIR_OUT_TABLES, "forecast_2026_summary.csv")

if (!file.exists(path_scenarios)) {
  message("05_forecast_2026/03: forecast_scenarios.rds not found. Run 02_scenarios.R first.")
  quit(save = "no", status = 0)
}

fc <- readRDS(path_scenarios)
scenarios <- fc$scenarios

if (!dir.exists(DIR_OUT_FIGURES)) dir.create(DIR_OUT_FIGURES, recursive = TRUE)
if (!dir.exists(DIR_OUT_TABLES))  dir.create(DIR_OUT_TABLES, recursive = TRUE)

# Summary table: state-level and by-county stats
summary_state <- scenarios %>%
  dplyr::summarise(
    n_counties   = dplyr::n(),
    mean_delta_log = mean(.data$delta_log, na.rm = TRUE),
    mean_pct_change = mean(.data$pct_change_log, na.rm = TRUE),
    sum_baseline_y_raw = sum(.data$baseline_y_raw, na.rm = TRUE),
    sum_obba_y_raw_approx = sum(.data$obba_y_raw_approx, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(level = "state")
readr::write_csv(summary_state, path_summary)
message("Wrote ", path_summary)

# Figure: (1) Bar plot of counties by predicted % change (log scale); (2) Histogram of delta_log
scenarios_plot <- scenarios %>%
  dplyr::mutate(county_label = paste0(.data$county, " (", .data$id, ")")) %>%
  dplyr::arrange(.data$delta_log) %>%
  dplyr::slice_head(n = 25)

p1 <- ggplot2::ggplot(
  scenarios_plot,
  ggplot2::aes(x = stats::reorder(.data$county_label, .data$delta_log), y = .data$pct_change_log, fill = .data$delta_log)
) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "2026 OBBA policy: predicted % change in SNAP participation (log scale)",
    subtitle = "Top 25 counties by impact; 55–64 newly subject to work requirement",
    x = NULL, y = "% change (log scale)"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none")

p2 <- ggplot2::ggplot(scenarios, ggplot2::aes(x = .data$delta_log)) +
  ggplot2::geom_histogram(ggplot2::aes(y = after_stat(density)), bins = 20, fill = "steelblue", alpha = 0.7) +
  ggplot2::geom_vline(xintercept = 0, linetype = 2) +
  ggplot2::labs(
    title = "Distribution of OBBA 2026 impact by county",
    x = "Delta (log outcome)", y = "Density"
  ) +
  ggplot2::theme_minimal()

# Two panels in one figure (patchwork preferred, else gridExtra, else single plot)
grDevices::png(path_fig, width = 10, height = 8, units = "in", res = 150)
if (requireNamespace("patchwork", quietly = TRUE)) {
  print(p1 / p2 + patchwork::plot_layout(heights = c(1.2, 0.8)))
} else if (requireNamespace("gridExtra", quietly = TRUE)) {
  gridExtra::grid.arrange(p1, p2, ncol = 1, heights = c(1.2, 0.8))
} else {
  print(p1)
}
grDevices::dev.off()
message("Wrote ", path_fig)
