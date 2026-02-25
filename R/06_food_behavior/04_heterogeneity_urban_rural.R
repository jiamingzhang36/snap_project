# R/06_food_behavior/04_heterogeneity_urban_rural.R
# Input:  data/derived/panel_food_behavior_weekly.rds
# Output: outputs/tables/food_behavior_heterogeneity_urban_rural_es.csv
#         outputs/tables/food_behavior_heterogeneity_urban_rural_summary.csv
#         outputs/figures/food_behavior_es_urban_rural.png

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)

path_panel <- file.path(DIR_DERIVED, "panel_food_behavior_weekly.rds")
stopifnot(file.exists(path_panel))
if (!dir.exists(DIR_OUT_TABLES)) dir.create(DIR_OUT_TABLES, recursive = TRUE)
if (!dir.exists(DIR_OUT_FIGURES)) dir.create(DIR_OUT_FIGURES, recursive = TRUE)

panel <- readRDS(path_panel) %>%
  dplyr::mutate(
    week_start_date = as.Date(week_start_date),
    g_date = as.Date(g_date)
  ) %>%
  dplyr::filter(week_start_date >= as.Date("2017-01-01"), week_start_date <= as.Date("2019-12-31"))

if (!"urban_dummy" %in% names(panel)) {
  stop("urban_dummy not found in panel_food_behavior_weekly.rds. Re-run 01_build_behavior_panel.R")
}

# county-level urban/rural label (stable within county)
county_ur <- panel %>%
  dplyr::group_by(county_fips) %>%
  dplyr::summarise(urban_dummy = as.numeric(stats::median(urban_dummy, na.rm = TRUE)), .groups = "drop") %>%
  dplyr::mutate(
    urban_group = dplyr::case_when(
      is.na(urban_dummy) ~ NA_character_,
      urban_dummy >= 0.5 ~ "urban",
      TRUE ~ "rural"
    )
  )

panel <- panel %>%
  dplyr::select(-dplyr::any_of(c("urban_group"))) %>%
  dplyr::left_join(county_ur %>% dplyr::select(county_fips, urban_group), by = "county_fips") %>%
  dplyr::filter(!is.na(urban_group))

run_group_sunab <- function(df, y_col, outcome, group_label) {
  sunab <- get("sunab", asNamespace("fixest"))
  d <- df %>%
    dplyr::filter(urban_group == group_label, !is.na(.data[[y_col]]), !is.na(county_fips), !is.na(week_start_date))

  mod <- fixest::feols(
    as.formula(sprintf("%s ~ sunab(g_date, week_start_date, ref.p = -1) | county_fips + week_start_date", y_col), env = environment()),
    data = d,
    cluster = ~ county_fips
  )

  ct <- as.data.frame(summary(mod)$coeftable)
  ct$term <- rownames(ct)
  out <- ct %>%
    dplyr::filter(grepl("^week_start_date::", .data$term)) %>%
    dplyr::mutate(
      event_time = suppressWarnings(as.integer(sub("^week_start_date::", "", .data$term))),
      outcome = outcome,
      urban_group = group_label,
      n_obs = stats::nobs(mod)
    ) %>%
    dplyr::rename(
      estimate = Estimate,
      std_error = `Std. Error`,
      t_value = `t value`,
      p_value = `Pr(>|t|)`
    ) %>%
    dplyr::mutate(
      conf_low = estimate - 1.96 * std_error,
      conf_high = estimate + 1.96 * std_error
    )

  out
}

es <- dplyr::bind_rows(
  run_group_sunab(panel, "l_grocery_visits", "grocery", "urban"),
  run_group_sunab(panel, "l_grocery_visits", "grocery", "rural"),
  run_group_sunab(panel, "l_fast_food_visits", "fast_food", "urban"),
  run_group_sunab(panel, "l_fast_food_visits", "fast_food", "rural")
) %>%
  dplyr::filter(!is.na(event_time), event_time >= -26, event_time <= 26)

path_es <- file.path(DIR_OUT_TABLES, "food_behavior_heterogeneity_urban_rural_es.csv")
readr::write_csv(es, path_es)

summary_tbl <- es %>%
  dplyr::group_by(outcome, urban_group) %>%
  dplyr::summarise(
    pre_mean = mean(estimate[event_time >= -26 & event_time <= -2], na.rm = TRUE),
    post_mean = mean(estimate[event_time >= 0 & event_time <= 26], na.rm = TRUE),
    post_minus_pre = post_mean - pre_mean,
    post_sig_share = mean(p_value[event_time >= 0 & event_time <= 26] < 0.05, na.rm = TRUE),
    n_obs = max(n_obs),
    .groups = "drop"
  )

# urban-rural gap by outcome
gap <- summary_tbl %>%
  dplyr::select(outcome, urban_group, post_minus_pre) %>%
  tidyr::pivot_wider(names_from = urban_group, values_from = post_minus_pre) %>%
  dplyr::mutate(urban_minus_rural = urban - rural)

summary_out <- summary_tbl %>%
  dplyr::left_join(gap %>% dplyr::select(outcome, urban_minus_rural), by = "outcome")

path_sum <- file.path(DIR_OUT_TABLES, "food_behavior_heterogeneity_urban_rural_summary.csv")
readr::write_csv(summary_out, path_sum)

plot_df <- es %>%
  dplyr::mutate(series = paste0(outcome, "_", urban_group))

p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = event_time, y = estimate, color = urban_group, fill = urban_group)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  ggplot2::geom_vline(xintercept = -1, linetype = "dotted", color = "gray40") +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = conf_low, ymax = conf_high), alpha = 0.12, color = NA) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::facet_wrap(~ outcome, ncol = 1) +
  ggplot2::scale_color_manual(values = c(urban = "#1b9e77", rural = "#7570b3")) +
  ggplot2::scale_fill_manual(values = c(urban = "#1b9e77", rural = "#7570b3")) +
  ggplot2::labs(
    title = "ABAWD Event Study Heterogeneity: Urban vs Rural",
    x = "Event time (weeks, ref = -1)",
    y = "Effect on log(1 + visits)",
    color = NULL,
    fill = NULL
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), legend.position = "top")

ggplot2::ggsave(
  filename = file.path(DIR_OUT_FIGURES, "food_behavior_es_urban_rural.png"),
  plot = p,
  width = 9,
  height = 7,
  dpi = 160
)

message("Wrote ", path_es)
message("Wrote ", path_sum)
message("Wrote outputs/figures/food_behavior_es_urban_rural.png")
