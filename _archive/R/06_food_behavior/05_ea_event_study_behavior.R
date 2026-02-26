# R/06_food_behavior/05_ea_event_study_behavior.R
# Michigan-only EA end analysis (statewide simultaneous shock)
# Uses interrupted time-series (ITS): level shift + slope change after EA end.
# Input:  data/derived/panel_food_behavior_weekly.rds
# Output: outputs/tables/food_behavior_ea_its_results.csv
#         outputs/tables/food_behavior_ea_its_weekly_means.csv
#         outputs/figures/food_behavior_ea_its_grocery_vs_fastfood.png

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)

path_panel <- file.path(DIR_DERIVED, "panel_food_behavior_weekly.rds")
stopifnot(file.exists(path_panel))
if (!dir.exists(DIR_OUT_TABLES)) dir.create(DIR_OUT_TABLES, recursive = TRUE)
if (!dir.exists(DIR_OUT_FIGURES)) dir.create(DIR_OUT_FIGURES, recursive = TRUE)

ea_end_date <- as.Date("2023-03-01")

panel <- readRDS(path_panel) %>%
  dplyr::mutate(
    week_start_date = as.Date(week_start_date),
    event_week = as.integer((week_start_date - ea_end_date) / 7),
    post = as.integer(week_start_date >= ea_end_date),
    t = as.integer(week_start_date - min(week_start_date, na.rm = TRUE)),
    post_t = post * t,
    week_of_year = as.integer(strftime(week_start_date, format = "%V")),
    l_grocery_visits = log1p(as.numeric(grocery_visits)),
    l_fast_food_visits = log1p(as.numeric(fast_food_visits))
  ) %>%
  dplyr::filter(
    week_start_date >= as.Date("2021-01-01"),
    week_start_date <= as.Date("2025-12-31"),
    event_week >= -104,
    event_week <= 104
  )

run_its <- function(df, y_col, outcome) {
  d <- df %>% dplyr::filter(!is.na(.data[[y_col]]), !is.na(county_fips), !is.na(week_of_year))

  # county FE + seasonal FE; statewide post and slope-change are identified in ITS framework.
  mod <- fixest::feols(
    as.formula(sprintf("%s ~ post + t + post_t | county_fips + week_of_year", y_col)),
    data = d,
    cluster = ~ county_fips
  )

  ct <- as.data.frame(summary(mod)$coeftable)
  ct$term <- rownames(ct)

  keep <- ct %>%
    dplyr::filter(term %in% c("post", "t", "post_t")) %>%
    dplyr::transmute(
      outcome = outcome,
      parameter = term,
      estimate = Estimate,
      std_error = `Std. Error`,
      t_value = `t value`,
      p_value = `Pr(>|t|)`,
      conf_low = estimate - 1.96 * std_error,
      conf_high = estimate + 1.96 * std_error,
      n_obs = stats::nobs(mod)
    )

  keep
}

res <- dplyr::bind_rows(
  run_its(panel, "l_grocery_visits", "grocery"),
  run_its(panel, "l_fast_food_visits", "fast_food")
)

# Simple weekly means by event time for visualization
means <- panel %>%
  dplyr::group_by(event_week) %>%
  dplyr::summarise(
    grocery_mean = mean(l_grocery_visits, na.rm = TRUE),
    fast_food_mean = mean(l_fast_food_visits, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::filter(event_week >= -52, event_week <= 52) %>%
  tidyr::pivot_longer(
    cols = c(grocery_mean, fast_food_mean),
    names_to = "outcome",
    values_to = "value"
  ) %>%
  dplyr::mutate(outcome = dplyr::recode(outcome, grocery_mean = "grocery", fast_food_mean = "fast_food"))

readr::write_csv(res, file.path(DIR_OUT_TABLES, "food_behavior_ea_its_results.csv"))
readr::write_csv(means, file.path(DIR_OUT_TABLES, "food_behavior_ea_its_weekly_means.csv"))

p <- ggplot2::ggplot(means, ggplot2::aes(x = event_week, y = value, color = outcome)) +
  ggplot2::geom_vline(xintercept = -1, linetype = "dotted", color = "gray40") +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::scale_color_manual(values = c(grocery = "#1b9e77", fast_food = "#d95f02")) +
  ggplot2::labs(
    title = "EA End (Mar 2023): Weekly Means Around Policy Date (Michigan)",
    subtitle = "ITS model estimated with county FE + week-of-year FE",
    x = "Event week (0 = week containing 2023-03-01)",
    y = "Mean log(1 + visits)",
    color = NULL
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), legend.position = "top")

ggplot2::ggsave(
  filename = file.path(DIR_OUT_FIGURES, "food_behavior_ea_its_grocery_vs_fastfood.png"),
  plot = p,
  width = 9,
  height = 5.5,
  dpi = 160
)

message("Wrote outputs/tables/food_behavior_ea_its_results.csv")
message("Wrote outputs/tables/food_behavior_ea_its_weekly_means.csv")
message("Wrote outputs/figures/food_behavior_ea_its_grocery_vs_fastfood.png")
