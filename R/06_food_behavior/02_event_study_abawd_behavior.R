# R/06_food_behavior/02_event_study_abawd_behavior.R
# Input:  data/derived/panel_food_behavior_weekly.rds
# Output: outputs/tables/food_behavior_event_study_abawd.csv
#         outputs/figures/food_behavior_es_grocery_vs_fastfood.png

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)

path_panel <- file.path(DIR_DERIVED, "panel_food_behavior_weekly.rds")
stopifnot(file.exists(path_panel))
if (!dir.exists(DIR_OUT_TABLES)) dir.create(DIR_OUT_TABLES, recursive = TRUE)
if (!dir.exists(DIR_OUT_FIGURES)) dir.create(DIR_OUT_FIGURES, recursive = TRUE)

panel <- readRDS(path_panel) %>%
  dplyr::mutate(
    week_start_date = as.Date(week_start_date),
    treated = !is.na(g_date),
    g_date = as.Date(g_date)
  ) %>%
  dplyr::filter(week_start_date >= as.Date("2017-01-01"), week_start_date <= as.Date("2019-12-31"))

run_sunab <- function(df, y_col, label) {
  d <- df %>%
    dplyr::filter(!is.na(.data[[y_col]]), !is.na(county_fips), !is.na(week_start_date))

  mod <- fixest::feols(
    as.formula(sprintf("%s ~ sunab(g_date, week_start_date, ref.p = -1) | county_fips + week_start_date", y_col)),
    data = d,
    cluster = ~ county_fips
  )

  ct <- as.data.frame(summary(mod)$coeftable)
  ct$term <- rownames(ct)
  ct <- ct %>%
    dplyr::filter(grepl("^week_start_date::", .data$term)) %>%
    dplyr::mutate(
      event_time = suppressWarnings(as.integer(sub("^week_start_date::", "", .data$term))),
      outcome = label
    )
  ct
}

es_grocery <- run_sunab(panel, "l_grocery_visits", "grocery")
es_fastfood <- run_sunab(panel, "l_fast_food_visits", "fast_food")

es <- dplyr::bind_rows(es_grocery, es_fastfood) %>%
  dplyr::filter(!is.na(event_time), event_time >= -26, event_time <= 26) %>%
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

path_out <- file.path(DIR_OUT_TABLES, "food_behavior_event_study_abawd.csv")
readr::write_csv(es, path_out)

p <- ggplot2::ggplot(es, ggplot2::aes(x = event_time, y = estimate, color = outcome, fill = outcome)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  ggplot2::geom_vline(xintercept = -1, linetype = "dotted", color = "gray40") +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = conf_low, ymax = conf_high), alpha = 0.15, color = NA) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::geom_point(size = 1.2) +
  ggplot2::scale_color_manual(values = c(grocery = "#1b9e77", fast_food = "#d95f02")) +
  ggplot2::scale_fill_manual(values = c(grocery = "#1b9e77", fast_food = "#d95f02")) +
  ggplot2::labs(
    title = "ABAWD Event Study: Grocery vs Fast Food Visits",
    x = "Event time (weeks, ref = -1)",
    y = "Effect on log(1 + visits)",
    color = NULL,
    fill = NULL
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), legend.position = "top")

ggplot2::ggsave(
  filename = file.path(DIR_OUT_FIGURES, "food_behavior_es_grocery_vs_fastfood.png"),
  plot = p,
  width = 9,
  height = 5.5,
  dpi = 160
)

message("Wrote ", path_out)
message("Wrote outputs/figures/food_behavior_es_grocery_vs_fastfood.png")
