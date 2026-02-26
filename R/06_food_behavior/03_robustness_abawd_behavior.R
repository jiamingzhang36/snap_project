# R/06_food_behavior/03_robustness_abawd_behavior.R
# Input:  data/derived/panel_food_behavior_weekly.rds
# Output: outputs/tables/food_behavior_robustness_abawd.csv

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)
sunab <- get("sunab", asNamespace("fixest"))

path_panel <- file.path(DIR_DERIVED, "panel_food_behavior_weekly.rds")
stopifnot(file.exists(path_panel))
if (!dir.exists(DIR_OUT_TABLES)) dir.create(DIR_OUT_TABLES, recursive = TRUE)

panel0 <- readRDS(path_panel) %>%
  dplyr::mutate(
    week_start_date = as.Date(week_start_date),
    g_date = as.Date(g_date)
  ) %>%
  dplyr::filter(week_start_date >= as.Date("2017-01-01"), week_start_date <= as.Date("2019-12-31"))

extract_stats <- function(mod, outcome, spec, term_prefix, pre_lo, pre_hi, post_lo, post_hi) {
  ct <- as.data.frame(summary(mod)$coeftable)
  ct$term <- rownames(ct)
  ev <- ct %>%
    dplyr::filter(grepl(paste0("^", term_prefix, "::"), .data$term)) %>%
    dplyr::mutate(event_time = suppressWarnings(as.integer(sub(paste0("^", term_prefix, "::"), "", .data$term))))

  pre <- ev %>% dplyr::filter(event_time >= pre_lo, event_time <= pre_hi)
  post <- ev %>% dplyr::filter(event_time >= post_lo, event_time <= post_hi)

  data.frame(
    outcome = outcome,
    spec = spec,
    n_obs = stats::nobs(mod),
    pre_mean = mean(pre$Estimate, na.rm = TRUE),
    post_mean = mean(post$Estimate, na.rm = TRUE),
    post_sig_share = mean(post$`Pr(>|t|)` < 0.05, na.rm = TRUE),
    post_minus_pre = mean(post$Estimate, na.rm = TRUE) - mean(pre$Estimate, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

run_weekly <- function(df, y_col, outcome, spec, trend = FALSE, window12 = FALSE) {
  sunab <- get("sunab", asNamespace("fixest"))
  d <- df %>% dplyr::filter(!is.na(.data[[y_col]]), !is.na(county_fips), !is.na(week_start_date))

  if (trend) {
    d <- d %>%
      dplyr::arrange(county_fips, week_start_date) %>%
      dplyr::group_by(county_fips) %>%
      dplyr::mutate(t_index = dplyr::row_number()) %>%
      dplyr::ungroup()
    fml <- as.formula(sprintf("%s ~ sunab(g_date, week_start_date, ref.p = -1) + i(county_fips, t_index) | county_fips + week_start_date", y_col), env = environment())
  } else {
    fml <- as.formula(sprintf("%s ~ sunab(g_date, week_start_date, ref.p = -1) | county_fips + week_start_date", y_col), env = environment())
  }

  mod <- fixest::feols(fml, data = d, cluster = ~ county_fips)
  if (window12) {
    extract_stats(mod, outcome, spec, "week_start_date", -12, -2, 0, 12)
  } else {
    extract_stats(mod, outcome, spec, "week_start_date", -26, -2, 0, 26)
  }
}

# Monthly aggregation for alignment robustness
panel_month <- panel0 %>%
  dplyr::mutate(month_date = as.Date(format(week_start_date, "%Y-%m-01"))) %>%
  dplyr::group_by(county_fips, month_date, g_date) %>%
  dplyr::summarise(
    l_grocery_visits = log1p(sum(grocery_visits, na.rm = TRUE)),
    l_fast_food_visits = log1p(sum(fast_food_visits, na.rm = TRUE)),
    .groups = "drop"
  )

run_monthly <- function(df, y_col, outcome, spec) {
  sunab <- get("sunab", asNamespace("fixest"))
  d <- df %>% dplyr::filter(!is.na(.data[[y_col]]), !is.na(county_fips), !is.na(month_date))
  mod <- fixest::feols(
    as.formula(sprintf("%s ~ sunab(g_date, month_date, ref.p = -1) | county_fips + month_date", y_col), env = environment()),
    data = d,
    cluster = ~ county_fips
  )
  extract_stats(mod, outcome, spec, "month_date", -6, -2, 0, 6)
}

rows <- list(
  run_weekly(panel0, "l_grocery_visits", "grocery", "weekly_baseline"),
  run_weekly(panel0, "l_fast_food_visits", "fast_food", "weekly_baseline"),
  run_weekly(panel0, "l_grocery_visits", "grocery", "weekly_window12", window12 = TRUE),
  run_weekly(panel0, "l_fast_food_visits", "fast_food", "weekly_window12", window12 = TRUE),
  run_weekly(panel0, "l_grocery_visits", "grocery", "weekly_county_trend", trend = TRUE),
  run_weekly(panel0, "l_fast_food_visits", "fast_food", "weekly_county_trend", trend = TRUE),
  run_monthly(panel_month, "l_grocery_visits", "grocery", "monthly_agg"),
  run_monthly(panel_month, "l_fast_food_visits", "fast_food", "monthly_agg")
)

out <- dplyr::bind_rows(rows)
path_out <- file.path(DIR_OUT_TABLES, "food_behavior_robustness_abawd.csv")
readr::write_csv(out, path_out)
message("Wrote ", path_out)
