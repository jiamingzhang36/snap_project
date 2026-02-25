# R/06_food_behavior/06_unemployment_dl_behavior.R
# Input:  data/derived/panel_food_behavior_weekly.rds
# Output: outputs/tables/food_behavior_unemp_dl_main.csv
#         outputs/tables/food_behavior_unemp_dl_cumulative.csv
#         data/derived/food_behavior_unemp_dl_models.rds

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)

path_panel <- file.path(DIR_DERIVED, "panel_food_behavior_weekly.rds")
stopifnot(file.exists(path_panel))
if (!dir.exists(DIR_OUT_TABLES)) dir.create(DIR_OUT_TABLES, recursive = TRUE)
if (!dir.exists(DIR_DERIVED)) dir.create(DIR_DERIVED, recursive = TRUE)

START_DATE <- as.Date("2017-01-01")
END_DATE   <- as.Date("2025-12-31")
LAG_MAX <- 6L

wk <- readRDS(path_panel) %>%
  dplyr::mutate(
    week_start_date = as.Date(week_start_date),
    ym_date = as.Date(format(week_start_date, "%Y-%m-01"))
  ) %>%
  dplyr::filter(week_start_date >= START_DATE, week_start_date <= END_DATE)

# Aggregate to county-month to align with monthly unemployment variation
m <- wk %>%
  dplyr::group_by(county_fips, ym_date) %>%
  dplyr::summarise(
    grocery_visits = sum(grocery_visits, na.rm = TRUE),
    fast_food_visits = sum(fast_food_visits, na.rm = TRUE),
    convenience_visits = sum(convenience_visits, na.rm = TRUE),
    visits_total = sum(visits_total, na.rm = TRUE),
    unemployment_rate = dplyr::first(stats::na.omit(unemployment_rate)),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    id = as.character(county_fips),
    date = as.Date(ym_date),
    id_num = as.integer(factor(id)),
    y_log_total = log1p(as.numeric(visits_total)),
    y_log_grocery = log1p(as.numeric(grocery_visits)),
    y_log_fastfood = log1p(as.numeric(fast_food_visits)),
    y_log_convenience = log1p(as.numeric(convenience_visits)),
    fastfood_share = dplyr::if_else(visits_total > 0, as.numeric(fast_food_visits) / as.numeric(visits_total), NA_real_),
    convenience_share = dplyr::if_else(visits_total > 0, as.numeric(convenience_visits) / as.numeric(visits_total), NA_real_)
  ) %>%
  dplyr::arrange(id, date) %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(
    du_yoy = unemployment_rate - dplyr::lag(unemployment_rate, 12)
  ) %>%
  dplyr::ungroup()

for (k in 0:LAG_MAX) {
  m <- m %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(!!paste0("du_yoy_l", k) := dplyr::lag(du_yoy, k)) %>%
    dplyr::ungroup()
}

lag_cols <- paste0("du_yoy_l", 0:LAG_MAX)
outcomes <- c(
  y_log_total = "total_visits_log1p",
  y_log_grocery = "grocery_visits_log1p",
  y_log_fastfood = "fast_food_visits_log1p",
  y_log_convenience = "convenience_visits_log1p",
  fastfood_share = "fast_food_share",
  convenience_share = "convenience_share"
)

run_one <- function(df, y_col, outcome_label) {
  d <- df %>%
    dplyr::filter(!is.na(.data[[y_col]]), !is.na(unemployment_rate)) %>%
    dplyr::filter(stats::complete.cases(dplyr::across(dplyr::all_of(lag_cols))))

  fml <- as.formula(paste0(y_col, " ~ ", paste(lag_cols, collapse = " + "), " | id_num + date"))
  mod <- fixest::feols(fml, data = d, cluster = ~ id_num)

  ct <- summary(mod)$coeftable
  coef_vec <- coef(mod)

  tab <- data.frame(
    outcome = outcome_label,
    lag = 0:LAG_MAX,
    term = lag_cols,
    estimate = as.numeric(coef_vec[lag_cols]),
    se = as.numeric(ct[lag_cols, "Std. Error"]),
    n_obs = nrow(d),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(
      ci_lower = estimate - 1.96 * se,
      ci_upper = estimate + 1.96 * se,
      p_value = 2 * stats::pnorm(abs(estimate / se), lower.tail = FALSE)
    )

  cum_eff <- sum(tab$estimate, na.rm = TRUE)
  cum_se <- sqrt(sum(tab$se^2, na.rm = TRUE))
  cum <- data.frame(
    outcome = outcome_label,
    lag_max = LAG_MAX,
    cumulative_effect = cum_eff,
    cumulative_se = cum_se,
    cumulative_ci_lower = cum_eff - 1.96 * cum_se,
    cumulative_ci_upper = cum_eff + 1.96 * cum_se,
    n_obs = nrow(d),
    stringsAsFactors = FALSE
  )

  list(model = mod, table = tab, cumulative = cum)
}

res_list <- lapply(names(outcomes), function(y) run_one(m, y, outcomes[[y]]))
main_tab <- dplyr::bind_rows(lapply(res_list, `[[`, "table"))
cum_tab <- dplyr::bind_rows(lapply(res_list, `[[`, "cumulative"))

readr::write_csv(main_tab, file.path(DIR_OUT_TABLES, "food_behavior_unemp_dl_main.csv"))
readr::write_csv(cum_tab, file.path(DIR_OUT_TABLES, "food_behavior_unemp_dl_cumulative.csv"))

saveRDS(
  list(
    models = lapply(res_list, `[[`, "model"),
    table = main_tab,
    cumulative = cum_tab,
    lag_max = LAG_MAX,
    start_date = START_DATE,
    end_date = END_DATE
  ),
  file.path(DIR_DERIVED, "food_behavior_unemp_dl_models.rds")
)

message("Wrote outputs/tables/food_behavior_unemp_dl_main.csv")
message("Wrote outputs/tables/food_behavior_unemp_dl_cumulative.csv")
message("Wrote data/derived/food_behavior_unemp_dl_models.rds")
