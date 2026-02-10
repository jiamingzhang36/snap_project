# R/01_build/03_merge_panel.R
# Input:  data/derived/panel_with_laus.rds, data_clean/acs_county_year_MI.csv (or data_clean/panel_with_G.csv fallback)
# Output: data/derived/panel_analysis.rds — full analysis panel (G, y_*, outcome_final, etc.)

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)
source("R/00_utils/helpers_policy.R", local = TRUE)

path_acs   <- file.path(DIR_DATA_CLEAN, "acs_county_year_MI.csv")
path_G     <- file.path(DIR_DATA_CLEAN, "panel_with_G.csv")
Y_MODE     <- "log1p_per1k_18_49"  # default outcome for DID

# Build from panel_with_laus + ACS when possible
panel_laus_path <- file.path(DIR_DERIVED, "panel_with_laus.rds")
if (!file.exists(panel_laus_path)) stop("Run 02_clean_laus first: ", panel_laus_path, " not found.")

panel0 <- readRDS(panel_laus_path)
panel0 <- panel0 %>%
  dplyr::mutate(
    ym_date = lubridate::make_date(.data$year, .data$month, 1L),
    county_id = sprintf("%05d", as.integer(.data$county_id))
  )

# Recipients column (adult_recipients preferred for ABAWD)
recip_col <- if ("adult_recipients" %in% names(panel0)) "adult_recipients" else "fap_recipients"
pop_total_col <- if ("population_total" %in% names(panel0)) "population_total" else NA_character_
pop_18_49_col <- if ("population_18_49" %in% names(panel0)) "population_18_49" else NA_character_
labor_force_col <- if ("labor_force" %in% names(panel0)) "labor_force" else NA_character_

# Merge ACS if not already in panel and file exists
if (!all(c("population_total", "population_18_49") %in% names(panel0)) && file.exists(path_acs)) {
  acs <- readr::read_csv(path_acs, show_col_types = FALSE) %>%
    dplyr::mutate(county_id = sprintf("%05d", as.integer(.data$county_id)))
  acs_lag <- acs %>%
    dplyr::arrange(.data$county_id, .data$year) %>%
    dplyr::group_by(.data$county_id) %>%
    dplyr::mutate(
      poverty_rate_lag12     = dplyr::lag(.data$poverty_rate, 1),
      bachelor_share_lag12  = dplyr::lag(.data$bachelor_share, 1),
      population_total_lag12 = dplyr::lag(.data$population_total, 1),
      population_18_49_lag12 = dplyr::lag(.data$population_18_49, 1)
    ) %>%
    dplyr::ungroup()
  panel0 <- panel0 %>%
    dplyr::left_join(acs_lag, by = c("county_id", "year"))
  panel0 <- panel0 %>%
    dplyr::mutate(
      recipients_per_1k_total = 1000 * .data$fap_recipients / .data$population_total,
      recipients_per_1k_18_49 = 1000 * .data$fap_recipients / .data$population_18_49,
      w_pop_total = .data$population_total,
      w_pop_18_49 = .data$population_18_49
    )
  pop_total_col <- "population_total"
  pop_18_49_col <- "population_18_49"
}

# Derive G from time_limit_proxy (requires policy columns from 02)
if ("time_limit_proxy" %in% names(panel0)) {
  G_table <- derive_G(panel0)
  panel0 <- panel0 %>%
    dplyr::mutate(id = .data$county_id) %>%
    dplyr::left_join(G_table, by = c("id" = "id_key")) %>%
    dplyr::mutate(
      G = dplyr::if_else(is.na(.data$G), "0", .data$G),
      G_int = dplyr::if_else(is.na(.data$G_int), 0L, .data$G_int)
    )
} else {
  panel0 <- panel0 %>% dplyr::mutate(id = .data$county_id, G = "0", G_int = 0L)
}

# Outcomes
panel <- panel0 %>%
  dplyr::mutate(
    date = .data$ym_date,
    y_raw = as.numeric(.data[[recip_col]]),
    y_per1k_total = if (!is.na(pop_total_col)) {
      dplyr::if_else(as.numeric(.data[[pop_total_col]]) > 0,
                     1000 * (.data$y_raw / as.numeric(.data[[pop_total_col]])), NA_real_)
    } else NA_real_,
    y_per1k_18_49 = if (!is.na(pop_18_49_col)) {
      dplyr::if_else(as.numeric(.data[[pop_18_49_col]]) > 0,
                     1000 * (.data$y_raw / as.numeric(.data[[pop_18_49_col]])), NA_real_)
    } else NA_real_,
    y_per100_lf = if (!is.na(labor_force_col) && labor_force_col %in% names(panel0)) {
      dplyr::if_else(as.numeric(.data[[labor_force_col]]) > 0,
                     100 * (.data$y_raw / as.numeric(.data[[labor_force_col]])), NA_real_)
    } else NA_real_,
    y_log1p_raw = log1p(.data$y_raw),
    y_log1p_per1k_total = log1p(.data$y_per1k_total),
    y_log1p_per1k_18_49 = log1p(.data$y_per1k_18_49),
    y_log1p_per100_lf = log1p(.data$y_per100_lf),
    y_log_raw = dplyr::if_else(!is.na(.data$y_raw) & .data$y_raw > 0, log(.data$y_raw), NA_real_),
    y_log_per1k_total = dplyr::if_else(!is.na(.data$y_per1k_total) & .data$y_per1k_total > 0, log(.data$y_per1k_total), NA_real_),
    y_log_per1k_18_49 = dplyr::if_else(!is.na(.data$y_per1k_18_49) & .data$y_per1k_18_49 > 0, log(.data$y_per1k_18_49), NA_real_),
    y_log_per100_lf = dplyr::if_else(!is.na(.data$y_per100_lf) & .data$y_per100_lf > 0, log(.data$y_per100_lf), NA_real_)
  ) %>%
  dplyr::arrange(.data$id, .data$date)

# Exit-based outcomes
panel <- panel %>%
  dplyr::group_by(.data$id) %>%
  dplyr::arrange(.data$date, .by_group = TRUE) %>%
  dplyr::mutate(
    y_raw_lag1 = dplyr::lag(.data$y_raw, 1),
    y_exit_rate_raw = dplyr::if_else(!is.na(.data$y_raw_lag1) & .data$y_raw_lag1 > 0,
                                     (.data$y_raw_lag1 - .data$y_raw) / .data$y_raw_lag1, NA_real_),
    y_exit_rate = pmax(.data$y_exit_rate_raw, 0, na.rm = TRUE),
    y_exits_count = pmax(.data$y_raw_lag1 - .data$y_raw, 0, na.rm = TRUE),
    y_exit_per1k_18_49 = if ("population_18_49" %in% names(panel)) {
      dplyr::if_else(as.numeric(.data$population_18_49) > 0,
                     1000 * .data$y_exits_count / as.numeric(.data$population_18_49), NA_real_)
    } else NA_real_,
    y_log1p_exit_per1k_18_49 = log1p(.data$y_exit_per1k_18_49)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-"y_exit_rate_raw")

y_map <- c(
  raw = "y_raw", per1k_total = "y_per1k_total", per1k_18_49 = "y_per1k_18_49", per100_lf = "y_per100_lf",
  log_raw = "y_log_raw", log_per1k_total = "y_log_per1k_total", log_per1k_18_49 = "y_log_per1k_18_49", log_per100_lf = "y_log_per100_lf",
  log1p_raw = "y_log1p_raw", log1p_per1k_total = "y_log1p_per1k_total", log1p_per1k_18_49 = "y_log1p_per1k_18_49", log1p_per100_lf = "y_log1p_per100_lf",
  exit_rate = "y_exit_rate", exit_per1k_18_49 = "y_exit_per1k_18_49", log1p_exit_per1k_18_49 = "y_log1p_exit_per1k_18_49"
)
if (!Y_MODE %in% names(y_map)) stop("Y_MODE must be one of: ", paste(names(y_map), collapse = ", "))
panel <- panel %>% dplyr::mutate(outcome_final = .data[[y_map[[Y_MODE]]]])

if (!dir.exists(DIR_DERIVED)) dir.create(DIR_DERIVED, recursive = TRUE)
saveRDS(panel, file.path(DIR_DERIVED, "panel_analysis.rds"))
message("Wrote data/derived/panel_analysis.rds  nrow = ", nrow(panel))
