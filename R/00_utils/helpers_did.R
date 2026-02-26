# R/00_utils/helpers_did.R — build_analysis_df, run_cs_did for DID and subgroup heterogeneity
# Input:  none (defines functions)
# Output: none

build_analysis_df <- function(panel_raw,
                              Y_COL,
                              start_date = as.Date("2014-01-01"),
                              end_date   = as.Date("2019-12-01")) {
  COVARS <- c("unemployment_rate")
  if (!Y_COL %in% names(panel_raw)) {
    available_cols <- grep("^y_", names(panel_raw), value = TRUE)
    stop(sprintf("Column '%s' not found. Available y_*: %s", Y_COL, paste(available_cols, collapse = ", ")))
  }
  required_cols <- c("date", "id", "G", Y_COL, "unemployment_rate", "population_18_49")
  missing_cols <- setdiff(required_cols, names(panel_raw))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing columns: %s", paste(missing_cols, collapse = ", ")))
  }
  df <- panel_raw %>%
    dplyr::mutate(
      date   = as.Date(date),
      t      = lubridate::year(date) * 12L + lubridate::month(date),
      id_num = as.integer(factor(id)),
      G_int = dplyr::case_when(
        is.na(G) ~ 0L,
        G == "0" ~ 0L,
        TRUE ~ {
          g_chr <- as.character(G)
          yy   <- as.integer(substr(g_chr, 1, 4))
          mm   <- as.integer(substr(g_chr, 6, 7))
          ifelse(is.na(yy) | is.na(mm), 0L, yy * 12L + mm)
        }
      ),
      Y = if (Y_COL %in% c("y_per1k_18_49", "y_per1k_total", "y_per100_lf")) {
        log1p(as.numeric(.data[[Y_COL]]))
      } else if (grepl("^y_log", Y_COL, ignore.case = TRUE)) {
        as.numeric(.data[[Y_COL]])
      } else if (Y_COL == "y_raw" && "population_18_49" %in% names(.data)) {
        y_rate <- (as.numeric(.data[[Y_COL]]) / pmax(as.numeric(.data[["population_18_49"]]), 1)) * 1000
        log1p(y_rate)
      } else {
        as.numeric(.data[[Y_COL]])
      },
      unemployment_rate = as.numeric(unemployment_rate),
      population_18_49  = as.numeric(population_18_49)
    ) %>%
    dplyr::filter(date >= start_date, date <= end_date, !is.na(Y)) %>%
    dplyr::filter(complete.cases(dplyr::across(dplyr::all_of(COVARS))))
  df
}

run_cs_did <- function(df, anticipation = 2, control_group = "notyettreated") {
  att <- tryCatch(
    did::att_gt(
      yname = "Y", tname = "t", idname = "id_num", gname = "G_int",
      xformla = ~ unemployment_rate, data = df, panel = TRUE,
      control_group = control_group, est_method = "dr",
      anticipation = anticipation, allow_unbalanced_panel = TRUE
    ),
    error = function(e) {
      if (grepl("singular|design matrix", conditionMessage(e), ignore.case = TRUE)) {
        did::att_gt(
          yname = "Y", tname = "t", idname = "id_num", gname = "G_int",
          xformla = NULL, data = df, panel = TRUE,
          control_group = control_group, est_method = "dr",
          anticipation = anticipation, allow_unbalanced_panel = TRUE
        )
      } else stop(e)
    }
  )
  list(
    att = att,
    overall = did::aggte(att, type = "simple"),
    es = did::aggte(att, type = "dynamic", cband = TRUE, balance_e = 6, min_e = -6, max_e = 6)
  )
}
