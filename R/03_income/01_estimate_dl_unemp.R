# R/03_income/01_estimate_dl_unemp.R
# Input:  data_clean/fap_panel_derived.csv (new extracted, to 2025 Dec); data_clean/fap_laus_merged_MI.csv (LAUS for unemp)
# Output: outputs/tables/income_dl_main.csv, data/derived/unemp_dl_model.rds, data/derived/panel_income.rds
# Distributed-lag: SNAP outcome on current and lagged unemployment.
# Time window: 2014-01-01 to 2025-12-01. No LAUS beyond 2022 in this project, so effective sample is 2014–2022; when LAUS is updated to 2025, rerun to use full window.

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)

# Data: new extracted FAP panel to 2025 Dec; merge LAUS for unemployment
PATH_FAP   <- file.path(DIR_DATA_CLEAN, "fap_panel_derived.csv")
PATH_LAUS  <- file.path(DIR_DATA_CLEAN, "fap_laus_merged_MI.csv")
START_DATE <- as.Date("2014-01-01")
END_DATE   <- as.Date("2025-12-01")
# Outcome: log(1+recipients) or ln_recipients from FAP panel (no per-1k in fap_panel_derived)
OUTCOME_FAP <- "ln_recipients"   # or use log1p(recipients) if column missing

stopifnot(file.exists(PATH_FAP))
fap <- readr::read_csv(PATH_FAP, show_col_types = FALSE)
fap <- fap %>%
  mutate(
    date = as.Date(paste(year, month, "01", sep = "-")),
    id = as.character(county),
    recipients = as.numeric(recipients)
  )
if (!OUTCOME_FAP %in% names(fap)) {
  fap$Y_raw <- log1p(fap$recipients)
} else {
  fap$Y_raw <- as.numeric(fap[[OUTCOME_FAP]])
}

# Merge LAUS (unemployment). Use fap_laus_merged_MI; extend with other LAUS if needed for 2023-2025
if (file.exists(PATH_LAUS)) {
  laus <- readr::read_csv(PATH_LAUS, show_col_types = FALSE) %>%
    mutate(laus_date = as.Date(paste(year, month, "01", sep = "-"))) %>%
    select(county, year, month, laus_date, unemployment_rate)
  panel <- fap %>%
    left_join(laus %>% rename(unemp = unemployment_rate), by = c("county" = "county", "year" = "year", "month" = "month"))
} else {
  panel <- fap %>% mutate(unemp = NA_real_)
}

d_start <- as.Date("2014-01-01")
d_end   <- as.Date("2025-12-01")
panel <- panel %>%
  filter(date >= d_start, date <= d_end) %>%
  mutate(
    id_num = as.integer(factor(id)),
    Y = as.numeric(Y_raw),
    unemp = as.numeric(unemp)
  ) %>%
  filter(!is.na(Y), is.finite(Y), !is.na(unemp), is.finite(unemp)) %>%
  arrange(id, date)

# Save merged panel for 02 (heterogeneity)
saveRDS(panel %>% select(id, date, year, month, Y, unemp, id_num), file.path(DIR_DERIVED, "panel_income.rds"))
message("Wrote data/derived/panel_income.rds  nrow = ", nrow(panel), "  date range: ", min(panel$date), " to ", max(panel$date))

if (!dir.exists(DIR_OUT_TABLES)) dir.create(DIR_OUT_TABLES, recursive = TRUE)

# Lags of unemployment (by county): 0 to LAG_MAX
LAG_MAX <- 6L
for (k in 0:LAG_MAX) {
  panel <- panel %>%
    group_by(id) %>%
    mutate(!!paste0("unemp_l", k) := dplyr::lag(unemp, k)) %>%
    ungroup()
}

lag_cols <- paste0("unemp_l", 0:LAG_MAX)
panel_dl <- panel %>%
  filter(complete.cases(across(all_of(lag_cols))))

# Regression: Y ~ unemp_l0 + ... + unemp_l6 | id + date (FE)
form <- as.formula(paste0("Y ~ ", paste(lag_cols, collapse = " + "), " | id_num + date"))
mod <- fixest::feols(form, data = panel_dl, cluster = ~ id_num)

sm <- summary(mod)
ct <- sm$coeftable
lag_idx <- 0:LAG_MAX
dl_table <- data.frame(
  lag = lag_idx,
  estimate = as.numeric(coef(mod)[lag_cols]),
  se = as.numeric(ct[lag_cols, "Std. Error"]),
  stringsAsFactors = FALSE
) %>%
  mutate(
    ci_lower = estimate - 1.96 * se,
    ci_upper = estimate + 1.96 * se
  )

readr::write_csv(dl_table, file.path(DIR_OUT_TABLES, "income_dl_main.csv"))
message("Wrote outputs/tables/income_dl_main.csv  (DL lags 0-", LAG_MAX, ")")

unemp_dl_model <- list(
  model = mod,
  coefs = dl_table,
  lag_max = LAG_MAX,
  n_obs = nrow(panel_dl),
  formula = form,
  start_date = START_DATE,
  end_date = END_DATE
)
saveRDS(unemp_dl_model, file.path(DIR_DERIVED, "unemp_dl_model.rds"))
message("Wrote data/derived/unemp_dl_model.rds")
