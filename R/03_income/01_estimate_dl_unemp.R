# R/03_income/01_estimate_dl_unemp.R
# Input:  data_clean/fap_panel_derived.csv; data_clean/fap_laus_merged_MI.csv (LAUS)
# Output: outputs/tables/income_dl_main.csv, data/derived/unemp_dl_model.rds, data/derived/panel_income.rds
# Distributed-lag: SNAP outcome on *change* in unemployment (du_yoy = 12-mo change; main). Optional du_mom for robustness.
#   Y_ct = α_c + γ_t + Σ_{ℓ=0}^{L} β_ℓ Δu_{c,t-ℓ} + ε_ct  => β_ℓ = impulse response to unemployment shock.

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)

PATH_FAP   <- file.path(DIR_DATA_CLEAN, "fap_panel_derived.csv")
PATH_LAUS  <- file.path(DIR_DATA_CLEAN, "fap_laus_merged_MI.csv")
START_DATE <- as.Date("2014-01-01")
END_DATE   <- as.Date("2025-12-01")
OUTCOME_FAP <- "ln_recipients"
# Baseline window for heterogeneity (02): pre-determined exposure, e.g. 2014-2016
BASELINE_YEARS <- 2014:2016

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

if (file.exists(PATH_LAUS)) {
  laus <- readr::read_csv(PATH_LAUS, show_col_types = FALSE) %>%
    mutate(laus_date = as.Date(paste(year, month, "01", sep = "-"))) %>%
    select(county, year, month, laus_date, unemployment_rate)
  panel <- fap %>%
    left_join(laus %>% rename(unemp = unemployment_rate), by = c("county" = "county", "year" = "year", "month" = "month"))
} else {
  panel <- fap %>% mutate(unemp = NA_real_)
}

panel <- panel %>%
  filter(date >= START_DATE, date <= END_DATE) %>%
  mutate(
    id_num = as.integer(factor(id)),
    Y = as.numeric(Y_raw),
    unemp = as.numeric(unemp)
  ) %>%
  filter(!is.na(Y), is.finite(Y), !is.na(unemp), is.finite(unemp)) %>%
  arrange(id, date)

# Unemployment *change*: YoY (main) and MoM (robustness)
panel <- panel %>%
  group_by(id) %>%
  mutate(
    du_yoy = unemp - dplyr::lag(unemp, 12),
    du_mom = unemp - dplyr::lag(unemp, 1)
  ) %>%
  ungroup()

# Lags of du_yoy for DL (need 12 months to get du_yoy, then 0..LAG_MAX lags)
LAG_MAX <- 6L
for (k in 0:LAG_MAX) {
  panel <- panel %>%
    group_by(id) %>%
    mutate(!!paste0("du_yoy_l", k) := dplyr::lag(du_yoy, k)) %>%
    ungroup()
}
lag_cols <- paste0("du_yoy_l", 0:LAG_MAX)
panel_dl <- panel %>%
  filter(complete.cases(across(all_of(lag_cols))))

# Regression: Y ~ du_yoy_l0 + ... + du_yoy_l6 | id_num + date
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

if (!dir.exists(DIR_OUT_TABLES)) dir.create(DIR_OUT_TABLES, recursive = TRUE)
readr::write_csv(dl_table, file.path(DIR_OUT_TABLES, "income_dl_main.csv"))
message("Wrote outputs/tables/income_dl_main.csv  (DL on du_yoy, lags 0-", LAG_MAX, ")")

unemp_dl_model <- list(
  model = mod,
  coefs = dl_table,
  lag_max = LAG_MAX,
  n_obs = nrow(panel_dl),
  formula = form,
  start_date = START_DATE,
  end_date = END_DATE,
  shock_var = "du_yoy",
  note = "DL on 12-month change in unemployment (YoY); sum(b) = cumulative effect of 1pp shock"
)
if (!dir.exists(DIR_DERIVED)) dir.create(DIR_DERIVED, recursive = TRUE)
saveRDS(unemp_dl_model, file.path(DIR_DERIVED, "unemp_dl_model.rds"))
message("Wrote data/derived/unemp_dl_model.rds")

# Save panel for 02: include unemp, du_yoy, and enough to compute baseline exposure (year)
panel_save <- panel %>%
  select(id, date, year, month, Y, unemp, du_yoy, id_num)
saveRDS(panel_save, file.path(DIR_DERIVED, "panel_income.rds"))
message("Wrote data/derived/panel_income.rds  nrow = ", nrow(panel), "  date range: ", min(panel$date), " to ", max(panel$date))
