# R/03_income/02_heterogeneity_unemp.R
# Input:  data/derived/panel_income.rds (from 01: FAP + LAUS, to 2025-12), data/derived/unemp_dl_model.rds
# Output: outputs/tables/income_heterogeneity_unemp.csv
# Heterogeneity: DL effects in high vs low unemployment exposure (median split).

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)

panel <- readRDS(file.path(DIR_DERIVED, "panel_income.rds"))
dl_model <- readRDS(file.path(DIR_DERIVED, "unemp_dl_model.rds"))
LAG_MAX <- dl_model$lag_max
lag_cols <- paste0("unemp_l", 0:LAG_MAX)

if (!dir.exists(DIR_OUT_TABLES)) dir.create(DIR_OUT_TABLES, recursive = TRUE)

# County-level average unemployment (exposure)
county_unemp <- panel %>%
  group_by(id) %>%
  summarise(avg_unemp = mean(unemp, na.rm = TRUE), .groups = "drop") %>%
  filter(is.finite(avg_unemp))
med_unemp <- median(county_unemp$avg_unemp)
county_unemp <- county_unemp %>%
  mutate(high_unemp = as.integer(avg_unemp > med_unemp))

panel <- panel %>%
  left_join(county_unemp %>% select(id, high_unemp), by = "id") %>%
  filter(!is.na(high_unemp)) %>%
  arrange(id, date)

for (k in 0:LAG_MAX) {
  panel <- panel %>%
    group_by(id) %>%
    mutate(!!paste0("unemp_l", k) := dplyr::lag(unemp, k)) %>%
    ungroup()
}
panel_dl <- panel %>% filter(complete.cases(across(all_of(lag_cols))))

form <- as.formula(paste0("Y ~ ", paste(lag_cols, collapse = " + "), " | id_num + date"))
mod_high <- fixest::feols(form, data = panel_dl %>% filter(high_unemp == 1L), cluster = ~ id_num)
mod_low  <- fixest::feols(form, data = panel_dl %>% filter(high_unemp == 0L), cluster = ~ id_num)

b_high <- coef(mod_high)[lag_cols]
b_low  <- coef(mod_low)[lag_cols]
sum_high <- sum(b_high, na.rm = TRUE)
sum_low  <- sum(b_low, na.rm = TRUE)
V_high <- as.matrix(vcov(mod_high))
V_low  <- as.matrix(vcov(mod_low))
w <- rep(1, length(lag_cols))
se_sum_high <- sqrt(drop(t(w) %*% V_high[lag_cols, lag_cols] %*% w))
se_sum_low  <- sqrt(drop(t(w) %*% V_low[lag_cols, lag_cols] %*% w))

het_table <- data.frame(
  group = c("high_unemp", "low_unemp"),
  avg_unemp_cutoff = c(med_unemp, med_unemp),
  sum_dl_effect = c(sum_high, sum_low),
  se = c(se_sum_high, se_sum_low),
  n_obs = c(nrow(panel_dl %>% filter(high_unemp == 1L)), nrow(panel_dl %>% filter(high_unemp == 0L))),
  stringsAsFactors = FALSE
)
readr::write_csv(het_table, file.path(DIR_OUT_TABLES, "income_heterogeneity_unemp.csv"))
message("Wrote outputs/tables/income_heterogeneity_unemp.csv  (high vs low unemp exposure)")
