# scripts/data_diagnosis.R — diagnose data issues behind significance problems
# Run: Rscript scripts/data_diagnosis.R

source("config/paths.R", local = TRUE)
suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
})

cat("\n========================================\n")
cat("  DATA DIAGNOSIS FOR SIGNIFICANCE ISSUES\n")
cat("========================================\n\n")

# Load panel
panel <- readRDS(file.path(DIR_DERIVED, "panel_analysis.rds"))

cat("Panel dimensions: ", nrow(panel), " rows x ", ncol(panel), " cols\n")
cat("Counties: ", length(unique(panel$id)), "\n")
cat("Date range: ", as.character(min(panel$date, na.rm = TRUE)), " to ",
    as.character(max(panel$date, na.rm = TRUE)), "\n\n")

# ============================================================
# DIAGNOSIS 1: Treatment Timing (G values)
# ============================================================
cat("== DIAGNOSIS 1: TREATMENT TIMING (G values) ==\n")

g_table <- panel %>%
  distinct(id, G, G_int) %>%
  arrange(G)

cat("Unique G values:\n")
print(table(g_table$G, useNA = "always"))
cat("\n")

cat("Treatment cohort distribution:\n")
g_summary <- g_table %>%
  group_by(G) %>%
  summarise(n_counties = n(), .groups = "drop") %>%
  arrange(G)
print(as.data.frame(g_summary))
cat("\n")

n_treated <- sum(g_table$G != "0" & !is.na(g_table$G))
n_never   <- sum(g_table$G == "0" | is.na(g_table$G))
cat("Treated counties: ", n_treated, "\n")
cat("Never-treated (control): ", n_never, "\n\n")

# ============================================================
# DIAGNOSIS 2: County Characteristics — Missing Values
# ============================================================
cat("== DIAGNOSIS 2: MODERATOR MISSING VALUES ==\n")

mod_cols <- c("unemployment_rate", "poverty_rate", "bachelor_share", "svi_total", "urban_dummy", "population_18_49")
for (col in mod_cols) {
  if (col %in% names(panel)) {
    n_na <- sum(is.na(panel[[col]]))
    n_total <- nrow(panel)
    pct_na <- round(100 * n_na / n_total, 2)
    # Check county-level
    county_na <- panel %>%
      group_by(id) %>%
      summarise(all_na = all(is.na(.data[[col]])), .groups = "drop")
    n_county_allna <- sum(county_na$all_na)
    cat(sprintf("  %-20s: %d/%d NA (%.1f%%), %d counties entirely NA\n",
                col, n_na, n_total, pct_na, n_county_allna))
  } else {
    cat(sprintf("  %-20s: COLUMN NOT FOUND\n", col))
  }
}
cat("\n")

# ============================================================
# DIAGNOSIS 3: Pre-period County Averages & Correlation Matrix
# ============================================================
cat("== DIAGNOSIS 3: PRE-PERIOD COUNTY CHARACTERISTICS ==\n")

pre_end <- as.Date("2016-12-31")
start_date <- as.Date("2014-01-01")

pre <- panel %>%
  filter(date >= start_date, date <= pre_end)

county_chars <- pre %>%
  group_by(id) %>%
  summarise(
    avg_unemp    = mean(as.numeric(unemployment_rate), na.rm = TRUE),
    avg_y        = mean(as.numeric(y_per1k_18_49), na.rm = TRUE),
    avg_poverty  = if ("poverty_rate" %in% names(.)) mean(as.numeric(poverty_rate), na.rm = TRUE) else NA_real_,
    avg_bachelor = if ("bachelor_share" %in% names(.)) mean(as.numeric(bachelor_share), na.rm = TRUE) else NA_real_,
    avg_svi      = if ("svi_total" %in% names(.)) mean(as.numeric(svi_total), na.rm = TRUE) else NA_real_,
    is_urban     = if ("urban_dummy" %in% names(.)) as.integer(round(mean(as.numeric(urban_dummy), na.rm = TRUE))) else NA_integer_,
    .groups = "drop"
  )

cat("County chars summary (n=", nrow(county_chars), "):\n")
num_cols <- c("avg_unemp", "avg_y", "avg_poverty", "avg_bachelor", "avg_svi")
for (col in num_cols) {
  if (col %in% names(county_chars) && !all(is.na(county_chars[[col]]))) {
    vals <- county_chars[[col]][!is.na(county_chars[[col]])]
    cat(sprintf("  %-15s: n=%d, mean=%.3f, sd=%.3f, min=%.3f, med=%.3f, max=%.3f\n",
                col, length(vals), mean(vals), sd(vals), min(vals), median(vals), max(vals)))
  }
}
if ("is_urban" %in% names(county_chars)) {
  cat(sprintf("  is_urban: urban=%d, rural=%d, NA=%d\n",
              sum(county_chars$is_urban == 1, na.rm = TRUE),
              sum(county_chars$is_urban == 0, na.rm = TRUE),
              sum(is.na(county_chars$is_urban))))
}
cat("\n")

# Correlation matrix
cat("== CORRELATION MATRIX (county characteristics) ==\n")
char_matrix <- county_chars %>%
  select(all_of(intersect(num_cols, names(county_chars)))) %>%
  filter(complete.cases(.))
if (nrow(char_matrix) >= 10) {
  cor_mat <- round(cor(char_matrix, use = "pairwise.complete.obs"), 3)
  print(cor_mat)
} else {
  cat("  Not enough complete cases for correlation matrix (n=", nrow(char_matrix), ")\n")
}
cat("\n")

# ============================================================
# DIAGNOSIS 4: Subgroup Sizes & G Variation within Splits
# ============================================================
cat("== DIAGNOSIS 4: SUBGROUP G-DISTRIBUTION ==\n")

# Merge G into county_chars
county_g <- panel %>%
  distinct(id, G, G_int)

county_chars_g <- county_chars %>%
  left_join(county_g, by = "id")

split_dims <- list(
  list(var = "avg_unemp",    label = "unemployment", type = "median"),
  list(var = "avg_y",        label = "baseline_snap", type = "median"),
  list(var = "avg_poverty",  label = "poverty_rate",  type = "median"),
  list(var = "avg_bachelor", label = "bachelor_share", type = "median"),
  list(var = "avg_svi",      label = "svi",           type = "median"),
  list(var = "is_urban",     label = "urbanicity",    type = "binary")
)

for (dim in split_dims) {
  var_col <- dim$var
  dim_label <- dim$label

  if (!var_col %in% names(county_chars_g) || all(is.na(county_chars_g[[var_col]]))) {
    cat(sprintf("\n--- %s: VARIABLE NOT AVAILABLE ---\n", dim_label))
    next
  }

  chars_valid <- county_chars_g %>% filter(!is.na(.data[[var_col]]))

  if (dim$type == "median") {
    cutoff <- median(chars_valid[[var_col]], na.rm = TRUE)
    chars_valid <- chars_valid %>%
      mutate(split_group = ifelse(.data[[var_col]] > cutoff, "high", "low"))
    high_label <- "high"
    low_label <- "low"
  } else {
    cutoff <- NA
    chars_valid <- chars_valid %>%
      mutate(split_group = ifelse(.data[[var_col]] == 1, "high", "low"))
    high_label <- "urban"
    low_label <- "rural"
  }

  cat(sprintf("\n--- %s (cutoff=%s) ---\n", dim_label,
              ifelse(is.na(cutoff), "binary", round(cutoff, 3))))

  for (grp in c("high", "low")) {
    grp_data <- chars_valid %>% filter(split_group == grp)
    n_counties <- nrow(grp_data)
    g_dist <- table(grp_data$G, useNA = "always")
    n_treated <- sum(grp_data$G != "0" & !is.na(grp_data$G))
    n_control <- sum(grp_data$G == "0" | is.na(grp_data$G))
    n_unique_g <- length(unique(grp_data$G[grp_data$G != "0" & !is.na(grp_data$G)]))

    grp_label <- ifelse(grp == "high", high_label, low_label)
    cat(sprintf("  %s: %d counties (treated=%d, control=%d, unique_G=%d)\n",
                grp_label, n_counties, n_treated, n_control, n_unique_g))
    cat("    G distribution: ")
    for (i in seq_along(g_dist)) {
      cat(sprintf("%s=%d ", names(g_dist)[i], g_dist[i]))
    }
    cat("\n")

    # Panel obs
    panel_sub <- panel %>%
      filter(id %in% grp_data$id, date >= as.Date("2014-01-01"), date <= as.Date("2019-12-01"))
    cat(sprintf("    Panel obs: %d rows\n", nrow(panel_sub)))
  }
}

# ============================================================
# DIAGNOSIS 5: ACS Age Share Data (share_55_64)
# ============================================================
cat("\n== DIAGNOSIS 5: ACS AGE SHARES (share_55_64) ==\n")

path_age_shares <- file.path(ROOT, "data", "external", "acs_age_shares_MI.csv")
if (file.exists(path_age_shares)) {
  age_data <- readr::read_csv(path_age_shares, show_col_types = FALSE)
  cat("File exists: data/external/acs_age_shares_MI.csv\n")
  cat("  Rows:", nrow(age_data), " Cols:", ncol(age_data), "\n")
  cat("  Column names:", paste(names(age_data), collapse = ", "), "\n")
  if ("share_55_64" %in% names(age_data)) {
    cat("  share_55_64 summary:\n")
    print(summary(age_data$share_55_64))
    cat("  Unique values:", length(unique(age_data$share_55_64)), "\n")
  }
} else {
  cat("FILE NOT FOUND: data/external/acs_age_shares_MI.csv\n")
  cat("  -> This is why all counties have share_55_64 = 0.15 (state average fallback)\n")
  cat("  -> All counties get identical delta_log, identical ranks\n")
}

# Also check forecast data
fc <- readRDS(file.path(DIR_DERIVED, "forecast_scenarios.rds"))
cat("\nForecast scenario data check:\n")
scen <- fc$scenarios_baseline
if (!is.null(scen)) {
  cat("  share_55_64 values: unique =", length(unique(scen$share_55_64)),
      " min =", min(scen$share_55_64, na.rm = TRUE),
      " max =", max(scen$share_55_64, na.rm = TRUE), "\n")
  cat("  delta_log values: unique =", length(unique(scen$delta_log)),
      " min =", round(min(scen$delta_log, na.rm = TRUE), 6),
      " max =", round(max(scen$delta_log, na.rm = TRUE), 6), "\n")
}

# ============================================================
# DIAGNOSIS 6: Interaction Model Data Quality
# ============================================================
cat("\n== DIAGNOSIS 6: INTERACTION MODEL INPUTS ==\n")

source("R/00_utils/helpers_did.R", local = TRUE)
df <- build_analysis_df(panel, "y_per1k_18_49", as.Date("2014-01-01"), as.Date("2019-12-01"))

cat("Analysis df: ", nrow(df), " rows, ", length(unique(df$id_num)), " counties\n")
cat("G_int distribution:\n")
print(table(df$G_int[!duplicated(paste0(df$id, df$G_int))]))

# Check post indicator
df$post <- as.integer(df$G_int > 0 & df$t >= df$G_int)
cat("\nPost indicator: ", sum(df$post), " treated obs, ", sum(!df$post), " control obs\n")
cat("Pct treated: ", round(100 * mean(df$post), 2), "%\n\n")

# Standardized county chars
std_chars <- county_chars %>%
  mutate(across(
    where(is.numeric),
    ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE),
    .names = "z_{.col}"
  )) %>%
  select(id, starts_with("z_"))

df2 <- df %>% left_join(std_chars, by = "id")
z_vars <- grep("^z_", names(df2), value = TRUE)
cat("Z-vars available:", paste(z_vars, collapse = ", "), "\n")
for (zv in z_vars) {
  n_na <- sum(is.na(df2[[zv]]))
  cat(sprintf("  %s: NA=%d (%.1f%%)\n", zv, n_na, 100 * n_na / nrow(df2)))
}

# Check VIF (multicollinearity)
cat("\n== VIF (Variance Inflation Factors) ==\n")
z_complete <- df2 %>%
  select(all_of(z_vars)) %>%
  filter(complete.cases(.))
if (nrow(z_complete) > 100 && length(z_vars) >= 2) {
  vif_model <- lm(as.formula(paste("z_avg_unemp ~", paste(z_vars[-1], collapse = " + "))),
                  data = z_complete)
  cat("  R² for z_avg_unemp ~ others:", round(summary(vif_model)$r.squared, 3), "\n")

  # Simple pairwise correlations among z-vars
  cor_z <- round(cor(z_complete, use = "pairwise.complete.obs"), 3)
  cat("\nPairwise correlations among z-vars:\n")
  print(cor_z)
}

# ============================================================
# DIAGNOSIS 7: ACS data file inspection
# ============================================================
cat("\n== DIAGNOSIS 7: ACS DATA FILE ==\n")
path_acs <- file.path(DIR_DATA_CLEAN, "acs_county_year_MI.csv")
if (file.exists(path_acs)) {
  acs <- readr::read_csv(path_acs, show_col_types = FALSE)
  cat("ACS file: ", nrow(acs), " rows x ", ncol(acs), " cols\n")
  cat("Columns:", paste(names(acs), collapse = ", "), "\n")
  cat("Years:", paste(sort(unique(acs$year)), collapse = ", "), "\n")
  cat("Counties:", length(unique(acs$county_id)), "\n")

  # Check for share_55_64 column
  age_cols <- grep("55|64|age|share", names(acs), value = TRUE, ignore.case = TRUE)
  if (length(age_cols) > 0) {
    cat("Age-related columns found:", paste(age_cols, collapse = ", "), "\n")
  } else {
    cat("NO age-share columns found in ACS data\n")
  }

  # Check key moderator columns
  for (col in c("poverty_rate", "bachelor_share")) {
    if (col %in% names(acs)) {
      vals <- acs[[col]][!is.na(acs[[col]])]
      cat(sprintf("  %s: n=%d non-NA, mean=%.3f, sd=%.3f\n", col, length(vals), mean(vals), sd(vals)))
    }
  }
} else {
  cat("ACS file not found at: ", path_acs, "\n")
}

cat("\n========================================\n")
cat("  END DATA DIAGNOSIS\n")
cat("========================================\n")
