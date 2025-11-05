############################################################
## Complete DID Analysis: Data Prep → Estimation → Diagnostics
## ABAWD Policy Impact on SNAP Recipients (Callaway-Sant'Anna)
## CORRECTED VERSION - Based on accurate policy timeline
############################################################

## Load libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(did)
  library(ggplot2)
  library(tibble)
  library(Matrix)
})

## ============================================================
## PART 1: Data Preparation
## ============================================================

## Set paths
in_path  <- "~/Desktop/snap_project/data_clean/snap_laus_with_policy.csv"
out_dir  <- "~/Desktop/snap_project/data_clean"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

## Helper: normalize county names
fix_name <- function(x) {
  x %>%
    str_squish() %>%
    str_to_title() %>%
    str_replace_all("\\bSt\\.?\\s", "St. ") %>%
    str_remove("\\s*County\\b")
}

## Read and clean data
df <- read_csv(in_path, show_col_types = FALSE) %>%
  mutate(county = fix_name(county)) %>%
  filter(county != "X-Unassigned")

## ============================================================
## Define Treatment Groups Based on Waiver Timeline
## ============================================================
##
## Key concept: 
##   - WAIVED counties = ABAWD work requirements NOT enforced (treated = 0)
##   - NON-WAIVED counties = ABAWD work requirements enforced (treated = 1)
##
## Timeline (from policy documents):
##   2016-2017: 4 counties non-waived (Kent, Oakland, Ottawa, Washtenaw)
##   2018 Jan-Jun: 14 counties non-waived (10 more added)
##   2018 Jul-Sep: 13 counties non-waived (Wayne temporarily excluded)
##   2018 Oct-Dec: 14 counties non-waived (Wayne reinstated)
##   2019: 14 counties non-waived (maintained)
##
## ============================================================

## Non-waived counties 2016-2017 (early adopters)
nonwaived_2016_2017 <- fix_name(c(
  "Kent", "Oakland", "Ottawa", "Washtenaw"
))

## Non-waived counties 2018-2019 (expanded list)
nonwaived_2018_all <- fix_name(c(
  "Allegan", "Barry", "Berrien", "Clinton", "Eaton", "Grand Traverse",
  "Ingham", "Ionia", "Kalamazoo", "Kent", "Livingston", "Oakland", 
  "Ottawa", "Washtenaw"
))

## Build treatment indicator
df <- df %>%
  mutate(
    treated = 0L,
    
    # 2016-2017: 4 counties enforce ABAWD
    treated = if_else(year %in% 2016:2017 & county %in% nonwaived_2016_2017, 
                      1L, treated),
    
    # 2018 Jan-Jun: 14 counties enforce ABAWD
    treated = if_else(year == 2018 & month <= 6 & county %in% nonwaived_2018_all,
                      1L, treated),
    
    # 2018 Jul-Sep: 13 counties enforce ABAWD (Wayne temporarily waived)
    treated = if_else(year == 2018 & month %in% 7:9 & 
                        county %in% nonwaived_2018_all & county != "Wayne",
                      1L, treated),
    
    # 2018 Oct-Dec: 14 counties enforce ABAWD (Wayne reinstated)
    treated = if_else(year == 2018 & month >= 10 & county %in% nonwaived_2018_all,
                      1L, treated),
    
    # 2019: 14 counties enforce ABAWD
    treated = if_else(year == 2019 & county %in% nonwaived_2018_all,
                      1L, treated)
  )

## Construct DID variables
df <- df %>%
  mutate(
    t        = as.integer(year * 12 + month),
    id_num   = as.integer(factor(county)),
    Recipients = pmax(0, as.numeric(Recipients)),
    log_recip  = log(Recipients + 1)
  )

## Keep 2016–2019 data only
did_ready <- df %>%
  filter(year %in% 2016:2019) %>%
  select(
    county, id_num, year, month, t, treated,
    Recipients, log_recip, participation_rate,
    labor_force, unemployment_rate, Cases, Payments
  ) %>%
  arrange(county, year, month)

cat("\n✓ Data preparation complete\n")
cat("Rows:", nrow(did_ready), " | Counties:", n_distinct(did_ready$county), "\n")
cat("\nTreatment share by year:\n")
print(
  did_ready %>%
    group_by(year) %>%
    summarise(
      n_treated = sum(treated == 1),
      n_total = n(),
      share_treated = mean(treated == 1),
      .groups = "drop"
    )
)

## Verify treatment assignment for key counties
cat("\nVerification: Treatment status for key counties in 2018:\n")
verification <- did_ready %>%
  filter(year == 2018, county %in% c("Kent", "Wayne", "Genesee")) %>%
  group_by(county, month) %>%
  summarise(treated = first(treated), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = month, values_from = treated, names_prefix = "M")
print(verification)

## Create dense time index and compute first-treatment cohort G
did_ready_dr <- did_ready %>%
  mutate(
    ym      = sprintf("%04d-%02d", year, month),
    t_dense = as.integer(factor(ym, levels = sort(unique(ym))))
  ) %>%
  arrange(id_num, t_dense) %>%
  distinct(id_num, t_dense, .keep_all = TRUE) %>%
  group_by(id_num) %>%
  mutate(
    treated = as.integer(cummax(as.integer(treated))),
    G       = ifelse(any(treated == 1L), min(t_dense[treated == 1L]), 0L)
  ) %>%
  ungroup()

## Final dataset for att_gt
did_ready_cs <- did_ready_dr %>%
  mutate(
    id_num    = as.integer(id_num),
    t_dense   = as.integer(t_dense),
    G         = as.integer(G),
    log_recip = as.numeric(log_recip),
    unemp_z   = as.numeric(scale(unemployment_rate))  # Standardized control
  ) %>%
  filter(is.finite(log_recip), is.finite(t_dense), !is.na(G)) %>%
  as.data.frame()

## Export audit file
write_csv(
  did_ready_cs %>% 
    select(county, year, month, t_dense, treated, G) %>%
    arrange(county, year, month),
  file.path(out_dir, "abawd_tdense_G_treated_corrected.csv")
)

cat("\nCohort distribution (G = first treatment period):\n")
cohort_summary <- did_ready_cs %>%
  group_by(G) %>%
  summarise(
    n_obs = n(),
    n_counties = n_distinct(id_num),
    first_date = first(ym),
    .groups = "drop"
  ) %>%
  arrange(G)
print(cohort_summary)

## ============================================================
## PART 2: Main Estimation with Covariate Adjustment
## ============================================================

cat("\n============================================================\n")
cat("PART 2: ESTIMATION\n")
cat("============================================================\n\n")

## Specify covariate formula (z-scored unemployment rate)
xformla <- ~ unemp_z

## Estimate group-time average treatment effects
att <- att_gt(
  yname  = "log_recip",
  tname  = "t_dense",
  idname = "id_num",
  gname  = "G",
  data   = did_ready_cs,
  panel  = TRUE,
  control_group = "notyettreated",
  clustervars   = "id_num",
  est_method    = "dr",
  xformla       = xformla,
  allow_unbalanced_panel = TRUE
)

## Aggregate to overall ATT
ov <- aggte(att, type = "simple", na.rm = TRUE)

## Aggregate to event-study (with balanced window and simultaneous confidence bands)
es <- aggte(
  att, 
  type = "dynamic",
  balance_e = TRUE,
  min_e = -12,    # 12 months pre-treatment
  max_e = 24,     # 24 months post-treatment
  na.rm = TRUE,
  cband = TRUE    # Simultaneous confidence bands
)

## Display results
cat("\n========== Overall ATT (with unemployment control) ==========\n")
print(summary(ov))
cat(sprintf("\nInterpretation: ATT (log) = %.4f\n", ov$overall.att))
cat(sprintf("   Approximate %% change = %.2f%%\n", 100 * (exp(ov$overall.att) - 1)))
cat(sprintf("   Standard error = %.4f\n", ov$overall.se))
cat(sprintf("   95%% CI: [%.4f, %.4f]\n", 
            ov$overall.att - 1.96 * ov$overall.se,
            ov$overall.att + 1.96 * ov$overall.se))

cat("\n========== Dynamic Effects (Event Study) ==========\n")
print(summary(es))

## ============================================================
## PART 3: Visualization
## ============================================================

cat("\n============================================================\n")
cat("PART 3: VISUALIZATION\n")
cat("============================================================\n\n")

## Extract critical value for simultaneous confidence bands
crit_val <- if (!is.null(es$crit.val.egt)) es$crit.val.egt else qnorm(0.975)

## Prepare event study data frame
es_df <- tibble(
  event_time = es$egt,
  att = es$att.egt,
  se  = es$se.egt
) %>%
  filter(is.finite(event_time), is.finite(att), is.finite(se)) %>%
  mutate(
    lo = att - crit_val * se,
    hi = att + crit_val * se,
    sig = !(lo < 0 & hi > 0),
    period = ifelse(event_time < 0, "Pre-treatment", "Post-treatment")
  )

## Create event study plot
p_event <- ggplot(es_df, aes(x = event_time, y = att, color = period)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4, color = "gray40") +
  geom_vline(xintercept = -0.5, linetype = "dotted", linewidth = 0.4, color = "gray40") +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = period),
              alpha = 0.20, color = NA, show.legend = FALSE) +
  geom_line(linewidth = 0.7) +
  geom_point(aes(shape = sig), size = 2.8) +
  scale_color_manual(
    name = "Period",
    values = c("Pre-treatment" = "steelblue", "Post-treatment" = "darkred")
  ) +
  scale_fill_manual(
    values = c("Pre-treatment" = "steelblue", "Post-treatment" = "darkred")
  ) +
  scale_shape_manual(
    values = c(`TRUE` = 16, `FALSE` = 1), 
    guide = "none"
  ) +
  labs(
    title = "Event Study: ABAWD Policy Impact on SNAP Recipients",
    subtitle = sprintf("Callaway-Sant'Anna DID with unemployment control | Simultaneous 95%% confidence bands (crit=%.2f)", crit_val),
    x = "Months Relative to Policy Implementation (Event Time)",
    y = "Average Treatment Effect on log(Recipients + 1)",
    caption = "Filled points indicate 95% confidence band excludes zero (significant)\nShaded area shows simultaneous confidence bands"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0),
    plot.subtitle = element_text(size = 10, color = "gray50", hjust = 0),
    plot.caption = element_text(size = 9, color = "gray60", hjust = 0),
    legend.position = "top",
    legend.title = element_text(size = 10, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9)
  )

print(p_event)

## Save plot
ggsave(
  file.path(out_dir, "event_study_corrected.png"),
  plot = p_event,
  width = 10, height = 6, dpi = 300
)

## Export results table
results_table <- tibble(
  Event_Time = es_df$event_time,
  ATT = es_df$att,
  Std_Error = es_df$se,
  CI_Lower = es_df$lo,
  CI_Upper = es_df$hi,
  Significant = ifelse(es_df$sig, "Yes", "No"),
  Period = es_df$period
)

write_csv(
  results_table,
  file.path(out_dir, "did_results_corrected.csv")
)

cat("✓ Event study plot saved: event_study_corrected.png\n")
cat("✓ Results table saved: did_results_corrected.csv\n")

## ============================================================
## PART 4: Robustness & Diagnostics
## ============================================================

cat("\n============================================================\n")
cat("PART 4: ROBUSTNESS DIAGNOSTICS\n")
cat("============================================================\n\n")

## ------------------------------------------------------------
## Test 1: Pointwise Pre-Period Effects
## ------------------------------------------------------------
cat("Test 1: Pointwise Pre-Period Effects\n")
cat(strrep("-", 60), "\n")

pre_idx <- which(es$egt < 0 & is.finite(es$att.egt) & is.finite(es$se.egt))

if (length(pre_idx) > 0) {
  pre_tbl <- tibble(
    event_time = es$egt[pre_idx],
    att = es$att.egt[pre_idx],
    se  = es$se.egt[pre_idx]
  ) %>%
    mutate(
      z_stat = att / se,
      p_value = 2 * pnorm(-abs(z_stat)),
      significant = ifelse(p_value < 0.05, "Yes", "No")
    ) %>%
    arrange(event_time)
  
  print(pre_tbl)
  
  n_sig <- sum(pre_tbl$significant == "Yes")
  n_total <- nrow(pre_tbl)
  cat(sprintf("\nSummary: %d out of %d pre-treatment periods significant (%.1f%%)\n", 
              n_sig, n_total, 100 * n_sig / n_total))
  
  if (n_sig == 0) {
    cat("✓ Interpretation: No significant pre-effects → parallel trends supported\n\n")
  } else if (n_sig <= 2) {
    cat("⚠ Interpretation: Few significant pre-effects → parallel trends mostly supported\n\n")
  } else {
    cat("⚠ Interpretation: Multiple significant pre-effects → parallel trends violated\n\n")
  }
} else {
  cat("No pre-treatment periods available\n\n")
}

## ------------------------------------------------------------
## Test 2: Joint Wald Test for Pre-Trends
## ------------------------------------------------------------
cat("Test 2: Joint Pre-Trend Wald Test\n")
cat(strrep("-", 60), "\n")

if (length(pre_idx) > 0) {
  Vfull <- tryCatch(es$V_egt, error = function(e) NULL)
  if (is.null(Vfull)) Vfull <- diag(es$se.egt^2)
  
  V_pre <- Vfull[pre_idx, pre_idx, drop = FALSE]
  att_pre <- matrix(es$att.egt[pre_idx], ncol = 1)
  
  # Ensure positive definite
  V_pre <- as.matrix(Matrix::nearPD(V_pre, corr = FALSE)$mat)
  
  Wald_stat <- tryCatch(
    drop(t(att_pre) %*% solve(V_pre) %*% att_pre),
    error = function(e) NA
  )
  
  if (!is.na(Wald_stat)) {
    df_w <- length(pre_idx)
    p_wald <- 1 - pchisq(Wald_stat, df = df_w)
    
    cat(sprintf("Wald statistic: %.3f\n", Wald_stat))
    cat(sprintf("Degrees of freedom: %d\n", df_w))
    cat(sprintf("p-value: %.4f\n", p_wald))
    
    if (p_wald > 0.05) {
      cat("✓ Interpretation: Fail to reject joint null → parallel trends supported\n\n")
    } else {
      cat("⚠ Interpretation: Reject joint null → parallel trends violated\n\n")
    }
  } else {
    cat("⚠ Unable to compute Wald test (singular covariance matrix)\n\n")
  }
} else {
  cat("No pre-treatment periods for joint test\n\n")
}

## ------------------------------------------------------------
## Test 3: Pre-Period Slope Test
## ------------------------------------------------------------
cat("Test 3: Pre-Period Slope Test (Weighted OLS)\n")
cat(strrep("-", 60), "\n")

if (exists("pre_tbl") && nrow(pre_tbl) > 1) {
  wt <- 1 / (pre_tbl$se^2)
  trend_fit <- lm(att ~ event_time, data = pre_tbl, weights = wt)
  trend_summary <- coef(summary(trend_fit))
  
  slope_est <- trend_summary["event_time", "Estimate"]
  slope_se <- trend_summary["event_time", "Std. Error"]
  slope_p <- trend_summary["event_time", "Pr(>|t|)"]
  
  cat(sprintf("Pre-period slope: %.6f\n", slope_est))
  cat(sprintf("Standard error: %.6f\n", slope_se))
  cat(sprintf("p-value: %.4f\n", slope_p))
  
  if (slope_p > 0.05) {
    cat("✓ Interpretation: No significant linear pre-trend → parallel trends supported\n\n")
  } else {
    cat("⚠ Interpretation: Significant linear pre-trend → parallel trends violated\n\n")
  }
} else {
  cat("Insufficient pre-treatment periods for slope test\n\n")
}

## ------------------------------------------------------------
## Test 4: Anticipation Check (K=1 month)
## ------------------------------------------------------------
cat("Test 4: Anticipation Robustness Check (K=1)\n")
cat(strrep("-", 60), "\n")

att_noanticip <- att_gt(
  yname = "log_recip",
  tname = "t_dense",
  idname = "id_num",
  gname = "G",
  data = did_ready_cs,
  panel = TRUE,
  control_group = "notyettreated",
  clustervars = "id_num",
  est_method = "dr",
  xformla = xformla,
  anticipation = 1,  # Exclude 1 period before treatment
  allow_unbalanced_panel = TRUE
)

ov_noanticip <- aggte(att_noanticip, type = "simple", na.rm = TRUE)

att_main <- ov$overall.att
att_noan <- ov_noanticip$overall.att
att_diff <- att_noan - att_main

cat(sprintf("ATT (main, K=0): %.4f\n", att_main))
cat(sprintf("ATT (K=1 excluded): %.4f\n", att_noan))
cat(sprintf("Difference: %.4f\n", att_diff))

if (abs(att_diff) < 0.01) {
  cat("✓ Interpretation: Minimal change → no anticipation effects\n\n")
} else {
  cat("⚠ Interpretation: Notable change → possible anticipation effects\n\n")
}

## ------------------------------------------------------------
## Test 5: Alternative Estimation Methods
## ------------------------------------------------------------
cat("Test 5: Comparison Across Estimation Methods\n")
cat(strrep("-", 60), "\n")

methods <- c("dr", "ipw", "reg")
method_results <- list()

for (m in methods) {
  att_m <- att_gt(
    yname = "log_recip", 
    tname = "t_dense", 
    idname = "id_num", 
    gname = "G",
    data = did_ready_cs, 
    panel = TRUE, 
    control_group = "notyettreated",
    clustervars = "id_num", 
    est_method = m, 
    xformla = xformla,
    allow_unbalanced_panel = TRUE
  )
  ov_m <- aggte(att_m, type = "simple", na.rm = TRUE)
  method_results[[m]] <- ov_m$overall.att
  cat(sprintf("%-15s: %.4f (SE: %.4f)\n", toupper(m), ov_m$overall.att, ov_m$overall.se))
}

max_diff <- max(abs(unlist(method_results) - method_results[["dr"]]))
cat(sprintf("\nMax difference from DR: %.4f\n", max_diff))

if (max_diff < 0.005) {
  cat("✓ Interpretation: Methods align closely → robust estimates\n\n")
} else {
  cat("⚠ Interpretation: Methods differ → sensitivity to specification\n\n")
}

## ------------------------------------------------------------
## Test 6: Diagnostic Summary Table
## ------------------------------------------------------------
cat("Test 6: Diagnostic Summary\n")
cat(strrep("-", 60), "\n")

diag_summary <- tibble(
  Check = c(
    "Main ATT estimate (DR)",
    "Pre-trend Wald p-value",
    "Pre-trend slope p-value",
    "ATT with K=1 anticipation",
    "IPW method",
    "Regression method",
    "% significant pre-periods"
  ),
  Value = c(
    round(ov$overall.att, 4),
    if (exists("p_wald")) round(p_wald, 4) else NA,
    if (exists("slope_p")) round(slope_p, 4) else NA,
    round(att_noan, 4),
    round(method_results[["ipw"]], 4),
    round(method_results[["reg"]], 4),
    if (exists("pre_tbl")) round(100 * mean(pre_tbl$significant == "Yes"), 1) else NA
  ),
  Assessment = c(
    "—",
    if (exists("p_wald") && p_wald > 0.05) "✓ Pass" else "⚠ Fail",
    if (exists("slope_p") && slope_p > 0.05) "✓ Pass" else "⚠ Fail",
    if (abs(att_diff) < 0.01) "✓ Robust" else "⚠ Sensitive",
    if (abs(method_results[["ipw"]] - att_main) < 0.005) "✓ Aligns" else "⚠ Differs",
    if (abs(method_results[["reg"]] - att_main) < 0.005) "✓ Aligns" else "⚠ Differs",
    if (exists("pre_tbl") && mean(pre_tbl$significant == "Yes") < 0.2) "✓ Good" else "⚠ Concern"
  )
)

print(diag_summary, n = Inf)

## ============================================================
## CONCLUSION
## ============================================================

cat("\n============================================================\n")
cat("CONCLUSION\n")
cat("============================================================\n\n")

cat(sprintf("Main Estimate:\n"))
cat(sprintf("  ATT (log scale) = %.4f (SE = %.4f)\n", ov$overall.att, ov$overall.se))
cat(sprintf("  Approximate %% change in recipients = %.2f%%\n\n", 
            100 * (exp(ov$overall.att) - 1)))

cat("Robustness Assessment:\n")
if (exists("p_wald") && p_wald > 0.05) {
  cat("  ✓ Joint pre-trend test passed\n")
} else {
  cat("  ⚠ Joint pre-trend test failed\n")
}

if (exists("slope_p") && slope_p > 0.05) {
  cat("  ✓ No significant pre-period slope\n")
} else {
  cat("  ⚠ Significant pre-period slope detected\n")
}

if (max_diff < 0.005) {
  cat("  ✓ Estimates consistent across methods\n")
} else {
  cat("  ⚠ Estimates vary across methods\n")
}

if (abs(att_diff) < 0.01) {
  cat("  ✓ No anticipation effects detected\n")
} else {
  cat("  ⚠ Possible anticipation effects\n")
}

cat("\nRecommendations:\n")
cat("  • Report both main estimate and K=1 specification\n")
cat("  • Include event study plot in paper\n")
cat("  • Discuss covariate adjustment (unemployment control)\n")
cat("  • Address any pre-trend concerns in robustness section\n")

cat("\n============================================================\n")
cat("Analysis complete!\n")
cat("============================================================\n")





# ============================================================
# STEP 2: Double Machine Learning + Orthogonal BLP
# ============================================================
# PURPOSE:
#   Estimate county-level treatment effect heterogeneity using:
#   1. Cross-fitted nuisance models (outcome & propensity) with LASSO
#   2. Orthogonal R-learner signals to remove confounding bias
#   3. Best Linear Projection (BLP) of CATE on county characteristics
#   4. Two-way clustered standard errors (county × month)
#
# OUTPUTS:
#   - step2_output.rds: Complete results for Step 3 (coefficients, vcov, ATE, standardization params)
#   - step2_county_effects.csv: County-level treatment effect indices
#   - step2_blp_coefs.csv: BLP regression coefficients with SEs
#
# KEY FEATURES:
#   - NO DATA LEAKAGE: Standardization uses pre-policy baseline only
#   - CROSS-FITTING: K-fold CV prevents overfitting in nuisance models
#   - OVERLAP PROTECTION: Clips propensity scores to [0.05, 0.95]
#   - ATE ESTIMATION: Provides anchor point for Step 3 forecasting
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(glmnet)
  library(fixest)
  library(broom)
})

# -------------------- Command-line Arguments --------------------
# Allows flexible input paths and hyperparameters via command line
# Example: Rscript step2.R input=/path/to/data.csv folds=10 overlap_lower=0.1

arg_map <- list()
if (length(commandArgs(TRUE))) {
  for (kv in strsplit(commandArgs(TRUE), "=", fixed=TRUE)) {
    if (length(kv)==2 && nzchar(kv[[1]])) arg_map[[kv[[1]]]] <- kv[[2]]
  }
}

get_arg <- function(key, default=NULL) {
  if (!is.null(arg_map[[key]]) && nzchar(arg_map[[key]])) return(arg_map[[key]])
  default
}

parse_num <- function(x, default) {
  if (is.null(x) || !nzchar(x)) return(default)
  as.numeric(x)
}

parse_bool <- function(x, default=FALSE) {
  if (is.null(x) || !nzchar(x)) return(default)
  tolower(x) %in% c("1","true","t","yes","y")
}

# Default parameters
input_path       <- path.expand(get_arg("input", "~/Desktop/snap_project/data_clean/snap_laus_with_acs_final_clean.csv.gz"))
K                <- as.integer(parse_num(get_arg("folds", "5"), 5))
overlap_lower    <- parse_num(get_arg("overlap_lower", "0.05"), 0.05)
overlap_upper    <- parse_num(get_arg("overlap_upper", "0.95"), 0.95)
trim_overlap     <- parse_bool(get_arg("trim_overlap", "FALSE"), FALSE)

# -------------------- Load Data --------------------
dt <- fread(input_path)
cat("✅ Loaded data:", nrow(dt), "rows,", ncol(dt), "columns\n")

# -------------------- Identify Key Columns (Flexible Column Names) --------------------
# Automatically detects column names to handle different naming conventions
# This makes the script robust to variations in your dataset structure

first_col <- function(nm, cand) cand[cand %in% nm][1]
nm <- names(dt)

# Year and Month columns (required for time fixed effects)
ycol <- first_col(nm, c("YEAR","year","Year"))
mcol <- first_col(nm, c("MONTH","month","Month"))
stopifnot("Need YEAR column" = !is.na(ycol))
stopifnot("Need MONTH column" = !is.na(mcol))
setnames(dt, c(ycol, mcol), c("YEAR","MONTH"))

# County identifier (required for county fixed effects and clustering)
id_col <- first_col(nm, c("id_num","county_id","countyid","fips","FIPS"))
stopifnot("Need county ID column" = !is.na(id_col))
setnames(dt, id_col, "cid")

# Treatment indicator (binary: 0/1 for policy exposure)
treatc <- first_col(nm, c("Treat","treat","enforced","policy_enforced","wr_in_effect"))
stopifnot("Need treatment indicator" = !is.na(treatc))
setnames(dt, treatc, "Treat")
dt[, Treat := as.integer(Treat > 0)]

# SNAP cases/recipients count
casec <- first_col(nm, c("cases","Cases","recip","recipients"))
stopifnot("Need cases/recipients count" = !is.na(casec))
setnames(dt, casec, "cases")

# Total households (optional, improves outcome measurement)
hhc <- first_col(nm, c("total_households","households","hh_total"))
has_hh <- !is.na(hhc)
if (has_hh) setnames(dt, hhc, "total_households")

# Create year-month identifiers for clustering and time fixed effects
dt[, YM := sprintf("%04d-%02d", YEAR, MONTH)]
dt[, ym := YEAR*100L + MONTH]  # Numeric format for comparisons

# -------------------- Construct Outcome Variable Y --------------------
# We use log transformation for two reasons:
# 1. Stabilizes variance (many counties have very different scales)
# 2. Makes treatment effects interpretable as % changes
# Formula: Y = log(1 + cases_per_1k_hh) or log(1 + cases)

if (has_hh) {
  dt[, cases_per_1k_hh := 1000 * cases / pmax(total_households, 1)]
  dt[, Y := log1p(cases_per_1k_hh)]
  cat("✅ Outcome: Y = log(1 + cases_per_1k_hh)\n")
} else {
  dt[, Y := log1p(cases)]
  cat("⚠️  No total_households found; using Y = log(1 + cases)\n")
}

# -------------------- Define Moderators (County Characteristics) --------------------
# These are the X variables that interact with treatment to create heterogeneity
# They should be:
# - Pre-treatment or slow-moving characteristics
# - Theoretically relevant for program take-up/exit
# - Not affected by the treatment itself (to avoid post-treatment bias)

mods <- c(
  # Demographics
  "pct_male_final",           # Gender composition
  "pct_age_65plus",           # Elderly population share
  "median_age_final",         # Age structure
  
  # Race/Ethnicity
  "pct_black_final",          # Black population share
  "pct_asian_final",          # Asian population share
  "pct_hispanic_final",       # Hispanic population share
  
  # Education & Language
  "pct_bachelors_plus",       # College education rate
  "pct_foreign_born",         # Immigrant share
  "pct_non_english_home",     # Limited English proficiency proxy
  
  # Household Structure
  "pct_married_hh",           # Married household share
  "pct_single_parent",        # Single-parent household share
  
  # Labor Market
  "lfpr",                     # Labor force participation rate
  "emp_rate",                 # Employment rate
  
  # Economic Conditions
  "income_avg",               # Average income
  "pov_le_130fpl_share",      # Poverty rate (≤130% FPL)
  
  # Housing
  "homeownership_rate",       # Homeownership rate
  "vacancy_rate",             # Housing vacancy rate
  "median_home_value",        # Median home value
  "median_rent_final",        # Median rent
  "pct_rent_burdened_final"   # Rent-burdened households (>30% income)
)

# Create derived variables for better model fit
# Log transformations help with skewed distributions
if (!"unemp_rate" %in% names(dt) && "emp_rate" %in% names(dt)) {
  dt[, unemp_rate := 1 - emp_rate]
}
if (!"log_income" %in% names(dt) && "income_avg" %in% names(dt)) {
  dt[, log_income := log1p(income_avg)]
}
if (!"log_rent" %in% names(dt) && "median_rent_final" %in% names(dt)) {
  dt[, log_rent := log1p(median_rent_final)]
}
if (!"log_home" %in% names(dt) && "median_home_value" %in% names(dt)) {
  dt[, log_home := log1p(median_home_value)]
}

# Keep only moderators that exist in the dataset
mods <- mods[mods %in% names(dt)]
stopifnot("Need at least one moderator" = length(mods) > 0)

# Remove redundant variables to avoid perfect collinearity
# emp_rate and unemp_rate are perfectly correlated (unemp = 1 - emp)
if ("emp_rate" %in% mods && "unemp_rate" %in% mods) {
  mods <- setdiff(mods, "unemp_rate")
  cat("ℹ️  Kept emp_rate, removed unemp_rate (avoid perfect collinearity)\n")
}

cat("Using", length(mods), "moderators for heterogeneity analysis\n")

# -------------------- Baseline Standardization (NO DATA LEAKAGE) --------------------
# CRITICAL: We standardize using ONLY pre-policy data to avoid leakage
# 
# Why this matters:
# - If we standardize using the full sample (including post-treatment), 
#   information from treated periods "leaks" into the training process
# - This violates the cross-fitting logic and can bias estimates
#
# Our approach:
# 1. Identify pre-policy period (before any treatment starts)
# 2. Calculate mean & SD using ONLY pre-policy observations
# 3. Apply these fixed parameters to standardize the FULL dataset
# 4. This ensures test folds don't contaminate training folds

cat("\n", strrep("=", 60), "\n")
cat("STANDARDIZATION: Pre-policy baseline (no leakage)\n")
cat(strrep("=", 60), "\n")

# Find when treatment first occurs
policy_start <- dt[Treat==1, suppressWarnings(min(ym, na.rm=TRUE))]

# Define baseline period: all observations before treatment starts
if (is.finite(policy_start)) {
  baseline <- dt[ym < policy_start]
  cat("Policy starts:", policy_start, "\n")
  cat("Baseline period: n =", nrow(baseline), "observations\n")
} else {
  # Fallback: if no clear policy start, use all untreated observations
  baseline <- dt[Treat == 0]
  cat("⚠️  No clear policy start; using Treat==0 as baseline (n =", 
      nrow(baseline), ")\n")
}

# Safety check: ensure we have baseline data
if (nrow(baseline) == 0) {
  stop("❌ No baseline data available for standardization")
}

# Compute baseline statistics (mean and SD) for each moderator
baseline_stats <- list()
for (mod in mods) {
  mu <- mean(baseline[[mod]], na.rm=TRUE)
  sigma <- sd(baseline[[mod]], na.rm=TRUE)
  
  # Handle edge cases
  if (!is.finite(mu)) mu <- 0
  if (!is.finite(sigma) || sigma < 1e-10) sigma <- 1  # Avoid division by zero
  
  baseline_stats[[mod]] <- list(mean=mu, sd=sigma)
}

# Apply standardization to FULL dataset using baseline parameters
# Formula: z = (x - μ_baseline) / σ_baseline
for (mod in mods) {
  mu <- baseline_stats[[mod]]$mean
  sigma <- baseline_stats[[mod]]$sd
  dt[, (mod) := (get(mod) - mu) / sigma]
}

cat("✅ Standardization complete (mean≈0, sd≈1)\n")
cat("First 3 moderators after standardization:\n")
print(dt[1:3, head(mods, 3), with=FALSE])

# -------------------- K-Fold Cross-Fitting Setup --------------------
# Cross-fitting prevents overfitting in nuisance models (m_hat, e_hat)
# 
# Key design choice: FOLD BY COUNTY, not by observation
# - If we fold by observation, the same county appears in both train and test
# - This creates temporal leakage (future predicts past within same county)
# - By folding at county level, we ensure no county appears in both sets
#
# Steps:
# 1. Randomly assign each county to one of K folds
# 2. For fold k: train on counties NOT in fold k, predict for counties IN fold k
# 3. This gives us out-of-sample predictions for ALL observations

set.seed(42)  # Reproducibility
uids <- unique(dt$cid)
fold_ids <- sample(rep(seq_len(K), length.out=length(uids)))
fold_map <- data.table(cid=uids, fold=fold_ids)
dt <- merge(dt, fold_map, by="cid", all.x=TRUE, sort=FALSE)

cat("\n✅ K-fold cross-fitting setup (K =", K, ") by county\n")

# Check fold balance
fold_balance <- dt[, .(
  n_obs = .N,
  n_counties = uniqueN(cid),
  n_treated = sum(Treat),
  pct_treated = 100*mean(Treat)
), by=fold]
cat("\nFold balance:\n")
print(fold_balance)

# -------------------- Cross-Fitted Nuisance Models (LASSO) --------------------
# We need two nuisance models for DML:
# 1. Outcome model: m(X) = E[Y | X] - predicts outcome given covariates
# 2. Propensity model: e(X) = E[D | X] - predicts treatment given covariates
#
# Why LASSO?
# - Handles high-dimensional X (many moderators)
# - Automatic variable selection
# - Prevents overfitting via L1 penalty
# - Cross-validation chooses optimal penalty (lambda)
#
# Cross-fitting procedure:
# For each fold k:
#   1. Train both models on folds ≠ k
#   2. Predict for observations in fold k
#   3. Store out-of-sample predictions (m_hat, e_hat)

# Utility function: Create model matrix and remove constant/non-finite columns
mmx <- function(D, vars){
  X <- as.matrix(D[, ..vars])
  # Keep only columns with variance and finite values
  keep <- which(apply(X, 2, function(z){
    z <- z[is.finite(z)]
    sd(z) > 0 && length(z) > 1
  }))
  if (!length(keep)) stop("All features constant or non-finite")
  X[, keep, drop=FALSE]
}

# Initialize prediction columns
dt[, c("m_hat","e_hat") := .(NA_real_, NA_real_)]

cat("\n", strrep("=", 60), "\n")
cat("CROSS-FITTED LASSO MODELS\n")
cat(strrep("=", 60), "\n")

need <- c(mods, "Y", "Treat")  # Required variables

for (k in seq_len(K)) {
  # Split: train on folds ≠ k, test on fold k
  tr <- which(dt$fold != k & complete.cases(dt[, ..need]))
  te <- which(dt$fold == k & complete.cases(dt[, ..need]))
  if (!length(te)) next  # Skip if test set is empty
  
  cat("\nFold", k, "| Train n =", length(tr), "| Test n =", length(te), "\n")
  
  Tr <- dt[tr]; Te <- dt[te]
  xtr <- mmx(Tr, mods)
  xte <- mmx(Te, mods)
  
  # Ensure train and test have same columns (required by glmnet)
  common <- intersect(colnames(xtr), colnames(xte))
  xtr <- xtr[, common, drop=FALSE]
  xte <- xte[, common, drop=FALSE]
  
  # 1. OUTCOME MODEL: Y ~ X (Gaussian family for continuous outcome)
  cvy <- cv.glmnet(xtr, Tr$Y, family="gaussian", alpha=1, nfolds=5)
  mhat <- as.numeric(predict(cvy, xte, s="lambda.min"))
  
  # 2. PROPENSITY MODEL: D ~ X (Binomial family for binary treatment)
  cvd <- cv.glmnet(xtr, Tr$Treat, family="binomial", alpha=1, nfolds=5)
  ehat <- as.numeric(predict(cvd, xte, s="lambda.min", type="response"))
  
  # Store predictions
  dt[te, m_hat := mhat]
  dt[te, e_hat := ehat]
}

# -------------------- Overlap Protection --------------------
# Propensity scores near 0 or 1 indicate weak overlap (positivity violation)
# - e_hat ≈ 0: units almost never treated (can't estimate treatment effect)
# - e_hat ≈ 1: units almost always treated (can't estimate control outcome)
#
# Solution: Clip propensity scores to [overlap_lower, overlap_upper]
# Default: [0.05, 0.95] ensures we have at least 5% probability of both treatment states

dt[, e_hat := pmin(pmax(e_hat, overlap_lower), overlap_upper)]

# Optional: Trim observations with extreme propensity scores
# (Usually not needed if clipping is sufficient, but useful for sensitivity analysis)
if (trim_overlap) {
  before <- nrow(dt)
  dt <- dt[e_hat > overlap_lower & e_hat < overlap_upper]
  cat("✂️  Trimmed", before-nrow(dt), "rows with extreme propensity scores\n")
}

cat("✅ Cross-fitting complete\n")

# Diagnose overlap quality
cat("\nPropensity score distribution by treatment status:\n")
overlap_diag <- dt[!is.na(e_hat), .(
  min = min(e_hat),
  p25 = quantile(e_hat, 0.25),
  median = median(e_hat),
  p75 = quantile(e_hat, 0.75),
  max = max(e_hat)
), by=Treat]
print(overlap_diag)

# Flag potential overlap issues
weak_overlap <- dt[!is.na(e_hat), .(
  n_extreme = sum(e_hat < 0.1 | e_hat > 0.9),
  pct_extreme = 100 * mean(e_hat < 0.1 | e_hat > 0.9)
)]
if (weak_overlap$pct_extreme > 10) {
  cat("⚠️  Warning:", round(weak_overlap$pct_extreme, 1), 
      "% observations have extreme propensity scores (consider trimming)\n")
}

# -------------------- Orthogonal R-Learner Signals --------------------
# The orthogonal score removes confounding bias via Neyman orthogonality
# 
# Formula: S_i = (D_i - e(X_i)) * (Y_i - m(X_i)) / Var(D_i | X_i)
# 
# Intuition:
# - (D_i - e(X_i)): Residualized treatment (removes confounding from X)
# - (Y_i - m(X_i)): Residualized outcome (removes direct effect of X on Y)
# - Divide by Var(D|X): Stabilizes the variance (downweights low-overlap regions)
# - S_i is now approximately mean-zero and unbiased for CATE
#
# Winsorization: Clip extreme values at 1st/99th percentiles
# - Prevents outliers from dominating the BLP regression
# - Common practice in robust machine learning

# Compute propensity variance (used for weighting)
dt[, v_hat := e_hat * (1-e_hat)]

# Compute orthogonal score
dt[, S := (Treat - e_hat) * (Y - m_hat) / pmax(v_hat, 1e-6)]

# Winsorize at 1% and 99% to handle outliers
qL <- quantile(dt$S, 0.01, na.rm=TRUE)
qU <- quantile(dt$S, 0.99, na.rm=TRUE)
dt[, S_w := pmin(pmax(S, qL), qU)]

cat("\n", strrep("=", 60), "\n")
cat("ORTHOGONAL SIGNALS\n")
cat(strrep("=", 60), "\n")
cat("Original signal S:\n")
print(summary(dt$S))
cat("\nWinsorized signal S_w (clipped at 1%/99%):\n")
print(summary(dt$S_w))

# -------------------- Estimate Average Treatment Effect (ATE) --------------------
# The BLP gives us HETEROGENEITY (how effects vary), not the average level
# We need a separate ATE estimate to anchor the heterogeneity index in Step 3
#
# Method: Two-way fixed effects (TWFE) regression
# - Controls for county fixed effects (time-invariant heterogeneity)
# - Controls for month fixed effects (common time trends)
# - Two-way clustering for robust standard errors
#
# Why not use BLP intercept? 
# - BLP is weighted by v_hat, which can bias the average
# - TWFE is more transparent and easier to interpret

cat("\n", strrep("=", 60), "\n")
cat("AVERAGE TREATMENT EFFECT (ATE) - For Step 3 Anchoring\n")
cat(strrep("=", 60), "\n")

ate_model <- feols(
  Y ~ Treat | cid + YM,
  data = dt,
  cluster = ~cid + YM
)

ate_estimate <- coef(ate_model)["Treat"]
ate_se <- se(ate_model)["Treat"]

cat(sprintf("\nATE (log scale): %.4f (SE: %.4f)\n", ate_estimate, ate_se))
cat(sprintf("  95%% CI: [%.4f, %.4f]\n", 
            ate_estimate - 1.96*ate_se, 
            ate_estimate + 1.96*ate_se))
cat(sprintf("\nATE (%% change): %.2f%%\n", 100*(exp(ate_estimate)-1)))
cat(sprintf("  Interpretation: Policy changes SNAP caseload by %.2f%% on average\n",
            100*(exp(ate_estimate)-1)))

# -------------------- Orthogonal BLP Regression --------------------
# Best Linear Projection (BLP) approximates the CATE function:
# 
# CATE(X) ≈ α + X'β
# 
# where X are standardized moderators and β captures heterogeneity
#
# Regression: S_w ~ X, weighted by v_hat
# - S_w: Orthogonal signal (unbiased proxy for CATE)
# - X: Standardized county characteristics
# - Weights: v_hat = e(X)(1-e(X)) gives more weight to regions with good overlap
# - Clustering: Two-way (county × month) for robust inference
#
# Output interpretation:
# - β̂_j: 1-SD increase in moderator j changes treatment effect by β̂_j log points
# - Since moderators are standardized, coefficients are directly comparable

cat("\n", strrep("=", 60), "\n")
cat("ORTHOGONAL BLP: Heterogeneity Analysis\n")
cat(strrep("=", 60), "\n")

form_blp <- as.formula(paste("S_w ~", paste(mods, collapse=" + ")))

blp_fit <- feols(
  fml = form_blp,
  data = dt,
  weights = ~v_hat,
  cluster = ~cid + YM
)

cat("\nTwo-way clustered standard errors (county × month):\n")
cat("Interpretation: β̂ = effect of 1-SD change in moderator on treatment effect\n\n")
print(summary(blp_fit))

# Extract coefficient table
blp_coef_table <- broom::tidy(blp_fit)
blp_coef_table <- blp_coef_table[order(-abs(blp_coef_table$statistic)), ]  # Sort by |t-stat|

cat("\nBLP Coefficients (sorted by |t-statistic|):\n")
print(blp_coef_table[, c("term","estimate","std.error","statistic","p.value")])

# Identify significant moderators (p < 0.05)
sig_mods <- blp_coef_table[blp_coef_table$p.value < 0.05 & 
                             blp_coef_table$term != "(Intercept)", ]
if (nrow(sig_mods) > 0) {
  cat("\n✅ Significant moderators (p < 0.05):\n")
  print(sig_mods[, c("term","estimate","p.value")])
} else {
  cat("\n⚠️  No significant moderators detected (no strong heterogeneity)\n")
}

# -------------------- County-Level Treatment Effect Index --------------------
# Compute τ̂_i for each county i:
# 
# τ̂_i = α̂ + X̄_i' β̂
# 
# where X̄_i = county-level mean of standardized moderators
#
# This gives us a linear heterogeneity index:
# - Positive τ̂_i: Treatment effect stronger than average in this county
# - Negative τ̂_i: Treatment effect weaker than average in this county
#
# IMPORTANT: τ̂_i is on LOG SCALE (since Y = log(cases))
# - To convert to % change: 100 × (exp(τ̂_i) - 1)
# - This is a RELATIVE index; need to anchor around ATE in Step 3

cat("\n", strrep("=", 60), "\n")
cat("COUNTY-LEVEL TREATMENT EFFECT INDEX\n")
cat(strrep("=", 60), "\n")

# Compute county-level means of standardized moderators
X_means <- dt[, lapply(.SD, mean, na.rm=TRUE), .SDcols=mods, by=cid]

# Predict τ̂_i using BLP coefficients
# Using predict() avoids manual matrix multiplication and ensures correct alignment
tau_i_pp <- as.numeric(predict(blp_fit, newdata=X_means, type="link"))
X_means[, tau_i_pp := tau_i_pp]

# Convert to percentage change for easier interpretation
X_means[, pct_change := 100 * (exp(tau_i_pp) - 1)]

cat("\nCounty-level tau_i_pp (log scale):\n")
print(summary(X_means$tau_i_pp))

cat("\nCounty-level treatment effects (% change):\n")
print(summary(X_means$pct_change))

cat("\nDistribution of heterogeneity:\n")
cat(sprintf("  Range: [%.2f%%, %.2f%%]\n", 
            min(X_means$pct_change, na.rm=TRUE),
            max(X_means$pct_change, na.rm=TRUE)))
cat(sprintf("  IQR: [%.2f%%, %.2f%%]\n",
            quantile(X_means$pct_change, 0.25, na.rm=TRUE),
            quantile(X_means$pct_change, 0.75, na.rm=TRUE)))

# Show counties with largest effects (both positive and negative)
cat("\n📊 Top 5 counties (largest effects in magnitude):\n")
print(X_means[order(-abs(tau_i_pp))][1:5, .(cid, tau_i_pp, pct_change)])

cat("\n📊 Bottom 5 counties (smallest effects):\n")
print(X_means[order(abs(tau_i_pp))][1:5, .(cid, tau_i_pp, pct_change)])

# -------------------- Save Results for Step 3 --------------------
cat("\n", strrep("=", 60), "\n")
cat("SAVING OUTPUTS\n")
cat(strrep("=", 60), "\n")

# Package all essential components for Step 3
step2_output <- list(
  # BLP model components
  blp_coef = coef(blp_fit),           # Coefficient vector β̂
  vcov_blp = vcov(blp_fit),           # Variance-covariance matrix for uncertainty
  moderators = mods,                   # List of moderator names (for alignment)
  
  # County-level predictions
  X_means = X_means,                   # County means + tau_i_pp
  
  # Average treatment effect (for anchoring)
  ate = ate_estimate,                  # ATE point estimate
  ate_se = ate_se,                     # ATE standard error
  
  # Standardization parameters (for new data in Step 3)
  baseline_stats = baseline_stats,     # Mean & SD from baseline period
  
  # Metadata
  overlap_bounds = c(lower=overlap_lower, upper=overlap_upper),
  n_obs = nrow(dt),
  n_counties = uniqueN(dt$cid),
  date_created = Sys.time()
)

# Save main output (R object for Step 3)
saveRDS(step2_output, "step2_output.rds")
cat("✅ Saved: step2_output.rds (complete results for Step 3)\n")

# Save county effects as CSV (for inspection)
fwrite(X_means, "step2_county_effects.csv")
cat("✅ Saved: step2_county_effects.csv (county-level predictions)\n")

# Save BLP coefficients as CSV (for reporting)
fwrite(blp_coef_table, "step2_blp_coefs.csv")
cat("✅ Saved: step2_blp_coefs.csv (regression coefficients)\n")

# -------------------- Interpretation Guide --------------------
cat("\n", strrep("=", 60), "\n")
cat("⚠️  HOW TO USE THESE RESULTS\n")
cat(strrep("=", 60), "\n")

cat("\n1. SCALE AND UNITS:\n")
cat("   • tau_i_pp is on LOG SCALE (since Y = log(cases))\n")
cat("   • Convert to % change: pct_change = 100 × (exp(tau_i_pp) - 1)\n")
cat("   • Small tau (<0.1): linear approximation works (% ≈ 100×tau)\n")









# ============================================================
# STEP 3: Anchor county-level heterogeneity (tau_i_pp) to an
#         interpretable effect scale and export artifacts
# ============================================================
# What this script does:
#   1) Loads Step 2 output (step2_tau_i_pp.csv), which contains:
#        - cid      : county identifier (kept consistent with Step 2)
#        - tau_i_pp : county-level relative heterogeneity index
#   2) Anchors tau_i_pp to a probability/percentage-point scale using
#        tau_tilde_c = tau_bar + (tau_i_pp - mean(tau_i_pp))
#      and clips to [lower, upper]. This preserves the *relative*
#      ranking from Step 2 while centering on your global ATE.
#   3) (Optional) If a panel path is provided, computes convenient
#      pre-policy weights by county (e.g., mean recipients/cases) so
#      that you can multiply effects into counts in later steps.
#   4) Exports a map-ready CSV and a compact summary.
#
# IMPORTANT CONSISTENCY WITH STEP 2:
#   - We do *not* re-standardize or re-estimate anything here.
#   - We treat Step 2’s tau_i_pp strictly as a RELATIVE index.
#   - The anchoring uses a single scalar tau_bar (your chosen ATE),
#     exactly as discussed; this avoids mixing scales with Step 2’s
#     target (which was based on the orthogonal signal for Y).
#
# Inputs (all via key=value CLI args):
#   step2_tau   : path to "step2_tau_i_pp.csv" (required)
#   tau_bar     : numeric global ATE in probability units (default 0.05)
#   lower       : numeric lower clip bound (default 0.00)
#   upper       : numeric upper clip bound (default 0.50)
#   out_prefix  : prefix for output files (default "step3")
#   panel       : (optional) the original panel used in Step 2
#                 (e.g., ~/Desktop/.../snap_laus_with_acs_final_clean.csv.gz)
#
# Outputs:
#   {out_prefix}_tau_anchored.csv
#       - cid, tau_i_pp, tau_centered, tau_tilde, rank, decile
#       - (optional) pre_policy_weight columns if panel provided
#   {out_prefix}_summary.txt (printed to console)
#
# Notes:
#   - Anchoring formula: tau_tilde = tau_bar + (tau_i_pp - mean(tau_i_pp))
#     This preserves relative heterogeneity and places the distribution
#     around your ATE. Finally we clip to [lower, upper].
#   - Choose tau_bar from your main policy effect (e.g., Step 1 DiD).
# ============================================================

# -------------------- small utilities --------------------
arg_map <- list()
if (length(commandArgs(TRUE))) {
  for (kv in strsplit(commandArgs(TRUE), "=", fixed = TRUE)) {
    if (length(kv) == 2 && nzchar(kv[[1]])) arg_map[[kv[[1]]]] <- kv[[2]]
  }
}
get_arg <- function(key, default = NULL) {
  if (!is.null(arg_map[[key]]) && nzchar(arg_map[[key]])) return(arg_map[[key]])
  default
}
parse_num <- function(x, default) {
  if (is.null(x) || !nzchar(x)) return(default)
  as.numeric(x)
}
need_pkg <- function(pkgs){
  repos <- getOption("repos")
  if (is.null(repos) || repos["CRAN"] == "@CRAN@") {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }
  for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}

# -------------------- packages --------------------
need_pkg(c("data.table"))
suppressPackageStartupMessages({ library(data.table) })

# -------------------- args --------------------
step2_tau  <- get_arg("step2_tau", "step2_tau_i_pp.csv")   # Step 2 output
tau_bar    <- parse_num(get_arg("tau_bar", "0.05"), 0.05)  # e.g., 0.05 = 5 pp
lower      <- parse_num(get_arg("lower", "0.00"), 0.00)
upper      <- parse_num(get_arg("upper", "0.50"), 0.50)
out_prefix <- get_arg("out_prefix", "step3")
panel_path <- get_arg("panel", NA_character_)              # optional

stopifnot(file.exists(step2_tau))
stopifnot(is.finite(tau_bar), is.finite(lower), is.finite(upper), lower <= tau_bar, upper >= tau_bar)

# -------------------- load Step 2 tau --------------------
tau_dt <- fread(step2_tau)
# Expect columns: cid, tau_i_pp
stopifnot("cid" %in% names(tau_dt), "tau_i_pp" %in% names(tau_dt))

# -------------------- anchor function (pure re-centering + clipping) --------------------
anchor_tau <- function(tau_lin, tau_bar, lower = 0.0, upper = 0.5){
  # Center around zero by subtracting the sample mean of tau_i_pp
  tau_centered <- tau_lin - mean(tau_lin, na.rm = TRUE)
  # Re-anchor on your chosen ATE (tau_bar) and clip
  tau_tilde <- tau_bar + tau_centered
  tau_tilde <- pmin(pmax(tau_tilde, lower), upper)
  list(centered = tau_centered, anchored = tau_tilde)
}

# -------------------- compute anchored tau --------------------
anch <- anchor_tau(tau_dt$tau_i_pp, tau_bar = tau_bar, lower = lower, upper = upper)
tau_dt[, tau_centered := anch$centered]
tau_dt[, tau_tilde    := anch$anchored]

# ------------------------------------------------------------
# Helpful ordering and deciles for reporting/mapping buckets
# ------------------------------------------------------------
setorder(tau_dt, -tau_tilde)
tau_dt[, rank := .I]  # 1 = strongest positive effect

# Robust decile computation: handle ties or flat distributions
qs <- unique(quantile(tau_dt$tau_tilde,
                      probs = seq(0, 1, 0.1),
                      na.rm = TRUE, type = 7))

if (length(qs) < 3) {
  # All values identical or nearly so — fallback to rank-based deciles
  tau_dt[, decile := ceiling(10 * rank(tau_tilde, ties.method = "average") / .N)]
  tau_dt[decile == 0, decile := 1]  # ensure 1–10 range
  cat("⚠️ Quantile breaks not unique — used rank-based deciles instead.\n")
} else {
  tau_dt[, decile := cut(tau_tilde,
                         breaks = qs,
                         include.lowest = TRUE,
                         labels = FALSE)]
}


# -------------------- optional: attach pre-policy weights --------------------
# If you provide the original panel, we will compute simple pre-policy county weights
# that you can use later to project effects into counts. This keeps consistency with
# Step 2: we do NOT re-standardize; we only summarize pre-policy levels.
if (!is.na(panel_path) && nzchar(panel_path) && file.exists(panel_path)) {
  pan <- fread(panel_path)
  # Identify year/month, id, and a suitable "size" proxy
  nm <- names(pan)
  ycol <- c("YEAR","year","Year"); ycol <- ycol[ycol %in% nm][1]
  mcol <- c("MONTH","month","Month","Mon"); mcol <- mcol[mcol %in% nm][1]
  idc  <- c("id_num","county_id","countyid","cid","fips","FIPS"); idc <- idc[idc %in% nm][1]
  tcol <- c("Treat","treat","treated","enforced","policy_enforced","wr_in_effect"); tcol <- tcol[tcol %in% nm][1]
  
  if (is.na(ycol) || is.na(mcol) || is.na(idc) || is.na(tcol)) {
    warning("Panel provided but missing YEAR/MONTH/county_id/Treat. Skipping pre-policy weights.")
  } else {
    setnames(pan, c(ycol,mcol,idc,tcol), c("YEAR","MONTH","cid","Treat"))
    pan[, Treat := as.integer(Treat > 0)]
    # A simple pre-policy window: months strictly before the first treated month in the state
    pan[, ym := YEAR*100L + MONTH]
    policy_start <- pan[Treat==1, suppressWarnings(min(ym, na.rm=TRUE))]
    baseline <- if (is.finite(policy_start)) pan[ym < policy_start] else pan[Treat==0]
    
    # Choose a convenient weight proxy; prefer 'cases' if present, else recipients, else households
    size_col <- intersect(c("cases","recipients","Recipients","cases_per_1k_hh","total_households","households"), names(baseline))[1]
    if (!is.na(size_col)) {
      # pre_policy_weight: average baseline size by county
      w_dt <- baseline[, .(pre_policy_weight = mean(get(size_col), na.rm = TRUE)), by = cid]
      tau_dt <- merge(tau_dt, w_dt, by = "cid", all.x = TRUE, sort = FALSE)
      cat("ℹ️ Attached pre_policy_weight from '", size_col, "' (baseline period)\n", sep = "")
    } else {
      warning("No suitable size column found in panel; skipping pre-policy weights.")
    }
  }
}

# -------------------- export --------------------
out_csv <- sprintf("%s_tau_anchored.csv", out_prefix)
fwrite(tau_dt[, .(cid, tau_i_pp, tau_centered, tau_tilde, rank, decile,
                  pre_policy_weight = if ("pre_policy_weight" %in% names(tau_dt)) pre_policy_weight else NA_real_)],
       out_csv)

# Console summary
cat("\n================ STEP 3 SUMMARY ================\n")
cat("Input Step 2 file : ", step2_tau, "\n", sep = "")
cat("tau_bar (ATE)     : ", sprintf("%.4f", tau_bar), " (i.e., ", sprintf("%.1f", 100*tau_bar), " pp)\n", sep = "")
cat("Clip bounds       : [", sprintf("%.2f", lower), ", ", sprintf("%.2f", upper), "]\n", sep = "")
cat("Counties          : ", nrow(tau_dt), "\n", sep = "")
cat("tau_tilde (anchored) summary:\n", sep = "")
print(summary(tau_dt$tau_tilde))
if ("pre_policy_weight" %in% names(tau_dt)) {
  cat("Non-missing pre_policy_weight: ", sum(!is.na(tau_dt$pre_policy_weight)), "\n", sep = "")
}
cat("Exported          : ", out_csv, "\n", sep = "")
cat("=================================================\n")

# -------------------- (COMMENT) Next-step tips --------------------
# 1) If you need to convert anchored effects into exits per month:
#       expected_exits_c,t  ≈  tau_tilde_c  *  (eligible_population_c,t)
#    Provide your own eligible population or recipients count by county-month.
# 2) If you want map breaks: use 'decile' or build custom quantiles.
# 3) Sensitivity: vary tau_bar, [lower, upper], or recompute Step 2 with
#    different overlap guardrails (e.g., 0.1..0.9) and compare anchored outputs.


# ============================================================
# STEP 3 POST-PROCESSING:
# - Robust deciles for tau_tilde (with unique-breaks fallback)
# - Histogram of tau_tilde (optional)
# - Expected exits by county using baseline eligible size
#   expected_exits_c ≈ tau_tilde_c * baseline_size_c
# - Optional aggregation to month/quarter/year (if panel provided)
# ============================================================

# ---------- small helpers ----------
arg_map <- list()
if (length(commandArgs(TRUE))) {
  for (kv in strsplit(commandArgs(TRUE), "=", fixed = TRUE)) {
    if (length(kv) == 2 && nzchar(kv[[1]])) arg_map[[kv[[1]]]] <- kv[[2]]
  }
}
get_arg <- function(key, default = NULL) {
  x <- arg_map[[key]]; if (!is.null(x) && nzchar(x)) return(x); default
}
need_pkg <- function(pkgs){
  repos <- getOption("repos")
  if (is.null(repos) || repos["CRAN"] == "@CRAN@") options(repos = c(CRAN="https://cloud.r-project.org"))
  for (p in pkgs) if (!requireNamespace(p, quietly=TRUE)) install.packages(p)
}

# ---------- packages ----------
need_pkg(c("data.table"))
suppressPackageStartupMessages({ library(data.table) })

# Optional plotting
hist_png <- get_arg("hist_png", "")
if (nzchar(hist_png)) {
  need_pkg(c("ggplot2"))
  library(ggplot2)
}

# ---------- args ----------
step3_csv <- get_arg("step3_csv", "step3_tau_anchored.csv")   # from Step 3
panel     <- get_arg("panel", NA_character_)                  # optional: Step 2 panel path
size_col  <- get_arg("size_col", "cases")                     # eligible size proxy
out_prefix<- get_arg("out_prefix", "step3_post")

stopifnot(file.exists(step3_csv))

# ---------- load data ----------
tau <- fread(step3_csv)  # expects: cid, tau_i_pp, tau_centered, tau_tilde, rank, decile (decile may be absent)
stopifnot(all(c("cid","tau_tilde") %in% names(tau)))

# ---------- robust deciles (in case not present or had 'breaks not unique') ----------
if (!("decile" %in% names(tau))) {
  # Order by descending tau_tilde; rank = 1 is strongest positive effect
  setorder(tau, -tau_tilde)
  tau[, rank := .I]
  # Try quantile deciles; if breaks not unique, fallback to rank-based
  qs <- unique(quantile(tau$tau_tilde, probs = seq(0,1,0.1), na.rm = TRUE, type = 7))
  if (length(qs) < 3) {
    tau[, decile := ceiling(10 * rank(tau_tilde, ties.method = "average") / .N)]
    tau[decile == 0, decile := 1]
    message("⚠️ Quantile breaks not unique — used rank-based deciles.")
  } else {
    tau[, decile := cut(tau_tilde, breaks = qs, include_lowest = TRUE, labels = FALSE)]
  }
} else {
  # Ensure integer 1..10 if already present
  tau[, decile := as.integer(decile)]
}

# ---------- optional: attach baseline eligible size from panel ----------
# We will compute a PRE-POLICY average size per county using 'size_col'.
if (!is.na(panel) && nzchar(panel) && file.exists(panel)) {
  pan <- fread(panel)
  nm <- names(pan)
  # Flexible column mapping (consistent with Step 2)
  ycol <- c("YEAR","year","Year"); ycol <- ycol[ycol %in% nm][1]
  mcol <- c("MONTH","month","Month","Mon"); mcol <- mcol[mcol %in% nm][1]
  idc  <- c("id_num","county_id","countyid","cid","fips","FIPS"); idc <- idc[idc %in% nm][1]
  tcol <- c("Treat","treat","treated","enforced","policy_enforced","wr_in_effect"); tcol <- tcol[tcol %in% nm][1]
  
  if (is.na(ycol) || is.na(mcol) || is.na(idc) || is.na(tcol)) {
    warning("Panel provided but missing YEAR/MONTH/county_id/Treat. Skipping baseline size merge.")
  } else if (!(size_col %in% nm)) {
    warning(sprintf("Column '%s' not in panel. Skipping baseline size merge.", size_col))
  } else {
    setnames(pan, c(ycol,mcol,idc,tcol), c("YEAR","MONTH","cid","Treat"))
    pan[, Treat := as.integer(Treat > 0)]
    pan[, ym := YEAR*100L + MONTH]
    # Pre-policy window = months strictly before first treated month in the state
    policy_start <- pan[Treat==1, suppressWarnings(min(ym, na.rm=TRUE))]
    baseline <- if (is.finite(policy_start)) pan[ym < policy_start] else pan[Treat==0]
    if (!nrow(baseline)) baseline <- pan  # fallback
    
    # Compute baseline eligible size per county
    w_dt <- baseline[, .(baseline_size = mean(get(size_col), na.rm = TRUE)), by = cid]
    tau <- merge(tau, w_dt, by = "cid", all.x = TRUE, sort = FALSE)
    
    # Expected exits (per period in baseline scale):
    # expected_exits_c ≈ tau_tilde_c * baseline_size_c
    tau[, expected_exits := tau_tilde * baseline_size]
    
    message(sprintf("✅ Attached baseline_size from '%s' (pre-policy average).", size_col))
    
    # Optional aggregations (if you later want month/quarter/year, you need panel by month).
    # Here we just provide county-level expected exits based on baseline size.
  }
}

# ---------- histogram of tau_tilde (optional) ----------
if (nzchar(hist_png)) {
  p <- ggplot(tau, aes(x = tau_tilde)) +
    geom_histogram(bins = 30) +
    labs(title = "Distribution of Anchored County Effects (tau_tilde)",
         x = "tau_tilde (probability units)", y = "Count")
  ggsave(hist_png, p, width = 7, height = 4.5, dpi = 300)
  message(sprintf("📈 Saved histogram to '%s'.", hist_png))
}

# ---------- export ----------
out_csv <- sprintf("%s_outputs.csv", out_prefix)
# Keep tidy columns; include baseline_size/expected_exits if available
keep_cols <- intersect(c("cid","tau_i_pp","tau_centered","tau_tilde","rank","decile",
                         "baseline_size","expected_exits"), names(tau))
fwrite(tau[, ..keep_cols], out_csv)

# Console summary
cat("\n=========== STEP 3 POST SUMMARY ===========\n")
cat("Input step3 file : ", step3_csv, "\n", sep = "")
if (!is.na(panel) && nzchar(panel)) cat("Panel (for size) : ", panel, "\n", sep = "")
cat("Counties         : ", nrow(tau), "\n", sep = "")
cat("tau_tilde summary:\n"); print(summary(tau$tau_tilde))
if ("baseline_size" %in% names(tau)) {
  cat("Non-missing baseline_size : ", sum(!is.na(tau$baseline_size)), "\n", sep = "")
  cat("expected_exits summary    :\n"); print(summary(tau$expected_exits))
}
cat("Exported         : ", out_csv, "\n", sep = "")
cat("===========================================\n")



