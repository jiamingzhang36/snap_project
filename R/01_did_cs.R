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

source("R/utils.R")  # Load helper functions (includes safe_dir)

in_path  <- "data_clean/snap_laus_with_policy.csv"   # 相对路径更稳
out_dir  <- safe_dir("outputs/step1_did")            # 专属子目录


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








