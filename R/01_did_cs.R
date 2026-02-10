############################################################
# Callaway-Sant'Anna DR-DiD: Complete Analysis
# ABAWD Enforcement and SNAP Participation
#
# Includes: Main specification + 15 robustness checks
############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(readr)
  library(did)
  library(fixest)
  library(ggplot2)
  library(tidyr)
  library(knitr)
})

# Set seed for reproducibility (did package uses bootstrap by default)
set.seed(123)

#############################
# Setup
#############################

if (file.exists("config/paths.R")) {
  source("config/paths.R", local = TRUE)
  root <- ROOT
} else {
  root <- normalizePath(if (file.exists("snap_project.Rproj")) "." else "..", winslash = "/", mustWork = TRUE)
}
if (!exists("outdir")) outdir <- file.path(root, "outputs", "step1_did")
infile <- file.path(root, "data_clean", "panel_with_G.csv")

if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
# Allow caller (e.g. 02_abawd/02_estimate_event_study.R) to inject panel_raw from panel_analysis.rds
if (!exists("panel_raw")) {
  stopifnot(file.exists(infile))
  panel_raw <- read_csv(infile, show_col_types = FALSE)
}

source("R/00_utils/helpers_did.R", local = TRUE)

event_study_df <- function(es) {
  # Use crit.val.egt for uniform confidence bands (not crit.val)
  # This ensures the confidence bands are simultaneous (uniform) bands, not pointwise
  crit <- if (!is.null(es$crit.val.egt)) es$crit.val.egt else 1.96
  data.frame(
    event_time = es$egt,
    estimate   = es$att.egt,
    se         = es$se.egt,
    ci_lower   = es$att.egt - crit * es$se.egt,
    ci_upper   = es$att.egt + crit * es$se.egt
  )
}

pretrend_test <- function(es_df, cutoff = -2, label = "", att_obj = NULL, min_pre = -6) {
  # NOTE: This is a SIMPLIFIED pre-trend test that ignores covariance between
  # event-study coefficients. It uses ∑(b/se)² which assumes independence.
  # This is a "quick check" but should not be over-interpreted as definitive proof.
  # 
  # Window: Focus on pre-periods within [min_pre, cutoff) to match event-study plot window.
  # Default: min_pre = -6, cutoff = -2, so tests pre-trends in [-6, -2) window.
  
  pre <- es_df %>%
    filter(event_time >= min_pre, event_time < cutoff, !is.na(estimate), !is.na(se))
  
  if (nrow(pre) == 0) {
    return(list(
      label = label,
      wald_stat_simple = NA_real_,
      p_wald_simple = NA_real_,
      p_conditional = NA_real_,
      n_pre_periods = 0
    ))
  }
  
  # Simplified Wald test (ignores covariance)
  wald_stat_simple <- sum((pre$estimate / pre$se)^2)
  p_wald_simple    <- 1 - pchisq(wald_stat_simple, df = nrow(pre))
  
  # Try to use did package's conditional_did_pretest() for rigorous test
  p_conditional <- NA_real_
  if (!is.null(att_obj)) {
    tryCatch({
      # conditional_did_pretest requires the att_gt object, not the aggte object
      pretest_res <- conditional_did_pretest(att_obj, type = "dynamic")
      if (!is.null(pretest_res) && !is.null(pretest_res$p.value)) {
        p_conditional <- pretest_res$p.value
      }
    }, error = function(e) {
      # Silently fail if pretest computation fails
    })
  }
  
  # Output
  if (!is.na(p_conditional) && is.finite(p_conditional)) {
    cat(sprintf("[%s] Pre-trend test (simplified, window [%d, %d)): Wald=%.3f (df=%d), p=%.4f\n",
                label, min_pre, cutoff, wald_stat_simple, nrow(pre), p_wald_simple))
    cat(sprintf("[%s] Pre-trend test (conditional_did_pretest, RECOMMENDED): p=%.4f\n",
                label, p_conditional))
  } else {
    cat(sprintf("[%s] Pre-trend test (simplified, window [%d, %d)): Wald=%.3f (df=%d), p=%.4f\n",
                label, min_pre, cutoff, wald_stat_simple, nrow(pre), p_wald_simple))
    cat("    ⚠️  Note: This is a quick check. For rigorous testing, use conditional_did_pretest().\n")
  }
  
  # Return results as data frame for saving
  result_df <- data.frame(
    specification = label,
    wald_stat_simple = wald_stat_simple,
    p_wald_simple = p_wald_simple,
    p_conditional = p_conditional,
    n_pre_periods = nrow(pre)
  )
  
  invisible(result_df)
}

make_es_plot <- function(es_df, title, subtitle, file_name = NULL, 
                         color = "#0072B2", min_pre = -6, max_post = 6,
                         caption_note = NULL) {
  # Focus on ±6 months window for cleaner visualization
  # This aligns with balance_e = 6 and min_e/max_e = ±6 in aggte
  # Note: Overall ATT is computed across all available post-treatment periods,
  # but event-study plot is displayed for [-6, +6] for readability.
  es_trim <- es_df %>% filter(event_time >= min_pre, event_time <= max_post)
  
  # Build caption
  plot_caption <- "Note: Overall ATT is simple aggregation across all available post-treatment periods. Event-study plot displayed for [-6, +6] for readability."
  if (!is.null(caption_note)) {
    plot_caption <- paste(plot_caption, caption_note, sep = " ")
  }
  
  p <- ggplot(es_trim, aes(x = event_time, y = estimate)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "gray85", alpha = 0.7) +
    geom_line(linewidth = 1.2, color = color) +
    geom_point(size = 2.6, color = color) +
    geom_vline(xintercept = 0, color = "black", linewidth = 0.9) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    scale_x_continuous(breaks = seq(min_pre, max_post, by = 6)) +
    labs(title = title, subtitle = subtitle,
         x = "Months relative to first enforcement",
         y = "ATT on log(1 + recipients per 1k, 18–49)",
         caption = plot_caption) +
    theme_bw(base_size = 14) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 40, hjust = 1),
          plot.title = element_text(face = "bold"),
          plot.caption = element_text(size = 9, hjust = 0, color = "gray40"))
  
  print(p)
  if (!is.null(file_name)) {
    ggsave(file.path(outdir, file_name), p, width = 8, height = 5, dpi = 300)
    cat("[✓] Plot saved:", file_name, "\n")
  }
  invisible(p)
}

safe_val <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  as.numeric(x)
}

############################################################
# 1. MAIN SPECIFICATION
############################################################

cat("\n╔", strrep("═", 60), "╗\n", sep = "")
cat("║", sprintf("%-60s", " MAIN SPECIFICATION"), "║\n", sep = "")
cat("╚", strrep("═", 60), "╝\n\n", sep = "")

# Main specification: use rate column and apply log1p transformation explicitly
# This ensures consistency between Y definition and plot labels
# Main spec uses Ant=2: treatment window includes -2, -1 months (anticipation effects)
# This aligns with SA (ref.p=c(-3, -1)) and TWFE (treat_post_ant2) for fair comparison
Y_MAIN <- "y_per1k_18_49"
df_main <- build_analysis_df(panel_raw, Y_MAIN)
cat(sprintf("[build] Rows: %d | Units: %d | Treated: %s\n",
            nrow(df_main), n_distinct(df_main$id_num), any(df_main$G_int > 0)))

res_main <- run_cs_did(df_main, anticipation = 2)
overall_main <- res_main$overall
es_main <- res_main$es

cat(sprintf("\nOverall ATT: %.4f (SE = %.4f)\n",
            overall_main$overall.att, overall_main$overall.se))

# Compute CS post 1-6 month window average for fair comparison with SA
# This uses the same window (post 1-6) as SA, making the comparison fair
es_cs_window <- aggte(res_main$att, type = "dynamic", cband = FALSE, balance_e = 6)
idx_cs <- which(es_cs_window$egt >= 1 & es_cs_window$egt <= 6)
cs_post_avg <- mean(es_cs_window$att.egt[idx_cs], na.rm = TRUE)

# SE calculation: use covariance matrix if available, otherwise conservative approximation
Vcs <- NULL
if (!is.null(es_cs_window$V_egt)) Vcs <- es_cs_window$V_egt
if (!is.null(es_cs_window$V))     Vcs <- es_cs_window$V

if (!is.null(Vcs) && length(idx_cs) > 0) {
  k <- length(idx_cs)
  w <- rep(1/k, k)
  Vsub <- Vcs[idx_cs, idx_cs, drop = FALSE]
  cs_post_se <- sqrt(drop(t(w) %*% Vsub %*% w))
} else {
  # Conservative approximation (ignores covariance)
  k <- length(idx_cs)
  cs_post_se <- sqrt(sum(es_cs_window$se.egt[idx_cs]^2, na.rm = TRUE)) / k
}

cat(sprintf("CS post 1-6 month average: %.4f (SE = %.4f)\n", cs_post_avg, cs_post_se))

# Diagnostic: Explain why different methods may give different results
cat("\n=== Why Different Methods May Give Different Results (Even When Aligned) ===\n")
cat("Even though all methods are aligned to Ant=2, they may still differ because:\n\n")
cat("1. ESTIMAND DIFFERENCES:\n")
cat("   - CS overall ATT: weighted average across ALL available post periods\n")
cat("   - CS post 1-6 avg: average of post months 1-6 only\n")
cat("   - SA full ATT: different aggregation rule (cohort weights differ from CS)\n")
cat("   - SA post 1-6 avg: average of post months 1-6 (should be more comparable to CS post 1-6)\n")
cat("   - TWFE: estimates a potentially biased weighted average under staggered adoption\n\n")
cat("2. WEIGHTING DIFFERENCES:\n")
cat("   - CS: weights by cohort size and number of available post periods\n")
cat("   - SA: uses different weighting scheme (may emphasize different cohorts)\n")
cat("   - TWFE: can have negative weights (causing bias) in staggered settings\n\n")
cat("3. SAMPLE SELECTION:\n")
cat("   - CS with balance_e=6: only uses cohorts with at least 6 post periods\n")
cat("   - SA: uses all available data (may include shorter panels)\n")
cat("   - Different methods handle unbalanced panels differently\n\n")
cat("4. TREATMENT EFFECT HETEROGENEITY:\n")
cat("   - If effects vary by cohort or time, different weighting = different averages\n")
cat("   - Early-treated vs late-treated may have different effects\n\n")
cat("5. TWFE BIAS:\n")
cat("   - TWFE is known to be biased under staggered adoption (Goodman-Bacon 2021)\n")
cat("   - Even when aligned, TWFE may give different (biased) results\n\n")
cat("RECOMMENDATION: Compare CS post 1-6 avg with SA post 1-6 avg for fair comparison.\n")
cat("These should be more similar than overall ATTs, as they use the same window.\n\n")

es_df_main <- event_study_df(es_main)
# Pre-trend test: focus on [-6, -3) window (before treatment window which includes -2, -1)
# Note: With Ant=2, -2 and -1 are included in treatment, so pre-trend test uses [-6, -3)
pt_main <- pretrend_test(es_df_main, label = "Main", att_obj = res_main$att, min_pre = -6, cutoff = -3)

# Print event-study coefficients for -6 to +6 to diagnose anticipation and compare with SA
cat("\n=== CS Ant=2 Event-Study Coefficients (-6 to +6) ===\n")
es_df_main_display <- es_df_main %>%
  filter(event_time >= -6 & event_time <= 6) %>%
  arrange(event_time) %>%
  select(event_time, estimate, se, ci_lower, ci_upper)

# Use as.data.frame() and explicit print to avoid na.print issues
cat("Event-time | Estimate | SE | CI_lower | CI_upper\n")
cat(paste(rep("-", 60), collapse = ""), "\n")
for (i in 1:nrow(es_df_main_display)) {
  row <- es_df_main_display[i, ]
  cat(sprintf("%10d | %8.4f | %7.4f | %8.4f | %8.4f\n",
              row$event_time, 
              ifelse(is.na(row$estimate), NA_real_, row$estimate),
              ifelse(is.na(row$se), NA_real_, row$se),
              ifelse(is.na(row$ci_lower), NA_real_, row$ci_lower),
              ifelse(is.na(row$ci_upper), NA_real_, row$ci_upper)))
}

cat("\n=== Full es_df_main (all event times, first 50 rows) ===\n")
cat("Event-time | Estimate | SE | CI_lower | CI_upper\n")
cat(paste(rep("-", 60), collapse = ""), "\n")
es_df_main_head <- head(es_df_main, 50)
for (i in 1:nrow(es_df_main_head)) {
  row <- es_df_main_head[i, ]
  cat(sprintf("%10d | %8.4f | %7.4f | %8.4f | %8.4f\n",
              row$event_time, 
              ifelse(is.na(row$estimate), NA_real_, row$estimate),
              ifelse(is.na(row$se), NA_real_, row$se),
              ifelse(is.na(row$ci_lower), NA_real_, row$ci_lower),
              ifelse(is.na(row$ci_upper), NA_real_, row$ci_upper)))
}

cat("\n=== Summary statistics ===\n")
cat(sprintf("Event times range: [%d, %d]\n", 
            min(es_df_main$event_time, na.rm = TRUE), 
            max(es_df_main$event_time, na.rm = TRUE)))
cat(sprintf("Number of event-time points: %d\n", nrow(es_df_main)))
cat(sprintf("\nPost 1-6 month coefficients:\n"))
post_1_6 <- es_df_main %>% filter(event_time >= 1 & event_time <= 6)
cat("Event-time | Estimate | SE\n")
cat(paste(rep("-", 40), collapse = ""), "\n")
for (i in 1:nrow(post_1_6)) {
  row <- post_1_6[i, ]
  cat(sprintf("%10d | %8.4f | %7.4f\n",
              row$event_time,
              ifelse(is.na(row$estimate), NA_real_, row$estimate),
              ifelse(is.na(row$se), NA_real_, row$se)))
}
cat(sprintf("\nPost 1-6 average (from es_df_main): %.4f\n", 
            mean(post_1_6$estimate, na.rm = TRUE)))
cat(sprintf("Post 1-6 average (from cs_post_avg calculation): %.4f\n", cs_post_avg))

# Save pre-trend test results
write_csv(pt_main, file.path(outdir, "pretrend_tests.csv"))
cat("[✓] Pre-trend test results saved to pretrend_tests.csv\n")

make_es_plot(
  es_df_main,
  title = "Event-Study: ABAWD Enforcement and SNAP Participation",
  subtitle = "CS DR-DiD, not-yet-treated control, anticipation=2 (includes -2, -1 in treatment)",
  file_name = "fig_es_main.png"
)



############################################################
# 2. ROBUSTNESS CHECKS (FINAL & CLEAN)
############################################################

cat("\n╔", strrep("═", 60), "╗\n", sep = "")
cat("║", sprintf("%-60s", " 2. ROBUSTNESS CHECKS"), "║\n", sep = "")
cat("╚", strrep("═", 60), "╝\n\n", sep = "")

results_list <- list()

# Store Baseline Results (from Main Specification)
results_list[["Main (CS, Ant=2)"]] <- list(
  att = overall_main$overall.att, 
  se  = overall_main$overall.se
)

# Store CS post 1-6 month window average (for fair comparison with SA)
results_list[["CS post 1-6mo avg"]] <- list(
  att = cs_post_avg,
  se  = cs_post_se
)

# =========================================================
# [A] Robustness: Anticipation = 0 (No Anticipation)
#     Goal: Show sensitivity to anticipation assumption
#     This defines treatment as enforcement month only (Ant=0)
# =========================================================
cat("\n[A] Robustness: Anticipation = 0 (No Anticipation)...\n")

res_ant0 <- run_cs_did(df_main, anticipation = 0)
results_list[["Robustness: Ant=0 (no anticipation)"]] <- list(
  att = res_ant0$overall$overall.att,
  se  = res_ant0$overall$overall.se
)

cat(sprintf("    ATT (Ant=0): %.4f (SE=%.4f)\n", 
            res_ant0$overall$overall.att, res_ant0$overall$overall.se))

# Plot event study with Ant=0 for comparison
es_ant0 <- aggte(res_ant0$att, type = "dynamic", cband = TRUE, 
                 balance_e = 6, min_e = -6, max_e = 6)
es_df_ant0 <- event_study_df(es_ant0)
make_es_plot(
  es_df_ant0,
  title = "Event-Study: ABAWD Enforcement (Robustness: Ant=0)",
  subtitle = "CS DR-DiD, not-yet-treated control, anticipation=0 (treatment = enforcement month)",
  file_name = "fig_es_main_anti0.png"
)

# =========================================================
# [B] Anticipation Sensitivity Scan (0 to 6 months)
#     Goal: Validate anticipation effects and show sensitivity
# =========================================================
cat("\n[B] Anticipation Sensitivity Scan (0-6 months)...\n")

ant_scan_list <- list()

for (k in 0:6) {
  cat(sprintf("    Running Anticipation = %d...\n", k))
  res_k <- run_cs_did(df_main, anticipation = k)
  
  # Store for plotting
  ant_scan_list[[paste0(k)]] <- data.frame(
    Horizon  = k,
    ATT      = res_k$overall$overall.att,
    SE       = res_k$overall$overall.se
  )
  
  # Store specific key horizons (0 and 6) for the main results table
  if (k %in% c(0, 6)) {
    results_list[[paste0("Anticipation = ", k)]] <- list(
      att = res_k$overall$overall.att,
      se  = res_k$overall$overall.se
    )
  }
}

# --- Plotting the Anticipation Sensitivity ---
df_ant_scan <- bind_rows(ant_scan_list) %>%
  mutate(
    CI_lower = ATT - 1.96 * SE,
    CI_upper = ATT + 1.96 * SE
  )

p_sens <- ggplot(df_ant_scan, aes(x = Horizon, y = ATT)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), fill = "#0072B2", alpha = 0.2) +
  geom_line(color = "#0072B2", linewidth = 1) +
  geom_point(size = 3, color = "#0072B2") +
  geom_vline(xintercept = 2, linetype = "dotted", color = "red") +
  annotate("text", x = 2.1, y = max(df_ant_scan$CI_upper), 
           label = "Main (Ant=2)", color = "red", hjust = 0) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "blue", alpha = 0.5) +
  annotate("text", x = 0.1, y = max(df_ant_scan$CI_upper) * 0.9, 
           label = "Robustness (Ant=0)", color = "blue", hjust = 0, alpha = 0.7) +
  scale_x_continuous(breaks = 0:6, name = "Anticipation Horizon (Months)") +
  labs(
    title = "Sensitivity of ATT to Anticipation Horizon",
    subtitle = "Validating the timing of behavioral response",
    y = "Average Treatment Effect (ATT)"
  ) +
  theme_bw(base_size = 14)

ggsave(file.path(outdir, "fig_anticipation_sensitivity.png"), p_sens, width = 8, height = 5)
cat("[✓] Sensitivity Plot saved: fig_anticipation_sensitivity.png\n")


# =========================================================
# [C] Sensitivity: Control Group Selection
#     Goal: Ensure results don't depend on control definition
# =========================================================
cat("\n[C] Sensitivity: Never-treated Control...\n")

if (any(df_main$G_int == 0)) {
  res_nc <- run_cs_did(df_main, control_group = "nevertreated", anticipation = 2)
  results_list[["Never-treated Control"]] <- list(
    att = safe_val(res_nc$overall$overall.att),
    se  = safe_val(res_nc$overall$overall.se)
  )
} else {
  cat("    [!] Skipped: No never-treated units found.\n")
}


# =========================================================
# [D] Sensitivity: Outliers (Drop Wayne County)
#     Goal: Ensure results aren't driven by one large county
# =========================================================
cat("\n[D] Sensitivity: Excluding Wayne County (Outlier)...\n")

# Drop Wayne County: safely check columns that may or may not exist
drop_ids <- c("Wayne", "Wayne County", "26163", "260163")
df_drop <- df_main

# Check and filter by id column if it exists
if ("id" %in% names(df_drop)) {
  df_drop <- df_drop %>% filter(!id %in% drop_ids)
}

# Check and filter by county column if it exists
if ("county" %in% names(df_drop)) {
  df_drop <- df_drop %>% filter(!county %in% c("Wayne", "Wayne County"))
}

# Check and filter by county_id column if it exists
if ("county_id" %in% names(df_drop)) {
  df_drop <- df_drop %>% filter(!county_id %in% c("26163", "260163"))
}

if (nrow(df_drop) < nrow(df_main)) {
  res_drop <- run_cs_did(df_drop, anticipation = 2)
  results_list[["Excl. Wayne County"]] <- list(
    att = safe_val(res_drop$overall$overall.att),
    se  = safe_val(res_drop$overall$overall.se)
  )
} else {
  cat("    [!] Skipped: Wayne County ID not found.\n")
}


# =========================================================
# [E] Method: Sun & Abraham (2021)
#     Goal: Robustness to alternative staggered estimator
#     Aligned to Ant=2: treatment effectively starts at g-2
# =========================================================
cat("\n[E] Method: Sun & Abraham (2021), aligned to Ant=2...\n")

# IMPORTANT: Set never-treated cohorts to a far-future value (recommended in fixest examples)
# This ensures never-treated units only have negative relative periods and serve as controls/references
t_max <- max(df_main$t, na.rm = TRUE)
df_sa <- df_main %>%
  mutate(G_sa = ifelse(G_int == 0L, t_max + 1000L, G_int))

tryCatch({
  # CRITICAL: ref.p = c(-3, -1) aligns with Ant=2 + universal base
  # This allows estimation of e=-2, -1 while keeping -3 as reference
  # The two reference periods help with identification
  sa_model <- feols(
    Y ~ unemployment_rate + sunab(G_sa, t, ref.p = c(-3, -1)) | id_num + t,
    data = df_sa,
    cluster = ~ id_num
  )

  # ---- Full ATT (reference) ----
  sa_tot <- summary(sa_model, agg = "ATT")
  sa_att_full <- as.numeric(sa_tot$coeftable["ATT", 1])
  sa_se_full  <- as.numeric(sa_tot$coeftable["ATT", 2])

  cat(sprintf("    SA full ATT (reference): %.4f (SE=%.4f)\n", sa_att_full, sa_se_full))

  results_list[["Sun & Abraham (2021, full ATT)"]] <- list(
    att = sa_att_full,
    se  = sa_se_full
  )

  # ---- Extract event-time coefficients using iplot (most robust method) ----
  # Use fixest's built-in iplot with only.params=TRUE to get event-time values
  # This avoids fragile regex parsing and works across fixest versions
  ip <- iplot(sa_model, only.params = TRUE)
  
  # ip is a list with $x (event times) and $y (estimates)
  sa_es <- data.frame(
    event_time = ip$x,
    estimate   = ip$y,
    stringsAsFactors = FALSE
  )
  
  # Build coefficient names using standard fixest naming: "t::k"
  sa_es$coef_name <- paste0("t::", sa_es$event_time)
  
  # Display window (-6 to +6) for diagnostics
  sa_es_win <- sa_es %>% filter(event_time >= -6 & event_time <= 6) %>% arrange(event_time)
  
  cat("\n    SA event-study coefficients (-6 to +6):\n")
  cat("    Event-time | Estimate\n")
  cat("    ", paste(rep("-", 30), collapse = ""), "\n")
  for (i in 1:nrow(sa_es_win)) {
    cat(sprintf("    %10d | %8.4f\n", sa_es_win$event_time[i], sa_es_win$estimate[i]))
  }
  
  # ---- Post 1-6 average with correct SE using vcov ----
  # Get coefficient names for post 1-6
  post_terms <- paste0("t::", 1:6)
  
  b <- coef(sa_model)
  V <- as.matrix(vcov(sa_model))
  
  # Keep only terms that exist in both coef and vcov
  post_terms <- post_terms[post_terms %in% names(b)]
  post_terms <- post_terms[post_terms %in% rownames(V)]
  
  if (length(post_terms) == 0) {
    cat("    [!] Warning: Could not find post 1-6 coefficients in model.\n")
    cat("    Available coefficient names (first 30):", paste(head(names(b), 30), collapse = ", "), "\n")
    stop("Post 1-6 coefficients not found. Check coefficient names above.")
  }
  
  # Equal-weight average
  w <- rep(1 / length(post_terms), length(post_terms))
  att_post <- sum(w * b[post_terms])
  
  # SE: linear combination using vcov (correct method)
  Vsub <- V[post_terms, post_terms, drop = FALSE]
  se_post <- sqrt(as.numeric(t(w) %*% Vsub %*% w))

  results_list[["Sun & Abraham (2021, post 1-6mo)"]] <- list(
    att = as.numeric(att_post),
    se  = as.numeric(se_post)
  )

  cat(sprintf("\n    SA post 1-6mo avg: %.4f (SE=%.4f, using vcov linear combination)\n", 
              att_post, se_post))
  
  # Direct comparison: CS post 1-6 vs SA post 1-6 (same window, fair comparison)
  cat("\n    === FAIR COMPARISON: Same Window (Post 1-6 months) ===\n")
  cat(sprintf("    CS post 1-6 avg:  %.4f (SE=%.4f)\n", cs_post_avg, cs_post_se))
  cat(sprintf("    SA post 1-6 avg:  %.4f (SE=%.4f)\n", att_post, se_post))
  cat(sprintf("    Difference:       %.4f\n", att_post - cs_post_avg))
  cat(sprintf("    Relative diff:   %.1f%%\n", 100 * (att_post - cs_post_avg) / abs(cs_post_avg)))
  if (sign(cs_post_avg) == sign(att_post)) {
    cat("    ✓ Sign consistent (both methods agree on direction)\n")
  } else {
    cat("    ⚠ Sign inconsistent (methods disagree on direction - investigate further)\n")
  }
  cat("\n    Note: CS overall ATT and SA full ATT may differ more due to different aggregation windows.\n")
  cat("    The post 1-6 comparison above is the fair comparison using the same time window.\n\n")
  
  # ---- Alternative: Estimate post 1-6 as a single binned coefficient ----
  # This uses sunab's bin.rel feature to directly estimate the post 1-6 average
  # This is more efficient and avoids manual aggregation
  cat("\n    Alternative: Estimating post 1-6 as binned coefficient...\n")
  
  tryCatch({
    sa_model_bin <- feols(
      Y ~ unemployment_rate + 
        sunab(G_sa, t, ref.p = c(-3, -1), bin.rel = list("post_1_6" = 1:6)) | 
        id_num + t,
      data = df_sa,
      cluster = ~ id_num
    )
    
    # Extract the binned coefficient
    ct_bin <- coeftable(sa_model_bin)
    bin_coef_name <- rownames(ct_bin)[grepl("post_1_6|bin", rownames(ct_bin), ignore.case = TRUE)]
    
    if (length(bin_coef_name) > 0) {
      att_bin <- ct_bin[bin_coef_name[1], "Estimate"]
      se_bin  <- ct_bin[bin_coef_name[1], "Std. Error"]
      cat(sprintf("    SA post 1-6 (binned): %.4f (SE=%.4f)\n", att_bin, se_bin))
      
      # Store as alternative estimate
      results_list[["Sun & Abraham (2021, post 1-6mo, binned)"]] <- list(
        att = as.numeric(att_bin),
        se  = as.numeric(se_bin)
      )
    } else {
      cat("    [!] Could not find binned coefficient. Using aggregated estimate above.\n")
    }
  }, error = function(e) {
    cat("    [!] Binned estimation failed:", conditionMessage(e), "\n")
    cat("    Using aggregated estimate from iplot method above.\n")
  })

  # Plot SA to visualize the anticipation drop
  png(file.path(outdir, "fig_es_sunab_ant.png"), width = 900, height = 600)
  iplot(sa_model, main = "Sun & Abraham (Ref = -3, -1), aligned to Ant=2",
        xlab = "Months relative to enforcement")
  dev.off()

}, error = function(e) cat("[!] SA Failed:", conditionMessage(e), "\n"))


# =========================================================
# [F] Method: Static TWFE (Naive Benchmark)
#     Goal: Show bias of traditional method vs CS
#     Note: TWFE is biased under staggered adoption, but we align it to Ant=2 for fair comparison
# =========================================================
cat("\n[F] Method: Static TWFE (Biased Benchmark, aligned to Ant=2)...\n")

# Treatment indicator aligned to Ant=2: shift treatment start forward by 2 months
# This makes TWFE estimate the same "treatment window" as CS with anticipation=2
df_twfe <- df_main %>%
  mutate(
    treat_post_ant0 = ifelse(G_int > 0 & t >= G_int, 1, 0),  # Ant=0 (for comparison)
    treat_post_ant2 = ifelse(G_int > 0 & t >= (G_int - 2), 1, 0)  # Ant=2 (aligned with main spec)
  )

tryCatch({
  # TWFE with Ant=2 (aligned with main CS specification)
  twfe_ant2 <- feols(
    Y ~ treat_post_ant2 + unemployment_rate | id_num + t,
    data = df_twfe, cluster = ~ id_num
  )
  
  results_list[["Static TWFE (Ant=2, aligned)"]] <- list(
    att = coef(twfe_ant2)["treat_post_ant2"],
    se  = se(twfe_ant2)["treat_post_ant2"]
  )
  
  cat(sprintf("    TWFE ATT (Ant=2): %.4f (SE=%.4f)\n", 
              coef(twfe_ant2)["treat_post_ant2"], 
              se(twfe_ant2)["treat_post_ant2"]))
  
  # Also report Ant=0 for comparison
  twfe_ant0 <- feols(
    Y ~ treat_post_ant0 + unemployment_rate | id_num + t,
    data = df_twfe, cluster = ~ id_num
  )
  
  results_list[["Static TWFE (Ant=0, comparison)"]] <- list(
    att = coef(twfe_ant0)["treat_post_ant0"],
    se  = se(twfe_ant0)["treat_post_ant0"]
  )
  
  cat(sprintf("    TWFE ATT (Ant=0, comparison): %.4f (SE=%.4f)\n", 
              coef(twfe_ant0)["treat_post_ant0"], 
              se(twfe_ant0)["treat_post_ant0"]))
  
}, error = function(e) cat("[!] TWFE Failed:", conditionMessage(e), "\n"))


# =========================================================
# [G] Functional Form: Participation Rate (Level)
#     Goal: Ensure results hold for Rate, not just Log
# =========================================================
cat("\n[G] Functional Form: Participation Rate (Level)...\n")

# Use original rate column directly instead of reverse-transforming from log
# This avoids double-scaling issues. Check if y_per1k_18_49 exists.
if ("y_per1k_18_49" %in% names(df_main)) {
  df_rate <- df_main %>%
    mutate(Y = y_per1k_18_49) %>%  # Use original rate directly
    filter(is.finite(Y), !is.na(Y))
} else if ("y_raw" %in% names(df_main) && "population_18_49" %in% names(df_main)) {
  # Fallback: calculate from raw recipients if rate column doesn't exist
  df_rate <- df_main %>%
    mutate(
      pop_safe = ifelse(population_18_49 < 1, 1, population_18_49),
      Y = (y_raw / pop_safe) * 1000
    ) %>%
    filter(is.finite(Y), !is.na(Y))
} else {
  cat("    [!] Skipped: Neither y_per1k_18_49 nor y_raw available for rate check.\n")
  df_rate <- NULL
}

if (!is.null(df_rate) && nrow(df_rate) > 0) {
  tryCatch({
    res_rate <- run_cs_did(df_rate, anticipation = 2)
    
    cat(sprintf("    Rate ATT: %.4f (SE=%.4f)\n", 
                res_rate$overall$overall.att, res_rate$overall$overall.se))
    
    # Note: We do NOT add this to results_list for the Forest Plot 
    # because the y-axis scale is completely different (Level vs Log).
  }, error = function(e) cat("[!] Rate Check Failed:", conditionMessage(e), "\n"))
} else {
  cat("    [!] Rate check skipped: insufficient data.\n")
}


############################################################
# 3. RESULTS TABLE & EXPORT
############################################################

cat("\n╔", strrep("═", 60), "╗\n", sep = "")
cat("║", sprintf("%-60s", " 3. FINAL RESULTS TABLE"), "║\n", sep = "")
cat("╚", strrep("═", 60), "╝\n\n", sep = "")

# Build results table, filtering out any NULL or invalid entries
results_df <- bind_rows(lapply(names(results_list), function(spec) {
  res <- results_list[[spec]]
  if (is.null(res) || is.null(res$att) || is.null(res$se) || 
      !is.finite(res$att) || !is.finite(res$se)) {
    return(NULL)
  }
  data.frame(
    Specification = spec,
    ATT = res$att,
    SE  = res$se
  )
})) %>%
  filter(is.finite(ATT), is.finite(SE)) %>%
  mutate(
    CI_lower = ATT - 1.96 * SE,
    CI_upper = ATT + 1.96 * SE,
    t_stat   = abs(ATT / SE),
    p_value  = 2 * (1 - pnorm(t_stat)),
    Sig      = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.10 ~ "*",
      TRUE ~ ""
    )
  )

# Print Table
print(kable(results_df, digits = 4, format = "simple"))

# Save CSV
write_csv(results_df, file.path(outdir, "robustness_complete.csv"))
cat("[✓] Table saved to robustness_complete.csv\n")


############################################################
# 4. FOREST PLOT
############################################################

cat("\n[✓] Generating Forest Plot...\n")

plot_data <- results_df %>%
  mutate(
    spec_num = row_number(),
    # Grouping for color logic in plot
    category = case_when(
      grepl("Main", Specification) ~ "Main",
      grepl("Sun|TWFE", Specification) ~ "Method",
      TRUE ~ "Sensitivity"
    )
  )

p_forest <- ggplot(plot_data, aes(x = ATT, y = reorder(Specification, -spec_num))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  geom_errorbar(aes(xmin = CI_lower, xmax = CI_upper), 
                orientation = "y", width = 0.3, color = "gray50") +
  geom_point(aes(color = category, size = category)) +
  scale_color_manual(values = c(
    "Main"        = "#D55E00", # Orange (Highlight)
    "Method"      = "#009E73", # Green (Alternative Estimators)
    "Sensitivity" = "#56B4E9"  # Blue (Standard Checks)
  )) +
  scale_size_manual(values = c("Main"=4.5, "Method"=3.5, "Sensitivity"=3)) +
  labs(
    title = "Robustness Checks: ABAWD Enforcement",
    subtitle = "ATT on log(1+SNAP Participation) across specifications",
    x = "Average Treatment Effect on Treated (ATT)",
    y = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave(file.path(outdir, "fig_forest_plot.png"), p_forest, width = 10, height = 7, dpi = 300)
cat("[✓] Forest Plot saved to fig_forest_plot.png\n")


############################################################
# 5. DDD: Adult vs Child (third difference)
############################################################
# Standard definition: DDD = DiD_adult - DiD_child. Negative => policy reduced adult participation more.
# Preferred: single regression with stacked data and triple-interaction event-study:
#   Y_ctg = sum_k θ_k (D_{c,t}^k × G_g) + μ_cg + λ_tg + ε_ctg
# so θ_k are event-study DDDs directly (no post-difference or covariance handling).
# FE: μ_cg = county×group, λ_tg = time×group.

cat("\n╔", strrep("═", 60), "╗\n", sep = "")
cat("║", sprintf("%-60s", " 5. DDD (ADULT vs CHILD)"), "║\n", sep = "")
cat("╚", strrep("═", 60), "╝\n\n", sep = "")

ddd_cols_need <- c("adult_recipients", "child_recipients",
                   "date", "id", "unemployment_rate")
if (!all(ddd_cols_need %in% names(panel_raw))) {
  cat("[!] DDD skipped: panel_with_G is missing adult/child columns.\n")
  cat("    Required:", paste(ddd_cols_need, collapse = ", "), "\n")
} else {
  COVARS_DDD <- c("unemployment_rate")
  start_date <- as.Date("2014-01-01")
  end_date   <- as.Date("2019-12-01")

  wide_ddd <- panel_raw %>%
    mutate(
      date   = as.Date(date),
      t      = year(date) * 12L + month(date),
      id_num = as.integer(factor(id)),
      G_int  = case_when(
        is.na(G) ~ 0L,
        G == "0" ~ 0L,
        TRUE ~ {
          g_chr <- as.character(G)
          yy    <- as.integer(substr(g_chr, 1, 4))
          mm    <- as.integer(substr(g_chr, 6, 7))
          ifelse(is.na(yy) | is.na(mm), 0L, yy * 12L + mm)
        }
      ),
      adult_recipients = as.numeric(adult_recipients),
      child_recipients = as.numeric(child_recipients),
      population_18_49 = as.numeric(population_18_49),
      unemployment_rate = as.numeric(unemployment_rate)
    ) %>%
    filter(date >= start_date, date <= end_date) %>%
    filter(!is.na(adult_recipients), !is.na(child_recipients),
           adult_recipients >= 0, child_recipients >= 0) %>%
    filter(complete.cases(across(all_of(COVARS_DDD))))

  cat(sprintf("    DDD wide panel: %d county-months\n", nrow(wide_ddd)))

  tryCatch({
    # Stacked data: two rows per (county, month), g in {adult, child}
    # G_sa: never-treated get max(t)+1000 so event_time is always "pre"
    wide_ddd <- wide_ddd %>%
      mutate(
        Y_adult = log1p(adult_recipients),
        Y_child = log1p(child_recipients),
        G_sa    = ifelse(G_int == 0L, max(t, na.rm = TRUE) + 1000L, G_int),
        event_time_raw = as.integer(t - G_sa)
      )
    # Bin event time to [-6, 6] so we keep all obs and get one θ_k per k
    wide_ddd$event_time <- pmin(pmax(wide_ddd$event_time_raw, -6L), 6L)

    long_adult <- wide_ddd %>%
      transmute(id_num, t, event_time, G_g = 1L, Y = Y_adult,
                id_cg = paste(id_num, "adult"), t_g = paste(t, "adult"))
    long_child <- wide_ddd %>%
      transmute(id_num, t, event_time, G_g = 0L, Y = Y_child,
                id_cg = paste(id_num, "child"), t_g = paste(t, "child"))
    long_ddd <- bind_rows(long_adult, long_child)
    cat(sprintf("    DDD long (stacked): %d rows\n", nrow(long_ddd)))

    # Single regression: Y ~ i(event_time, ref=-1):G_g | county×group + time×group
    # θ_k = event-study DDD at k (no post-difference, SE from same model)
    mod_ddd <- feols(
      Y ~ i(event_time, ref = -1) : G_g | id_cg + t_g,
      data = long_ddd,
      cluster = ~ id_num
    )

    # Extract θ_k: fixest names like "event_time::-6:G_g"; parse event time robustly
    b <- coef(mod_ddd)
    V <- as.matrix(vcov(mod_ddd))
    nms <- names(b)[grepl("^event_time::", names(b))]
    evt_vals <- as.integer(regmatches(nms, regexpr("-?[0-9]+", nms)))
    ddd_es <- data.frame(
      event_time = evt_vals,
      estimate   = as.numeric(b[nms]),
      se         = sqrt(diag(V)[nms]),
      stringsAsFactors = FALSE
    ) %>%
      filter(!is.na(event_time)) %>%
      mutate(
        ci_lower = estimate - 1.96 * se,
        ci_upper = estimate + 1.96 * se
      ) %>%
      arrange(event_time)

    # Overall DDD (post 1-6 month average); SE from same-model vcov
    post_evt <- 1:6
    post_row <- ddd_es %>% filter(event_time %in% post_evt) %>% arrange(event_time)
    ddd_post_avg <- NA_real_
    ddd_post_se  <- NA_real_
    if (nrow(post_row) >= 1L) {
      w <- rep(1 / nrow(post_row), nrow(post_row))
      ddd_post_avg <- sum(w * post_row$estimate)
      post_nms <- paste0("event_time::", post_row$event_time, ":G_g")
      post_nms <- post_nms[post_nms %in% rownames(V)]
      if (length(post_nms) == length(w)) {
        Vsub <- V[post_nms, post_nms, drop = FALSE]
        ddd_post_se <- sqrt(drop(t(w) %*% Vsub %*% w))
      } else {
        ddd_post_se <- sqrt(sum(w^2 * post_row$se^2))
      }
      results_list[["DDD (adult−child, post 1-6)"]] <- list(att = ddd_post_avg, se = ddd_post_se)
    }

    cat("\n--- DDD result (adult − child) ---\n")
    cat(sprintf("  Post 1-6 month average: % .4f (SE = %.4f)\n", ddd_post_avg, ddd_post_se))
    cat("  [Negative => policy reduced adult participation more than child.]\n\n")
    cat("=== DDD event-study (θ_k by event time k) ===\n")
    print(ddd_es)
    cat("\n")

    # Plot
    ddd_es_plot <- ddd_es %>% filter(event_time >= -6, event_time <= 6)
    p_ddd <- ggplot(ddd_es_plot, aes(x = event_time, y = estimate)) +
      geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "gray85", alpha = 0.7) +
      geom_line(linewidth = 1.2, color = "#0072B2") +
      geom_point(size = 2.6, color = "#0072B2") +
      geom_vline(xintercept = 0, color = "black", linewidth = 0.9) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      scale_x_continuous(breaks = seq(-6, 6, by = 2)) +
      labs(
        title = "DDD Event-Study: ABAWD Enforcement (Adult vs Child)",
        subtitle = "Single regression: θ_k = (D^k × G_g); FE = county×group + time×group",
        x = "Months relative to first enforcement",
        y = "DDD (adult − child)"
      ) +
      theme_bw(base_size = 14) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold")
      )
    ggsave(file.path(outdir, "fig_es_ddd_adult_child.png"), p_ddd, width = 8, height = 5, dpi = 300)
    cat("[✓] DDD event-study plot saved: fig_es_ddd_adult_child.png\n")

    write_csv(ddd_es, file.path(outdir, "ddd_event_study.csv"))
    cat("[✓] DDD event-study coefficients saved: ddd_event_study.csv\n")
  }, error = function(e) {
    cat("[!] DDD estimation failed:", conditionMessage(e), "\n")
  })
}

cat("\n╔", strrep("═", 60), "╗\n", sep = "")
cat("║", sprintf("%-60s", " ANALYSIS COMPLETE"), "║\n", sep = "")
cat("╚", strrep("═", 60), "╝\n\n", sep = "")

