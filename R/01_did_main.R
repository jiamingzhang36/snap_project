############################################################
# Main DiD Analysis: CS + Sun-Abraham + Imputation + Stacked
# ABAWD Enforcement and SNAP Participation
#
# Design: Main CS (Ant=2) + Alternative estimators
# Main estimand: Post 1-3 month average (so 2018-07 cohort contributes)
# NOTE: Post 1-6 only covers early cohorts due to Michigan-only design limits
############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(readr)
  library(did)
  library(fixest)
  library(ggplot2)
  library(tidyr)
  library(tibble)  # For tibble() function
  library(didimputation)  # CRAN package for BJS imputation
})

# Set seed for reproducibility
set.seed(123)

#############################
# Setup
#############################

root   <- "/Users/jiamingzhang/Desktop/snap_project"
outdir <- file.path(root, "outputs", "step1_did")
infile <- file.path(root, "data_clean", "panel_with_G.csv")

if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
stopifnot(file.exists(infile))

panel_raw <- read_csv(infile, show_col_types = FALSE)

############################################################
# Helper Functions
############################################################

# Build analysis dataframe
build_df <- function(panel_raw, 
                     y_col = "y_per1k_18_49",
                     start_date = as.Date("2014-01-01"),
                     end_date   = as.Date("2019-12-01"),
                     weight_col = NULL) {
  
  df <- panel_raw %>%
    mutate(
      date = as.Date(date),
      t    = year(date) * 12L + (month(date) - 1L),
      id_num = as.integer(factor(id)),
      G_int = case_when(
        is.na(G) | G == "0" ~ 0L,
        TRUE ~ {
          yy <- as.integer(substr(as.character(G), 1, 4))
          mm <- as.integer(substr(as.character(G), 6, 7))
          ifelse(is.na(yy) | is.na(mm), 0L, yy * 12L + (mm - 1L))
        }
      ),
      unemployment_rate = as.numeric(unemployment_rate),
      log_lf = log1p(as.numeric(labor_force)),
      pop_1849 = as.numeric(population_18_49),
      Y = log1p(as.numeric(.data[[y_col]]))
    ) %>%
    filter(date >= start_date, date <= end_date) %>%
    filter(is.finite(Y), !is.na(Y)) %>%
    filter(complete.cases(unemployment_rate, log_lf))
  
  # Add weights if specified
  if (!is.null(weight_col) && weight_col %in% names(panel_raw)) {
    df <- df %>% mutate(weight = as.numeric(.data[[weight_col]]))
  } else if (!is.null(weight_col) && weight_col == "pop_1849") {
    df <- df %>% mutate(weight = pop_1849)
  } else {
    df <- df %>% mutate(weight = 1)
  }
  
  cat(sprintf("[build] Rows: %d | Units: %d | Treated: %s\n",
              nrow(df), n_distinct(df$id_num), any(df$G_int > 0)))
  
  df
}

# Extract post-window average from CS/Imputation results
post_window_avg <- function(att_obj, e_min = 1, e_max = 6) {
  es <- aggte(att_obj, type = "dynamic", cband = FALSE, balance_e = e_max)
  idx <- which(es$egt >= e_min & es$egt <= e_max)
  
  if (length(idx) == 0) {
    return(c(att = NA_real_, se = NA_real_))
  }
  
  # SE with covariance if available
  V <- NULL
  if (!is.null(es$V_egt)) V <- es$V_egt
  if (is.null(V) && !is.null(es$V)) V <- es$V
  
  att_hat <- mean(es$att.egt[idx], na.rm = TRUE)
  
  if (!is.null(V) && length(idx) > 0) {
    k <- length(idx)
    w <- rep(1/k, k)
    Vsub <- V[idx, idx, drop = FALSE]
    se_hat <- sqrt(drop(t(w) %*% Vsub %*% w))
  } else {
    k <- length(idx)
    se_hat <- sqrt(sum(es$se.egt[idx]^2, na.rm = TRUE)) / k
  }
  
  c(att = att_hat, se = se_hat)
}

# Event-study dataframe for plotting
event_study_df <- function(es) {
  crit <- if (!is.null(es$crit.val.egt)) es$crit.val.egt else 1.96
  data.frame(
    event_time = es$egt,
    estimate   = es$att.egt,
    se         = es$se.egt,
    ci_lower   = es$att.egt - crit * es$se.egt,
    ci_upper   = es$att.egt + crit * es$se.egt
  )
}

# Plot event-study
make_es_plot <- function(es_df, title, subtitle, file_name, 
                         min_pre = -6, max_post = 6) {
  es_df_win <- es_df %>%
    filter(event_time >= min_pre & event_time <= max_post)
  
  p <- ggplot(es_df_win, aes(x = event_time, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = "red", alpha = 0.5) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
                fill = "#0072B2", alpha = 0.2) +
    geom_line(color = "#0072B2", linewidth = 1) +
    geom_point(size = 2.5, color = "#0072B2") +
    scale_x_continuous(breaks = seq(min_pre, max_post, by = 2),
                       name = "Months relative to enforcement") +
    labs(
      title = title,
      subtitle = subtitle,
      y = "ATT on log(1 + recipients per 1k, 18–49)"
    ) +
    theme_bw(base_size = 12)
  
  ggsave(file.path(outdir, file_name), p, width = 9, height = 6, dpi = 300)
  cat(sprintf("[✓] Event-study plot saved: %s\n", file_name))
}

############################################################
# 1. MAIN: Callaway-Sant'Anna DR-DiD
############################################################

# MAIN DEFINITION: Enforcement month + anticipation
# Use enforcement month (G_int) and exclude the 2 months before enforcement
# to account for pre-implementation behavioral responses.
G_USE <- "G_int"  # Enforcement month (from cleaned data)
ANT_USE <- 2L     # Anticipation window (months before enforcement)

cat("\n╔", strrep("═", 60), "╗\n", sep = "")
cat("║", sprintf("%-60s", " MAIN: CS DR-DiD (G=enforcement, Ant=2)"), "║\n", sep = "")
cat("╚", strrep("═", 60), "╝\n\n", sep = "")

# Build main dataframe
Y_MAIN <- "y_per1k_18_49"
df_main <- build_df(panel_raw, y_col = Y_MAIN)

cat("Main specification: G = enforcement month, anticipation = 2\n")
cat("This excludes the two months before enforcement from the comparison set.\n")
cat("Rationale: Early behavioral responses likely begin ~2 months before enforcement.\n\n")

# Ensure G_USE exists in df_main
stopifnot(G_USE %in% names(df_main))

# Check cohort sizes to identify tiny cohorts (potential issue for CS estimation)
# IMPORTANT: Count unique counties per cohort, not total rows
cat("\nCohort size check (by G_int, unique counties per cohort):\n")
cohort_sizes <- df_main %>%
  filter(.data[[G_USE]] > 0) %>%
  distinct(id_num, .data[[G_USE]]) %>%
  count(.data[[G_USE]], name = "n_counties") %>%
  arrange(n_counties)
print(cohort_sizes, n = 50)

# Warn about tiny cohorts
tiny_cohorts <- cohort_sizes %>% filter(n_counties <= 2)
if (nrow(tiny_cohorts) > 0) {
  cat(sprintf("\n⚠️  WARNING: Found %d tiny cohort(s) (n_counties <= 2):\n", nrow(tiny_cohorts)))
  print(tiny_cohorts)
  cat("These tiny cohorts may cause issues in CS estimation.\n")
  cat("Consider: (1) reporting group-specific effects, (2) leave-one-county-out sensitivity checks.\n\n")
} else {
  # Check for small cohorts (3-6 counties) which may still need attention
  small_cohorts <- cohort_sizes %>% filter(n_counties >= 3, n_counties <= 6)
  if (nrow(small_cohorts) > 0) {
    cat(sprintf("\n⚠️  NOTE: Found %d small cohort(s) (3-6 counties):\n", nrow(small_cohorts)))
    print(small_cohorts)
    cat("These small cohorts may benefit from group-specific effects and sensitivity checks.\n\n")
  } else {
    cat("\n✅ All cohorts have reasonable size (n_counties > 6).\n\n")
  }
}

# Main CS specification: Enforcement month with anticipation = 2
# Supports population weighting via weightsname parameter
# Includes fallback for singular design matrix (common with small cohorts)
run_cs <- function(df, gname = G_USE, control_group = "notyettreated", 
                   anticipation = ANT_USE, weightsname = NULL) {
  # Try DR method first (preferred)
  att <- tryCatch({
    att_gt(
      yname = "Y",
      tname = "t",
      idname = "id_num",
      gname = gname,
      xformla = ~ unemployment_rate + log_lf,
      data = df,
      panel = TRUE,
      control_group = control_group,
      est_method = "dr",
      anticipation = anticipation,
      allow_unbalanced_panel = TRUE,
      weightsname = weightsname
    )
  }, error = function(e) {
    # If DR fails due to singular matrix, try IPW (less efficient but more robust)
    if (grepl("singular|design matrix", conditionMessage(e), ignore.case = TRUE)) {
      cat("    [!] DR method failed (singular design matrix), trying IPW method...\n")
      tryCatch({
        att_gt(
          yname = "Y",
          tname = "t",
          idname = "id_num",
          gname = gname,
          xformla = ~ unemployment_rate + log_lf,
          data = df,
          panel = TRUE,
          control_group = control_group,
          est_method = "ipw",
          anticipation = anticipation,
          allow_unbalanced_panel = TRUE,
          weightsname = weightsname
        )
      }, error = function(e2) {
        # If IPW also fails, try without covariates
        cat("    [!] IPW method also failed, trying without covariates...\n")
        att_gt(
          yname = "Y",
          tname = "t",
          idname = "id_num",
          gname = gname,
          xformla = NULL,
          data = df,
          panel = TRUE,
          control_group = control_group,
          est_method = "ipw",
          anticipation = anticipation,
          allow_unbalanced_panel = TRUE,
          weightsname = weightsname
        )
      })
    } else {
      stop(e)  # Re-throw if it's a different error
    }
  })
  att
}

att_cs <- run_cs(df_main, gname = G_USE, anticipation = ANT_USE)
overall_cs <- aggte(att_cs, type = "simple")
es_cs <- aggte(att_cs, type = "dynamic", cband = TRUE, 
               balance_e = 6, min_e = -6, max_e = 6)

cat(sprintf("CS Overall ATT: %.4f (SE = %.4f)\n",
            overall_cs$overall.att, overall_cs$overall.se))

# MAIN ESTIMAND: Post 1-3 month average (so 2018-07 cohort contributes)
# NOTE: Post 1-6 excludes major 2018-07 cohort because by k=4+ there's no control
cs_1_3 <- post_window_avg(att_cs, e_min = 1, e_max = 3)
cat(sprintf("CS Post 1-3 avg (MAIN): %.4f (SE = %.4f)\n", cs_1_3["att"], cs_1_3["se"]))

# Secondary: Post 1-6 (only early cohorts contribute - for appendix)
cs_1_6 <- post_window_avg(att_cs, e_min = 1, e_max = 6)
cat(sprintf("CS Post 1-6 avg (early cohorts only): %.4f (SE = %.4f)\n", cs_1_6["att"], cs_1_6["se"]))

# DIAGNOSTIC: Event-time coverage by cohort
# Shows which cohorts contribute to each event time (critical for interpretation)
cat("\n[DIAGNOSTIC] Event-time coverage by cohort:\n")
att_df <- tibble(
  g   = att_cs$group,
  t   = att_cs$t,
  att = att_cs$att,
  se  = att_cs$se
) %>%
  mutate(e = t - g)

# Coverage table: which cohorts contribute to each event time
coverage <- att_df %>%
  filter(!is.na(att)) %>%
  group_by(e) %>%
  summarise(
    n_cohorts = n_distinct(g),
    cohorts = paste(sort(unique(g)), collapse = ","),
    n_cells = n(),
    .groups = "drop"
  ) %>%
  filter(e >= -6, e <= 6) %>%
  arrange(e)
print(coverage)

# Cohort-specific post 1-3 averages (main window)
cat("\n[DIAGNOSTIC] Cohort-specific post 1-3 averages:\n")
cohort_post13 <- att_df %>%
  filter(e >= 1, e <= 3) %>%
  group_by(g) %>%
  summarise(
    att_1_3 = mean(att, na.rm = TRUE),
    se_1_3  = sqrt(sum(se^2, na.rm = TRUE)) / n(),
    n_cells = sum(!is.na(att)),
    .groups = "drop"
  ) %>%
  arrange(g)
print(cohort_post13)

# Cohort-specific post 1-6 averages (for reference)
cat("\n[DIAGNOSTIC] Cohort-specific post 1-6 averages (early cohorts only):\n")
cohort_post16 <- att_df %>%
  filter(e >= 1, e <= 6) %>%
  group_by(g) %>%
  summarise(
    att_1_6 = mean(att, na.rm = TRUE),
    se_1_6  = sqrt(sum(se^2, na.rm = TRUE)) / n(),
    n_cells = sum(!is.na(att)),
    .groups = "drop"
  ) %>%
  arrange(g)
print(cohort_post16)

# Identify small cohort
small_cohort <- cohort_post16 %>% 
  left_join(cohort_sizes, by = c("g" = G_USE)) %>%
  filter(n_counties <= 4) %>%
  pull(g)

if (length(small_cohort) > 0) {
  cat(sprintf("\n⚠️  Small cohort detected: G_int = %s\n", paste(small_cohort, collapse = ", ")))
  cat("This cohort may be driving the overall CS estimate.\n")
}

# Create event-study dataframe FIRST (before using it in pretrend test)
es_df_cs <- event_study_df(es_cs)

# Pre-trend test: Try conditional_did_pretest() first, fallback to simplified test
# Note: conditional_did_pretest() requires data argument and may not support unbalanced panels
cat("\nPre-trend test:\n")
pretest_result <- NULL
tryCatch({
  # Try conditional_did_pretest() with data argument
  # This function may not work with unbalanced panels, so we wrap in tryCatch
  pretest_result <- conditional_did_pretest(
    yname = "Y",
    tname = "t",
    idname = "id_num",
    gname = G_USE,  # Use G_int (enforcement month)
    xformla = ~ unemployment_rate + log_lf,
    data = df_main,
    panel = TRUE,
    control_group = "notyettreated",
    est_method = "dr",
    anticipation = ANT_USE,  # anticipation = 2 with enforcement month
    allow_unbalanced_panel = TRUE
  )
  if (!is.null(pretest_result) && !is.null(pretest_result$p.value)) {
    cat(sprintf("  Conditional pre-test: p = %.4f\n", pretest_result$p.value))
  } else {
    cat("  Conditional pre-test: p-value not available\n")
  }
}, error = function(e) {
  cat("  Conditional pre-test not available (", conditionMessage(e), ")\n")
  cat("  Note: This function may not support unbalanced panels.\n")
  cat("  Using simplified Wald test as diagnostic...\n")
})

# Simplified Wald test as diagnostic (ignores covariance, use with caution)
# IMPORTANT: With anticipation=2, e=-2 and e=-1 are contaminated (excluded).
# Only test e ≤ -3.
es_df_cs_pre <- es_df_cs %>%
  filter(event_time <= -3) %>%  # Pre-periods: only e ≤ -3 (pure pre, no contamination)
  filter(!is.na(estimate), !is.na(se))

if (nrow(es_df_cs_pre) > 0) {
  # Simplified Wald test: sum of (estimate/se)^2 (ignores covariance)
  wald_stat <- sum((es_df_cs_pre$estimate / es_df_cs_pre$se)^2, na.rm = TRUE)
  p_wald <- 1 - pchisq(wald_stat, df = nrow(es_df_cs_pre))
  cat(sprintf("  Simplified Wald (pre-periods <= -3, ignores covariance): p = %.4f\n", p_wald))
  
  # Check if any pre-period coefficients are significantly different from zero
  sig_pre <- sum(abs(es_df_cs_pre$estimate / es_df_cs_pre$se) > 1.96, na.rm = TRUE)
  cat(sprintf("  Number of pre-periods with |t| > 1.96: %d out of %d\n", 
              sig_pre, nrow(es_df_cs_pre)))
  
  # Print pre-period coefficients for inspection
  cat("  Pre-period coefficients:\n")
  for (i in 1:nrow(es_df_cs_pre)) {
    cat(sprintf("    e = %2d: %.4f (SE=%.4f, t=%.2f)\n",
                es_df_cs_pre$event_time[i],
                es_df_cs_pre$estimate[i],
                es_df_cs_pre$se[i],
                es_df_cs_pre$estimate[i] / es_df_cs_pre$se[i]))
  }
} else {
  cat("  No pre-period coefficients available.\n")
}
make_es_plot(
  es_df_cs,
  title = "Event-Study: ABAWD Enforcement and SNAP Participation",
  subtitle = "CS DR-DiD, not-yet-treated control, anticipation = 2",
  file_name = "fig_es_cs_main.png"
)

# Store results
results_list <- list()
results_list[["Main: CS DR-DiD (post 1-3mo)"]] <- list(
  att = cs_1_3["att"],
  se  = cs_1_3["se"]
)
results_list[["Secondary: CS (post 1-6mo, early cohorts)"]] <- list(
  att = cs_1_6["att"],
  se  = cs_1_6["se"]
)

############################################################
# 2. ALTERNATIVE 1: Sun-Abraham (TWFE)
############################################################

cat("\n╔", strrep("═", 60), "╗\n", sep = "")
cat("║", sprintf("%-60s", " ALTERNATIVE 1: Sun-Abraham (TWFE)"), "║\n", sep = "")
cat("╚", strrep("═", 60), "╝\n\n", sep = "")

# Align with anticipation=2 by dropping rel = -2, -1
df_sa <- df_main %>%
  mutate(
    G_use = .data[[G_USE]],
    rel = t - .data[[G_USE]]
  ) %>%
  filter(rel <= -3 | rel >= 0)

sa_mod <- feols(
  Y ~ sunab(G_use, t, ref.p = -3) + unemployment_rate + log_lf | id_num + t,
  data = df_sa,
  cluster = ~ id_num
)

cat("Sun-Abraham model fit complete.\n")

# Extract Sun-Abraham event-time coefficients
b_sa <- coef(sa_mod)
V_sa <- vcov(sa_mod)
nm_sa <- names(b_sa)
sa_terms <- nm_sa[grepl("sunab", nm_sa)]

if (length(sa_terms) > 0) {
  # Ensure terms exist in vcov
  V_names <- names(sqrt(diag(V_sa)))
  sa_terms <- intersect(sa_terms, V_names)
  if (length(sa_terms) == 0) {
    cat("[!] Sun-Abraham coefficients found, but none matched vcov names.\n")
  }
  
  ev_sa <- suppressWarnings(as.integer(sub(".*::(-?\\d+)$", "\\1", sa_terms)))
  if (all(is.na(ev_sa))) {
    ev_sa <- suppressWarnings(as.integer(sub(".*t::(-?\\d+).*", "\\1", sa_terms)))
  }
  
  sa_df <- data.frame(
    term = sa_terms,
    event_time = ev_sa,
    estimate = b_sa[sa_terms],
    se = sqrt(diag(V_sa))[sa_terms]
  ) %>%
    filter(!is.na(event_time)) %>%
    arrange(event_time)
  
  # Post 1-6 average
  post_terms_sa <- sa_df$term[sa_df$event_time >= 1 & sa_df$event_time <= 6]
  if (length(post_terms_sa) > 0) {
    w_sa <- rep(1 / length(post_terms_sa), length(post_terms_sa))
    att_sa_1_6 <- sum(w_sa * b_sa[post_terms_sa])
    Vsub_sa <- V_sa[post_terms_sa, post_terms_sa, drop = FALSE]
    se_sa_1_6 <- sqrt(as.numeric(t(w_sa) %*% Vsub_sa %*% w_sa))
    
    cat(sprintf("Sun-Abraham Post 1-6 avg: %.4f (SE = %.4f)\n",
                att_sa_1_6, se_sa_1_6))
    # Post 1-3 (main window)
    post_terms_sa_13 <- sa_df$term[sa_df$event_time >= 1 & sa_df$event_time <= 3]
    if (length(post_terms_sa_13) > 0) {
      w_sa_13 <- rep(1 / length(post_terms_sa_13), length(post_terms_sa_13))
      att_sa_1_3 <- sum(w_sa_13 * b_sa[post_terms_sa_13])
      Vsub_sa_13 <- V_sa[post_terms_sa_13, post_terms_sa_13, drop = FALSE]
      se_sa_1_3 <- sqrt(as.numeric(t(w_sa_13) %*% Vsub_sa_13 %*% w_sa_13))
      cat(sprintf("Sun-Abraham Post 1-3 avg: %.4f (SE = %.4f)\n",
                  att_sa_1_3, se_sa_1_3))
      results_list[["Alt 1: Sun-Abraham (post 1-3mo)"]] <- list(
        att = att_sa_1_3,
        se  = se_sa_1_3
      )
    }
    
    # Post 1-6 (secondary)
    results_list[["Alt 1: Sun-Abraham (post 1-6mo, secondary)"]] <- list(
      att = att_sa_1_6,
      se  = se_sa_1_6
    )
  } else {
    cat("[!] Could not extract post 1-6 coefficients from Sun-Abraham model.\n")
  }
  
  # Event-study plot
  sa_es <- sa_df %>%
    filter(event_time >= -6, event_time <= 6) %>%
    mutate(
      ci_lower = estimate - 1.96 * se,
      ci_upper = estimate + 1.96 * se
    )
  make_es_plot(
    sa_es,
    title = "Event-Study: Sun-Abraham (TWFE)",
    subtitle = "Same sample and covariates, anticipation=2 (rel -2/-1 dropped)",
    file_name = "fig_es_sunab.png"
  )
} else {
  cat("[!] No Sun-Abraham coefficients found.\n")
}

############################################################
# 3. ALTERNATIVE 2: Imputation DID (BJS)
############################################################

cat("\n╔", strrep("═", 60), "╗\n", sep = "")
cat("║", sprintf("%-60s", " ALTERNATIVE 2: Imputation DID (BJS)"), "║\n", sep = "")
cat("╚", strrep("═", 60), "╝\n\n", sep = "")

# Create weight column for post 1-6 average (wtr parameter)
# This allows did_imputation to directly compute post 1-6 average with correct SE
# wtr should assign weight 1/6 to treated units at event times 1-6, zero otherwise
df_main <- df_main %>%
  mutate(
    # Calculate event time relative to G_int (enforcement month)
    event_time_imp = ifelse(.data[[G_USE]] > 0, t - .data[[G_USE]], NA_integer_),
    # Assign equal weight (1/6) to event times 1-6 for treated units
    w_post1_6 = ifelse(!is.na(event_time_imp) & event_time_imp >= 1 & event_time_imp <= 6, 
                       1/6, 0)
  )

run_imp <- function(df, gname = G_USE, horizon = NULL, pretrends = -6:-3, 
                   wname = NULL, wtr = NULL) {
  # If wtr is specified, use it to get weighted average directly (no horizon needed)
  # Otherwise, use horizon to get event-study coefficients
  did_imputation(
    data = df,
    yname = "Y",
    gname = gname,
    tname = "t",
    idname = "id_num",
    first_stage = ~ unemployment_rate + log_lf | id_num + t,
    horizon = horizon,
    pretrends = pretrends,
    wname = wname,
    wtr = wtr,  # Weight column for targeted average (post 1-6)
    cluster_var = "id_num"
  )
}

# Try wtr first, but fallback to horizon if it doesn't work
# Use wtr to get post 1-6 average directly with correct SE
imp <- tryCatch({
  run_imp(df_main, gname = G_USE, horizon = NULL, pretrends = -6:-3, wtr = "w_post1_6")
}, error = function(e) {
  cat("    [!] wtr parameter failed, falling back to horizon method.\n")
  cat("    Error:", conditionMessage(e), "\n")
  NULL
})

# Extract post 1-6 average from imputation results
if (is.null(imp)) {
  # Fallback: use horizon method
  cat("    Using horizon method (manual average of event-study coefficients)...\n")
  imp <- run_imp(df_main, gname = G_USE, horizon = -6:6, pretrends = -6:-3, wtr = NULL)
}

if (is.data.frame(imp)) {
  # Debug: print structure of imputation output
  cat(sprintf("    Imputation output: %d rows, columns: %s\n", 
              nrow(imp), paste(names(imp), collapse = ", ")))
  if (nrow(imp) <= 5) {
    cat("    First few rows:\n")
    print(head(imp))
  }
  
  # Initialize flag to track if we've successfully extracted
  imp_1_6 <- NULL
  es_df_imp <- NULL
  
  # Check if wtr was used (should return a single row with "treat" or similar term)
  # Some versions of didimputation may return different term names
  if (nrow(imp) == 1) {
    # Single row: check if it's a wtr result or just a single event time
    term_name <- as.character(imp$term[1])
    cat(sprintf("    Single row detected, term: '%s'\n", term_name))
    
    # Check if term looks like a weighted average result (not a numeric event time)
    is_wtr_result <- any(grepl("treat|wtr|weighted|att|effect", term_name, ignore.case = TRUE)) || 
                     !grepl("^-?[0-9]+$", term_name)
    
    if (is_wtr_result) {
      # wtr was used: direct weighted average
      imp_1_6 <- c(att = imp$estimate[1], se = imp$std.error[1])
      cat(sprintf("    Imputation Post 1-6 avg (via wtr): %.4f (SE = %.4f)\n", 
                  imp_1_6["att"], imp_1_6["se"]))
      
      # For event-study plot, run again with horizon
      imp_es_obj <- run_imp(df_main, gname = G_USE, horizon = -6:6, pretrends = -6:-3, wtr = NULL)
      if (is.data.frame(imp_es_obj)) {
        imp_es <- imp_es_obj %>%
          filter(lhs == "Y") %>%
          mutate(event_time = suppressWarnings(as.integer(term))) %>%
          filter(!is.na(event_time))
        
        es_df_imp <- imp_es %>%
          filter(event_time >= -6, event_time <= 6) %>%
          mutate(
            estimate = estimate,
            se = std.error,
            ci_lower = estimate - 1.96 * se,
            ci_upper = estimate + 1.96 * se
          ) %>%
          select(event_time, estimate, se, ci_lower, ci_upper) %>%
          arrange(event_time)
      }
    }
  }
  
  # Extract from event-study coefficients (horizon method or fallback)
  # Only do this if wtr didn't work or we need event-study plot
  if (is.null(imp_1_6)) {
    # If single row but wasn't wtr result, or multiple rows, extract from event-study
    if (nrow(imp) > 1 || (nrow(imp) == 1 && grepl("^-?[0-9]+$", as.character(imp$term[1])))) {
      imp_es <- imp %>%
        filter(lhs == "Y") %>%
        mutate(event_time = suppressWarnings(as.integer(term))) %>%
        filter(!is.na(event_time))
      
      cat(sprintf("    Extracted event times: %s\n", 
                  paste(sort(unique(imp_es$event_time)), collapse = ", ")))
      
      imp_post <- imp_es %>%
        filter(event_time >= 1, event_time <= 6) %>%
        filter(!is.na(estimate), !is.na(std.error))
      
      if (nrow(imp_post) > 0) {
        # Calculate average
        imp_att_1_6 <- mean(imp_post$estimate, na.rm = TRUE)
        
        # SE: conservative approximation (assumes independence)
        k <- nrow(imp_post)
        imp_se_1_6 <- sqrt(sum(imp_post$std.error^2, na.rm = TRUE)) / k
        
        imp_1_6 <- c(att = imp_att_1_6, se = imp_se_1_6)
        cat(sprintf("    Imputation Post 1-6 avg: %.4f (SE = %.4f, conservative - from %d horizons)\n", 
                    imp_1_6["att"], imp_1_6["se"], k))
        
        es_df_imp <- imp_es %>%
          filter(event_time >= -6, event_time <= 6) %>%
          mutate(
            estimate = estimate,
            se = std.error,
            ci_lower = estimate - 1.96 * se,
            ci_upper = estimate + 1.96 * se
          ) %>%
          select(event_time, estimate, se, ci_lower, ci_upper) %>%
          arrange(event_time)
      } else {
        cat("    [!] Could not extract post 1-6 coefficients.\n")
        if (nrow(imp_es) > 0) {
          cat(sprintf("    Available event times: %s\n", 
                      paste(sort(unique(imp_es$event_time)), collapse = ", ")))
        } else {
          cat("    No event times found in imputation output.\n")
        }
        imp_1_6 <- c(att = NA_real_, se = NA_real_)
        if (is.null(es_df_imp)) {
          es_df_imp <- data.frame(event_time = integer(), estimate = numeric(), 
                                 se = numeric(), ci_lower = numeric(), ci_upper = numeric())
        }
      }
    }
  }
  
  # Ensure imp_1_6 and es_df_imp are set
  if (is.null(imp_1_6)) {
    imp_1_6 <- c(att = NA_real_, se = NA_real_)
  }
  if (is.null(es_df_imp)) {
    es_df_imp <- data.frame(event_time = integer(), estimate = numeric(), 
                           se = numeric(), ci_lower = numeric(), ci_upper = numeric())
  }
} else if (inherits(imp, "att_gt") || inherits(imp, "MP")) {
  # If did_imputation returns an att_gt-like object, use standard method
  imp_1_6 <- post_window_avg(imp, e_min = 1, e_max = 6)
  cat(sprintf("    Imputation Post 1-6 avg: %.4f (SE = %.4f)\n", 
              imp_1_6["att"], imp_1_6["se"]))
  
  es_imp <- aggte(imp, type = "dynamic", cband = TRUE, 
                  balance_e = 6, min_e = -6, max_e = 6)
  es_df_imp <- event_study_df(es_imp)
} else {
  cat("    [!] Unexpected imputation output format.\n")
  imp_1_6 <- c(att = NA_real_, se = NA_real_)
  es_df_imp <- data.frame(event_time = integer(), estimate = numeric(), 
                         se = numeric(), ci_lower = numeric(), ci_upper = numeric())
}
make_es_plot(
  es_df_imp,
  title = "Event-Study: Imputation DID (BJS)",
  subtitle = "Borusyak, Jaravel, and Spiess (2021)",
  file_name = "fig_es_imputation.png"
)

# Post 1-3 (main window) from imputation
if (exists("imp_es") && is.data.frame(imp_es) && nrow(imp_es) > 0) {
  imp_post_13 <- imp_es %>%
    filter(event_time >= 1, event_time <= 3) %>%
    filter(!is.na(estimate), !is.na(std.error))
  
  if (nrow(imp_post_13) > 0) {
    imp_att_1_3 <- mean(imp_post_13$estimate, na.rm = TRUE)
    k_13 <- nrow(imp_post_13)
    imp_se_1_3 <- sqrt(sum(imp_post_13$std.error^2, na.rm = TRUE)) / k_13
    cat(sprintf("    Imputation Post 1-3 avg: %.4f (SE = %.4f)\n",
                imp_att_1_3, imp_se_1_3))
    results_list[["Alt 2: Imputation (post 1-3mo)"]] <- list(
      att = imp_att_1_3,
      se  = imp_se_1_3
    )
  }
}

results_list[["Alt 2: Imputation (post 1-6mo, secondary)"]] <- list(
  att = imp_1_6["att"],
  se  = imp_1_6["se"]
)

############################################################
# 4. ALTERNATIVE 3: Stacked Event Study
############################################################

cat("\n╔", strrep("═", 60), "╗\n", sep = "")
cat("║", sprintf("%-60s", " ALTERNATIVE 3: Stacked Event Study"), "║\n", sep = "")
cat("╚", strrep("═", 60), "╝\n\n", sep = "")

build_stacked <- function(df, gname = G_USE, L = 6, K = 6, drop_anticipation = ANT_USE) {
  # Use same G as main specification
  treated_g <- sort(unique(df[[gname]][df[[gname]] > 0]))
  
  out <- lapply(treated_g, function(g) {
    win <- df %>% filter(t >= g - L, t <= g + K)
    
    # Controls: never-treated or treated after window ends
    # Note: This uses "clean controls" - more restrictive than CS not-yet-treated
    win <- win %>%
      filter(.data[[gname]] == g | .data[[gname]] == 0L | .data[[gname]] > (g + K)) %>%
      mutate(
        stack_g = g,
        rel = t - g,
        treat = as.integer(.data[[gname]] == g),
        unit_stack = interaction(id_num, stack_g, drop = TRUE),
        time_stack = interaction(t, stack_g, drop = TRUE)
      )
    # Drop anticipation periods (e.g., rel = -2, -1 when anticipation = 2)
    if (!is.null(drop_anticipation) && drop_anticipation > 0) {
      win <- win %>% filter(!(rel %in% (-drop_anticipation):-1))
    }
    win
  })
  
  bind_rows(out)
}

run_stacked <- function(stk_df, ref_period = -3) {
  # ref = -3 is correct when anticipation = 2 (last pure pre-period)
  feols(
    Y ~ i(rel, treat, ref = ref_period) + unemployment_rate + log_lf | 
      unit_stack + time_stack,
    data = stk_df,
    cluster = ~ id_num
  )
}

stk <- build_stacked(df_main, gname = G_USE, L = 6, K = 6)
mod_stk <- run_stacked(stk)

# Extract post 1-6 average from stacked model
b_stk <- coef(mod_stk)
V_stk <- vcov(mod_stk)

# Find coefficients for rel = 1, 2, ..., 6 using regex (robust across fixest versions)
# Pattern: "rel::1:treat", "rel::2:treat", etc. (or variations)
nm_stk <- names(b_stk)
post_terms_stk <- nm_stk[grepl("^rel::[1-6]:treat$", nm_stk)]

# If that doesn't work, try alternative patterns
if (length(post_terms_stk) == 0) {
  post_terms_stk <- nm_stk[grepl("rel.*::[1-6].*treat|treat.*::[1-6]", nm_stk)]
}

# Debug: print available coefficient names if not found
if (length(post_terms_stk) == 0) {
  cat("    [!] Could not find post 1-6 coefficients with standard pattern.\n")
  cat("    Available coefficient names (first 30):\n")
  cat("    ", paste(head(nm_stk, 30), collapse = ", "), "\n")
  # Try to extract all rel coefficients and filter
  rel_coefs <- nm_stk[grepl("rel|treat", nm_stk, ignore.case = TRUE)]
  cat("    Coefficients containing 'rel' or 'treat':\n")
  cat("    ", paste(head(rel_coefs, 20), collapse = ", "), "\n")
}

if (length(post_terms_stk) > 0) {
  w_stk <- rep(1 / length(post_terms_stk), length(post_terms_stk))
  att_stk_1_6 <- sum(w_stk * b_stk[post_terms_stk])
  
  Vsub_stk <- V_stk[post_terms_stk, post_terms_stk, drop = FALSE]
  se_stk_1_6 <- sqrt(as.numeric(t(w_stk) %*% Vsub_stk %*% w_stk))
  
  cat(sprintf("Stacked Post 1-6 avg: %.4f (SE = %.4f)\n", 
              att_stk_1_6, se_stk_1_6))
  
  # Post 1-3 (main window)
  post_terms_stk_13 <- nm_stk[grepl("^rel::[1-3]:treat$", nm_stk)]
  if (length(post_terms_stk_13) > 0) {
    w_stk_13 <- rep(1 / length(post_terms_stk_13), length(post_terms_stk_13))
    att_stk_1_3 <- sum(w_stk_13 * b_stk[post_terms_stk_13])
    Vsub_stk_13 <- V_stk[post_terms_stk_13, post_terms_stk_13, drop = FALSE]
    se_stk_1_3 <- sqrt(as.numeric(t(w_stk_13) %*% Vsub_stk_13 %*% w_stk_13))
    cat(sprintf("Stacked Post 1-3 avg: %.4f (SE = %.4f)\n",
                att_stk_1_3, se_stk_1_3))
    results_list[["Alt 3: Stacked (post 1-3mo)"]] <- list(
      att = att_stk_1_3,
      se  = se_stk_1_3
    )
  }
  
  results_list[["Alt 3: Stacked (post 1-6mo, secondary)"]] <- list(
    att = att_stk_1_6,
    se  = se_stk_1_6
  )
} else {
  cat("[!] Could not extract post 1-6 coefficients from stacked model.\n")
  cat("Available coefficient names (first 20):\n")
  cat(paste(head(names(b_stk), 20), collapse = ", "), "\n")
}

# Event-study plot for stacked (extract all rel coefficients)
# Use regex to extract event times from coefficient names (robust across fixest versions)
rel_coefs <- names(b_stk)[grepl("rel|treat", names(b_stk), ignore.case = TRUE) & 
                          grepl("::", names(b_stk))]

# Extract numeric values from coefficient names (try multiple patterns)
rel_vals <- suppressWarnings({
  # Pattern 1: "rel::k:treat" or "rel::k::treat"
  vals1 <- as.integer(sub(".*rel::(-?\\d+).*treat.*", "\\1", rel_coefs))
  # Pattern 2: "treat::k" or similar
  vals2 <- as.integer(sub(".*treat.*::(-?\\d+).*", "\\1", rel_coefs))
  # Pattern 3: any number between ::
  vals3 <- as.integer(sub(".*::(-?\\d+).*", "\\1", rel_coefs))
  # Use first non-NA value
  ifelse(!is.na(vals1), vals1, ifelse(!is.na(vals2), vals2, vals3))
})

if (length(rel_vals) > 0) {
  stk_es <- data.frame(
    event_time = rel_vals,
    estimate = b_stk[rel_coefs],
    se = se(mod_stk)[rel_coefs]
  ) %>%
    filter(event_time >= -6, event_time <= 6) %>%
    arrange(event_time) %>%
    mutate(
      ci_lower = estimate - 1.96 * se,
      ci_upper = estimate + 1.96 * se
    )
  
  make_es_plot(
    stk_es,
    title = "Event-Study: Stacked DiD",
    subtitle = "Stacked event-study with clean controls",
    file_name = "fig_es_stacked.png"
  )
}

############################################################
# 5. ROBUSTNESS CHECKS (8 Core Checks)
############################################################

cat("\n╔", strrep("═", 60), "╗\n", sep = "")
cat("║", sprintf("%-60s", " ROBUSTNESS CHECKS"), "║\n", sep = "")
cat("╚", strrep("═", 60), "╝\n\n", sep = "")

# [1] Anticipation sensitivity (using post 1-3 as main window)
cat("[1] Robustness: Anticipation Sensitivity...\n")
for (ant in c(0L, 1L, 2L, 3L)) {
  if (ant == ANT_USE) next
  att_ant <- run_cs(df_main, gname = G_USE, anticipation = ant)
  ant_1_3 <- post_window_avg(att_ant, e_min = 1, e_max = 3)
  cat(sprintf("    Anticipation = %d: %.4f (SE=%.4f)\n",
              ant, ant_1_3["att"], ant_1_3["se"]))
  results_list[[sprintf("Robust: Anticipation=%d", ant)]] <- list(
    att = ant_1_3["att"],
    se  = ant_1_3["se"]
  )
}

# [2] Trim to last untreated period (before final cohort treated)
cat("\n[2] Robustness: Trim to Last Untreated Period...\n")
t_last <- max(df_main[[G_USE]][df_main[[G_USE]] > 0], na.rm = TRUE)
t_trim <- t_last - 1L
df_trim <- df_main %>% filter(t <= t_trim)
if (nrow(df_trim) > 0) {
  trim_date <- as.Date(sprintf("%04d-%02d-01", t_trim %/% 12L, t_trim %% 12L))
  cat(sprintf("    Trimming to t <= %d (%s)\n", t_trim, trim_date))
  att_trim <- run_cs(df_trim, gname = G_USE, anticipation = ANT_USE)
  trim_1_3 <- post_window_avg(att_trim, e_min = 1, e_max = 3)
  cat(sprintf("    CS (trimmed) Post 1-3: %.4f (SE=%.4f)\n",
              trim_1_3["att"], trim_1_3["se"]))
  results_list[["Robust: Trim to pre-final cohort"]] <- list(
    att = trim_1_3["att"],
    se  = trim_1_3["se"]
  )
} else {
  cat("    [!] Trimmed sample is empty.\n")
}

# [3] Population-weighted: Use pop_1849 as sampling weights
cat("\n[3] Robustness: Population-Weighted...\n")
if ("pop_1849" %in% names(df_main) && any(!is.na(df_main$pop_1849) & df_main$pop_1849 > 0)) {
  att_cs_weighted <- run_cs(df_main, gname = G_USE, anticipation = ANT_USE, 
                            weightsname = "pop_1849")
  cs_weighted_1_3 <- post_window_avg(att_cs_weighted, e_min = 1, e_max = 3)
  cat(sprintf("    CS (pop-weighted) Post 1-3: %.4f (SE=%.4f)\n",
              cs_weighted_1_3["att"], cs_weighted_1_3["se"]))
  results_list[["Robust: Pop-weighted"]] <- list(
    att = cs_weighted_1_3["att"],
    se  = cs_weighted_1_3["se"]
  )
} else {
  cat("    [!] Population weights (pop_1849) not available.\n")
}

# [4] Outcome scale: Level (rate) instead of log
cat("\n[4] Robustness: Outcome Scale (Level vs Log)...\n")
if ("y_per1k_18_49" %in% names(panel_raw)) {
  df_level <- build_df(panel_raw, y_col = "y_per1k_18_49") %>%
    mutate(Y = y_per1k_18_49)  # Use level, not log
  
  stopifnot(G_USE %in% names(df_level))
  att_level <- run_cs(df_level, gname = G_USE, anticipation = ANT_USE)
  level_1_3 <- post_window_avg(att_level, e_min = 1, e_max = 3)
  cat(sprintf("    Level outcome Post 1-3: %.4f (SE=%.4f)\n",
              level_1_3["att"], level_1_3["se"]))
  results_list[["Robust: Level outcome"]] <- list(
    att = level_1_3["att"],
    se  = level_1_3["se"]
  )
}

# [5] Window sensitivity: Different post windows
cat("\n[5] Robustness: Post Window Sensitivity...\n")
# Post 1-2 months (very short-term)
cs_1_2 <- post_window_avg(att_cs, e_min = 1, e_max = 2)
cat(sprintf("    Post 1-2 months: %.4f (SE=%.4f)\n",
            cs_1_2["att"], cs_1_2["se"]))
results_list[["Robust: Post 1-2mo"]] <- list(
  att = cs_1_2["att"],
  se  = cs_1_2["se"]
)

# Post 1-4 months
cs_1_4 <- post_window_avg(att_cs, e_min = 1, e_max = 4)
cat(sprintf("    Post 1-4 months: %.4f (SE=%.4f)\n",
            cs_1_4["att"], cs_1_4["se"]))
results_list[["Robust: Post 1-4mo"]] <- list(
  att = cs_1_4["att"],
  se  = cs_1_4["se"]
)

# Post 1-6 months (note: early cohorts only due to identification limits)
cat(sprintf("    Post 1-6 months (early cohorts only): %.4f (SE=%.4f)\n",
            cs_1_6["att"], cs_1_6["se"]))

# [6] Drop small cohort (if applicable)
cat("\n[6] Robustness: Drop Small Cohort...\n")
if (length(small_cohort) > 0) {
  for (g_drop in small_cohort) {
    df_drop_small <- df_main %>% filter(.data[[G_USE]] != g_drop)
    
    if (nrow(df_drop_small) < nrow(df_main) && any(df_drop_small[[G_USE]] > 0)) {
      cat(sprintf("    Dropping cohort G_int = %d...\n", g_drop))
      att_drop_small <- run_cs(df_drop_small, gname = G_USE, anticipation = ANT_USE)
      drop_small_1_3 <- post_window_avg(att_drop_small, e_min = 1, e_max = 3)
      cat(sprintf("    CS (drop G_int=%d) Post 1-3: %.4f (SE=%.4f)\n",
                  g_drop, drop_small_1_3["att"], drop_small_1_3["se"]))
      results_list[[sprintf("Robust: Drop cohort %d", g_drop)]] <- list(
        att = drop_small_1_3["att"],
        se  = drop_small_1_3["se"]
      )
    }
  }
} else {
  cat("    No small cohorts to drop.\n")
}

# [7] Leave-one-out (for small cohort only)
cat("\n[7] Robustness: Leave-One-Out (Small Cohort)...\n")
if (length(small_cohort) > 0) {
  for (g_loo in small_cohort) {
    small_ids <- df_main %>%
      filter(.data[[G_USE]] == g_loo) %>%
      distinct(id_num) %>%
      pull(id_num)
    
    if (length(small_ids) > 0) {
      cat(sprintf("    Leave-one-out for cohort G_int = %d (%d counties)...\n", 
                  g_loo, length(small_ids)))
      
      loo_results <- lapply(small_ids, function(drop_id) {
        d <- df_main %>% filter(id_num != drop_id)
        if (any(d[[G_USE]] > 0)) {
          a <- run_cs(d, gname = G_USE, anticipation = ANT_USE)
          p <- post_window_avg(a, e_min = 1, e_max = 3)
          tibble(drop_id = drop_id, att = p["att"], se = p["se"])
        } else {
          NULL
        }
      })
      
      loo_df <- bind_rows(loo_results)
      if (nrow(loo_df) > 0) {
        loo_df <- loo_df %>% arrange(att)
        cat("    Leave-one-out results:\n")
        print(loo_df)
        
        # Store min/max for summary
        results_list[[sprintf("Robust: LOO min (cohort %d)", g_loo)]] <- list(
          att = min(loo_df$att, na.rm = TRUE),
          se  = loo_df$se[which.min(loo_df$att)]
        )
        results_list[[sprintf("Robust: LOO max (cohort %d)", g_loo)]] <- list(
          att = max(loo_df$att, na.rm = TRUE),
          se  = loo_df$se[which.max(loo_df$att)]
        )
      }
    }
  }
} else {
  cat("    No small cohorts for leave-one-out.\n")
}

# [8] Exclude Wayne County (if applicable)
cat("\n[8] Robustness: Exclude Wayne County...\n")
drop_ids <- c("Wayne", "Wayne County", "26163", "260163")
df_drop <- df_main

if ("id" %in% names(df_drop)) {
  df_drop <- df_drop %>% filter(!as.character(id) %in% drop_ids)
}
if ("county" %in% names(df_drop)) {
  df_drop <- df_drop %>% filter(!as.character(county) %in% c("Wayne", "Wayne County"))
}
if ("county_id" %in% names(df_drop)) {
  df_drop <- df_drop %>% filter(!as.character(county_id) %in% c("26163", "260163"))
}

if (nrow(df_drop) < nrow(df_main)) {
  stopifnot(G_USE %in% names(df_drop))
  att_drop <- run_cs(df_drop, gname = G_USE, anticipation = ANT_USE)
  drop_1_3 <- post_window_avg(att_drop, e_min = 1, e_max = 3)
  cat(sprintf("    Excl. Wayne Post 1-3: %.4f (SE=%.4f)\n",
              drop_1_3["att"], drop_1_3["se"]))
  cat("    NOTE: Dropping Wayne changes identification (2018-07 cohort loses controls).\n")
  results_list[["Robust: Excl. Wayne County"]] <- list(
    att = drop_1_3["att"],
    se  = drop_1_3["se"]
  )
} else {
  cat("    [!] Wayne County not found or already excluded.\n")
}

############################################################
# 6. RESULTS TABLE
############################################################

cat("\n╔", strrep("═", 60), "╗\n", sep = "")
cat("║", sprintf("%-60s", " MAIN RESULTS TABLE"), "║\n", sep = "")
cat("╚", strrep("═", 60), "╝\n\n", sep = "")

results_df <- bind_rows(lapply(names(results_list), function(spec) {
  res <- results_list[[spec]]
  if (is.null(res) || !is.finite(res$att) || !is.finite(res$se)) {
    return(NULL)
  }
  data.frame(
    Specification = spec,
    ATT = res$att,
    SE  = res$se,
    stringsAsFactors = FALSE
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

# Print main table (Main + Alternatives)
cat("=== MAIN RESULTS (Post 1-6 Month Average) ===\n")
main_table <- results_df %>%
  filter(grepl("^Main:|^Alt", Specification)) %>%
  select(Specification, ATT, SE, CI_lower, CI_upper, Sig)
print(main_table)

# Diagnostic: Compare the three main methods
cat("\n=== DIAGNOSTIC: Method Comparison ===\n")
main_methods <- results_df %>%
  filter(grepl("^Main:|^Alt", Specification)) %>%
  select(Specification, ATT, SE)

if (nrow(main_methods) >= 2) {
  cat("Post 1-6 month average across methods:\n")
  for (i in 1:nrow(main_methods)) {
    cat(sprintf("  %-30s: %8.4f (SE=%6.4f)\n",
                main_methods$Specification[i],
                main_methods$ATT[i],
                main_methods$SE[i]))
  }
  
  # Check for sign consistency
  atts <- main_methods$ATT[is.finite(main_methods$ATT)]
  if (length(atts) >= 2) {
    all_positive <- all(atts > 0, na.rm = TRUE)
    all_negative <- all(atts < 0, na.rm = TRUE)
    if (all_positive || all_negative) {
      cat("  ✓ Sign consistent across methods\n")
    } else {
      cat("  ⚠ WARNING: Sign inconsistency across methods!\n")
      cat("  This may indicate:\n")
      cat("    - Different estimands (check sample windows, control definitions)\n")
      cat("    - Treatment timing (G) may need adjustment\n")
      cat("    - Check event-study plots for dynamic patterns\n")
    }
  }
}

# Save full table
write_csv(results_df, file.path(outdir, "did_main_results.csv"))
cat("\n[✓] Full results table saved to did_main_results.csv\n")

# Save main table only
write_csv(main_table, file.path(outdir, "did_main_table.csv"))
cat("[✓] Main results table saved to did_main_table.csv\n")

cat("\n╔", strrep("═", 60), "╗\n", sep = "")
cat("║", sprintf("%-60s", " ANALYSIS COMPLETE"), "║\n", sep = "")
cat("╚", strrep("═", 60), "╝\n\n", sep = "")
