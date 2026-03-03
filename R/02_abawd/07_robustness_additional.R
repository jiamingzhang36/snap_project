# R/02_abawd/07_robustness_additional.R
# ============================================================================
# Additional robustness checks identified in audit:
#   (1) Goodman-Bacon (2021) TWFE decomposition
#   (2) Leave-one-cohort-out analysis
#   (3) Joint pre-trends test with proper VCOV
#   (4) Covariate balance table across treatment cohorts
#   (5) Age-group transferability sensitivity (for projection)
#
# Input:  data/derived/panel_analysis.rds
# Output: outputs/tables/bacon_decomposition.csv
#         outputs/tables/leave_one_cohort_out.csv
#         outputs/tables/pretrends_joint_test.csv
#         outputs/tables/covariate_balance_cohorts.csv
#         outputs/tables/age_transferability_sensitivity.csv
#         outputs/figures/bacon_decomposition.png
# ============================================================================

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)
source("R/00_utils/helpers_did.R", local = TRUE)

set.seed(123)

path_panel <- file.path(DIR_DERIVED, "panel_analysis.rds")
stopifnot(file.exists(path_panel))

panel_raw <- as.data.frame(readRDS(path_panel))
if (!"date" %in% names(panel_raw) && "ym_date" %in% names(panel_raw))
  panel_raw$date <- panel_raw$ym_date
panel_raw$date <- as.Date(panel_raw$date)

if (!dir.exists(DIR_OUT_TABLES))  dir.create(DIR_OUT_TABLES,  recursive = TRUE)
if (!dir.exists(DIR_OUT_FIGURES)) dir.create(DIR_OUT_FIGURES, recursive = TRUE)

Y_MAIN <- "y_per1k_18_49"
df_main <- build_analysis_df(panel_raw, Y_MAIN)

# Baseline: re-run main spec for reference
res_main <- run_cs_did(df_main, anticipation = 2)
main_att <- res_main$overall$overall.att
main_se  <- res_main$overall$overall.se
message(sprintf("Baseline ATT = %.4f (SE = %.4f)", main_att, main_se))


# ============================================================================
# 1) Goodman-Bacon (2021) TWFE Decomposition
# ============================================================================
message("\n=== [1] Goodman-Bacon TWFE Decomposition ===")

tryCatch({
  # bacondecomp needs plain data.frame with simple column names
  df_bacon <- data.frame(
    id = df_main$id_num,
    time = df_main$t,
    Y = df_main$Y,
    treat = as.integer(df_main$G_int > 0 & df_main$t >= df_main$G_int)
  )

  bacon_out <- bacondecomp::bacon(
    Y ~ treat,
    data = df_bacon,
    id_var = "id",
    time_var = "time"
  )

  # Summarize by type
  bacon_summary <- bacon_out %>%
    dplyr::group_by(type) %>%
    dplyr::summarise(
      n_comparisons = dplyr::n(),
      avg_estimate  = weighted.mean(estimate, weight),
      total_weight  = sum(weight),
      min_estimate  = min(estimate),
      max_estimate  = max(estimate),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(total_weight))

  readr::write_csv(bacon_summary, file.path(DIR_OUT_TABLES, "bacon_decomposition.csv"))
  message("  Decomposition by type:")
  for (i in seq_len(nrow(bacon_summary))) {
    message(sprintf("    %-30s weight=%.3f  est=%.4f  [%.4f, %.4f]  n=%d",
                    bacon_summary$type[i],
                    bacon_summary$total_weight[i],
                    bacon_summary$avg_estimate[i],
                    bacon_summary$min_estimate[i],
                    bacon_summary$max_estimate[i],
                    bacon_summary$n_comparisons[i]))
  }

  # Check for negative weights
  neg_weight <- any(bacon_out$weight < 0)
  message(sprintf("  Negative weights present: %s", neg_weight))

  # TWFE overall (weighted sum)
  twfe_overall <- sum(bacon_out$estimate * bacon_out$weight)
  message(sprintf("  TWFE overall (sum of decomposition): %.4f", twfe_overall))

  # Plot
  p_bacon <- ggplot2::ggplot(bacon_out,
                              ggplot2::aes(x = weight, y = estimate, color = type)) +
    ggplot2::geom_point(size = 3, alpha = 0.7) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_hline(yintercept = main_att, linetype = "dotted", color = "firebrick",
                         linewidth = 0.8) +
    ggplot2::annotate("text", x = max(bacon_out$weight) * 0.8, y = main_att + 0.005,
                       label = paste0("CS-DID ATT = ", round(main_att, 3)),
                       color = "firebrick", size = 3.5) +
    ggplot2::labs(
      title = "Goodman-Bacon (2021) Decomposition of TWFE Estimate",
      subtitle = "Each point is a 2x2 DiD comparison; size proportional to TWFE weight",
      x = "Weight in TWFE", y = "2x2 DiD Estimate", color = "Comparison type"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "bottom"
    )

  ggplot2::ggsave(file.path(DIR_OUT_FIGURES, "bacon_decomposition.png"),
                  p_bacon, width = 9, height = 6, dpi = 300)
  message("  Wrote outputs/figures/bacon_decomposition.png")

}, error = function(e) {
  message("  Goodman-Bacon decomposition failed: ", conditionMessage(e))
})


# ============================================================================
# 2) Leave-One-Cohort-Out Analysis
# ============================================================================
message("\n=== [2] Leave-One-Cohort-Out Analysis ===")

# Identify cohorts (G_int values > 0)
cohort_values <- sort(unique(df_main$G_int[df_main$G_int > 0]))
cohort_labels <- sapply(cohort_values, function(g) {
  y <- g %/% 12
  m <- g %% 12
  sprintf("%d-%02d", y, m)
})
message("  Cohorts: ", paste(cohort_labels, collapse = ", "))
message("  Cohort sizes: ", paste(
  sapply(cohort_values, function(g) sum(df_main$G_int == g) / length(unique(df_main$t))),
  collapse = ", "))

loo_results <- list()

for (i in seq_along(cohort_values)) {
  g_drop <- cohort_values[i]
  label <- cohort_labels[i]

  # Drop the cohort entirely (remove those counties)
  df_loo <- df_main %>% dplyr::filter(G_int != g_drop)
  n_counties_remaining <- length(unique(df_loo$id_num[df_loo$G_int > 0]))

  tryCatch({
    res_loo <- run_cs_did(df_loo, anticipation = 2)
    loo_att <- res_loo$overall$overall.att
    loo_se  <- res_loo$overall$overall.se

    loo_results[[label]] <- data.frame(
      dropped_cohort = label,
      dropped_G = g_drop,
      n_treated_counties_remaining = n_counties_remaining,
      att = loo_att,
      se = loo_se,
      ci_lower = loo_att - 1.96 * loo_se,
      ci_upper = loo_att + 1.96 * loo_se,
      stringsAsFactors = FALSE
    )
    message(sprintf("  Drop %s: ATT = %.4f (SE = %.4f), %d treated counties remain",
                    label, loo_att, loo_se, n_counties_remaining))

  }, error = function(e) {
    loo_results[[label]] <<- data.frame(
      dropped_cohort = label,
      dropped_G = g_drop,
      n_treated_counties_remaining = n_counties_remaining,
      att = NA_real_, se = NA_real_,
      ci_lower = NA_real_, ci_upper = NA_real_,
      stringsAsFactors = FALSE
    )
    message(sprintf("  Drop %s: FAILED — %s", label, conditionMessage(e)))
  })
}

# Add baseline (no cohort dropped)
loo_all <- dplyr::bind_rows(
  data.frame(dropped_cohort = "None (baseline)", dropped_G = NA_integer_,
             n_treated_counties_remaining = length(unique(df_main$id_num[df_main$G_int > 0])),
             att = main_att, se = main_se,
             ci_lower = main_att - 1.96 * main_se,
             ci_upper = main_att + 1.96 * main_se),
  dplyr::bind_rows(loo_results)
)

readr::write_csv(loo_all, file.path(DIR_OUT_TABLES, "leave_one_cohort_out.csv"))
message("  Wrote outputs/tables/leave_one_cohort_out.csv")


# ============================================================================
# 3) Joint Pre-Trends Test with Proper VCOV
# ============================================================================
message("\n=== [3] Joint Pre-Trends Test (Proper VCOV) ===")

tryCatch({
  # Get dynamic event study with influence function
  es_dynamic <- did::aggte(res_main$att, type = "dynamic",
                            cband = TRUE, balance_e = 6, min_e = -6, max_e = 6)

  # Extract influence function for proper VCOV
  inf_func <- es_dynamic$inf.function$dynamic.inf.func.e
  n_obs <- nrow(inf_func)
  V_full <- t(inf_func) %*% inf_func / n_obs / n_obs

  # Event times and pre-period indices
  egt <- es_dynamic$egt
  att <- es_dynamic$att.egt
  se  <- es_dynamic$se.egt

  # Pre-period: e < -2 (with anticipation=2, e=-2 and e=-1 are anticipation)
  pre_idx <- which(egt < -2)
  pre_egt <- egt[pre_idx]
  pre_att <- att[pre_idx]
  pre_se  <- se[pre_idx]
  n_pre   <- length(pre_idx)

  message(sprintf("  Pre-period event times tested: %s", paste(pre_egt, collapse = ", ")))
  message(sprintf("  Number of pre-period coefficients: %d", n_pre))

  if (n_pre >= 2) {
    # Extract sub-VCOV for pre-period coefficients
    V_pre <- V_full[pre_idx, pre_idx, drop = FALSE]

    # Joint Wald test: W = beta' * V^{-1} * beta ~ chi^2(n_pre)
    V_pre_inv <- tryCatch(solve(V_pre), error = function(e) {
      message("  V_pre singular, using generalized inverse")
      MASS::ginv(V_pre)
    })

    wald_joint <- as.numeric(t(pre_att) %*% V_pre_inv %*% pre_att)
    p_joint <- 1 - pchisq(wald_joint, df = n_pre)

    # Also compute simplified Wald (for comparison)
    wald_simple <- sum((pre_att / pre_se)^2)
    p_simple <- 1 - pchisq(wald_simple, df = n_pre)

    message(sprintf("  Joint Wald test (proper VCOV): W = %.3f, df = %d, p = %.4f",
                    wald_joint, n_pre, p_joint))
    message(sprintf("  Simplified Wald (independence): W = %.3f, df = %d, p = %.4f",
                    wald_simple, n_pre, p_simple))

    pretrend_result <- data.frame(
      test = c("Joint Wald (proper VCOV)", "Simplified Wald (independence)"),
      statistic = c(wald_joint, wald_simple),
      df = c(n_pre, n_pre),
      p_value = c(p_joint, p_simple),
      n_pre_periods = c(n_pre, n_pre),
      pre_period_range = c(paste(range(pre_egt), collapse = " to "),
                            paste(range(pre_egt), collapse = " to "))
    )
  } else {
    message("  Fewer than 2 pre-period coefficients; skipping joint test")
    pretrend_result <- data.frame(
      test = "Insufficient pre-periods",
      statistic = NA_real_, df = NA_integer_,
      p_value = NA_real_, n_pre_periods = n_pre,
      pre_period_range = NA_character_
    )
  }

  readr::write_csv(pretrend_result, file.path(DIR_OUT_TABLES, "pretrends_joint_test.csv"))
  message("  Wrote outputs/tables/pretrends_joint_test.csv")

}, error = function(e) {
  message("  Joint pre-trends test failed: ", conditionMessage(e))
})


# ============================================================================
# 4) Covariate Balance Table Across Treatment Cohorts
# ============================================================================
message("\n=== [4] Covariate Balance Table ===")

# Get treatment cohort for each county (use pre-period data)
county_cohort <- df_main %>%
  dplyr::group_by(id_num) %>%
  dplyr::summarise(
    G_int = dplyr::first(G_int),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    cohort_label = dplyr::case_when(
      G_int == 0 ~ "Never treated",
      TRUE ~ {
        y <- G_int %/% 12
        m <- G_int %% 12
        sprintf("G=%d-%02d", y, m)
      }
    )
  )

# Merge with panel characteristics (pre-treatment period only)
pre_data <- panel_raw %>%
  dplyr::filter(date >= as.Date("2014-01-01"), date <= as.Date("2016-12-31")) %>%
  dplyr::mutate(id_num = as.integer(factor(id)))

balance_vars <- c("unemployment_rate", "y_per1k_18_49", "poverty_rate",
                   "bachelor_share", "svi_total", "urban_dummy")
balance_vars <- intersect(balance_vars, names(pre_data))

county_chars <- pre_data %>%
  dplyr::group_by(id_num) %>%
  dplyr::summarise(
    dplyr::across(dplyr::all_of(balance_vars),
                  ~ mean(as.numeric(.x), na.rm = TRUE),
                  .names = "mean_{.col}"),
    county_name = dplyr::first(county),
    .groups = "drop"
  )

county_chars <- county_chars %>%
  dplyr::left_join(county_cohort, by = "id_num")

# Balance table: means by cohort
balance_table <- county_chars %>%
  dplyr::group_by(cohort_label) %>%
  dplyr::summarise(
    n_counties = dplyr::n(),
    dplyr::across(dplyr::starts_with("mean_"), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(n_counties == 0), cohort_label)

readr::write_csv(balance_table, file.path(DIR_OUT_TABLES, "covariate_balance_cohorts.csv"))
message("  Covariate balance by cohort:")
for (i in seq_len(nrow(balance_table))) {
  row <- balance_table[i, ]
  msg_parts <- sprintf("%-12s (n=%2d):", row$cohort_label, row$n_counties)
  for (v in balance_vars) {
    col_name <- paste0("mean_", v)
    if (col_name %in% names(row)) {
      msg_parts <- paste0(msg_parts, sprintf("  %s=%.2f", v, row[[col_name]]))
    }
  }
  message("    ", msg_parts)
}
message("  Wrote outputs/tables/covariate_balance_cohorts.csv")


# ============================================================================
# 5) Age-Group Transferability Sensitivity
# ============================================================================
message("\n=== [5] Age-Group Transferability Sensitivity ===")

# How do projected losses change if the 55-64 ATT differs from the 18-49 ATT?
# Multiplier: 1.0 means identical ATT; 1.5 means 50% larger effect for 55-64
path_scenarios <- file.path(DIR_DERIVED, "forecast_scenarios.rds")

if (file.exists(path_scenarios)) {
  fc <- readRDS(path_scenarios)
  baseline_loss <- fc$summary$total_loss_approx[fc$summary$scenario == "baseline"]

  # Multipliers: 0.5x (more optimistic), 0.75x, 1.0x (baseline), 1.25x, 1.5x, 2.0x
  multipliers <- c(0.50, 0.75, 1.00, 1.25, 1.50, 2.00)
  transfer_results <- data.frame(
    att_multiplier = multipliers,
    interpretation = c(
      "55-64 respond half as much as 18-49",
      "55-64 respond 25% less",
      "Same response (baseline assumption)",
      "55-64 respond 25% more",
      "55-64 respond 50% more",
      "55-64 respond twice as much"
    ),
    projected_loss = baseline_loss * multipliers,
    stringsAsFactors = FALSE
  )

  readr::write_csv(transfer_results,
                   file.path(DIR_OUT_TABLES, "age_transferability_sensitivity.csv"))
  message("  Age-group transferability sensitivity:")
  for (i in seq_len(nrow(transfer_results))) {
    message(sprintf("    Multiplier %.2f: projected loss = %s (%s)",
                    transfer_results$att_multiplier[i],
                    format(round(transfer_results$projected_loss[i]), big.mark = ","),
                    transfer_results$interpretation[i]))
  }
  message("  Wrote outputs/tables/age_transferability_sensitivity.csv")
} else {
  message("  forecast_scenarios.rds not found; skipping transferability sensitivity")
}


# ============================================================================
# Summary
# ============================================================================
message("\n=== Additional Robustness Complete ===")
message("Outputs:")
message("  outputs/tables/bacon_decomposition.csv")
message("  outputs/tables/leave_one_cohort_out.csv")
message("  outputs/tables/pretrends_joint_test.csv")
message("  outputs/tables/covariate_balance_cohorts.csv")
message("  outputs/tables/age_transferability_sensitivity.csv")
message("  outputs/figures/bacon_decomposition.png")
