# R/02_abawd/08_robustness_nicetohave.R
# ============================================================================
# Nice-to-have robustness checks (supplementary):
#   (1) Permutation / randomization inference (Fisher exact p-value)
#   (2) Stacking estimator a la Cengiz-Dube-Lindner-Zipperer (2019)
#   (3) Two-way clustering (county + time) for TWFE
#   (4) ML-based moderator selection (LASSO)
#
# Input:  data/derived/panel_analysis.rds
# Output: outputs/tables/permutation_inference.csv
#         outputs/tables/stacking_estimator.csv
#         outputs/tables/twoway_clustering.csv
#         outputs/tables/lasso_moderator_selection.csv
#         outputs/figures/permutation_distribution.png
# ============================================================================

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)
source("R/00_utils/helpers_did.R", local = TRUE)

set.seed(20240302)

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

# Baseline CS-DID for comparison
res_main <- run_cs_did(df_main, anticipation = 2)
main_att <- res_main$overall$overall.att
main_se  <- res_main$overall$overall.se
message(sprintf("Baseline ATT = %.4f (SE = %.4f)", main_att, main_se))


# ============================================================================
# 1) Permutation / Randomization Inference
# ============================================================================
message("\n=== [1] Permutation / Randomization Inference ===")
message("  Tests the sharp null of no treatment effect by randomly permuting")
message("  treatment-timing assignments across counties.")

tryCatch({
  N_PERM <- 500

  # Extract county-level treatment timing
  county_G <- df_main %>%
    dplyr::distinct(id_num, G_int)

  # Observed TWFE ATT (faster than CS-DID; valid under sharp null)
  df_perm_base <- df_main
  df_perm_base$post <- as.integer(df_perm_base$G_int > 0 &
                                    df_perm_base$t >= df_perm_base$G_int)

  m_obs <- fixest::feols(Y ~ post | id_num + t, data = df_perm_base, cluster = ~id_num)
  t_obs <- coef(m_obs)["post"]

  message(sprintf("  Observed TWFE ATT = %.4f", t_obs))
  message(sprintf("  Running %d permutations (shuffling treatment timing across counties)...",
                  N_PERM))

  t_perm <- numeric(N_PERM)

  for (b in seq_len(N_PERM)) {
    if (b %% 100 == 0) message(sprintf("    Permutation %d/%d", b, N_PERM))

    # Shuffle G_int across counties (preserves marginal distribution of cohort sizes)
    perm_G <- county_G
    perm_G$G_int_perm <- sample(perm_G$G_int)

    # Map permuted treatment timing back to panel
    df_b <- df_main %>%
      dplyr::left_join(perm_G %>% dplyr::select(id_num, G_int_perm), by = "id_num") %>%
      dplyr::mutate(post_perm = as.integer(G_int_perm > 0 & t >= G_int_perm))

    m_b <- fixest::feols(Y ~ post_perm | id_num + t, data = df_b)
    t_perm[b] <- coef(m_b)["post_perm"]
  }

  # Fisher exact p-value (two-sided)
  p_fisher <- mean(abs(t_perm) >= abs(t_obs))
  # One-sided: proportion at least as negative
  p_fisher_onesided <- mean(t_perm <= t_obs)

  message(sprintf("  Fisher exact p-value (two-sided): %.4f", p_fisher))
  message(sprintf("  Fisher exact p-value (one-sided): %.4f", p_fisher_onesided))
  message(sprintf("  Permutation distribution: mean = %.4f, sd = %.4f",
                  mean(t_perm), sd(t_perm)))

  perm_result <- data.frame(
    test = c("Observed TWFE ATT", "Fisher p (two-sided)", "Fisher p (one-sided)",
             "Permutation mean", "Permutation SD", "N permutations",
             "CS-DID ATT (reference)"),
    value = c(t_obs, p_fisher, p_fisher_onesided,
              mean(t_perm), sd(t_perm), N_PERM,
              main_att)
  )
  readr::write_csv(perm_result, file.path(DIR_OUT_TABLES, "permutation_inference.csv"))

  # Plot: histogram of permutation distribution with observed value
  p_hist <- ggplot2::ggplot(data.frame(t_stat = t_perm), ggplot2::aes(x = t_stat)) +
    ggplot2::geom_histogram(bins = 40, fill = "steelblue", alpha = 0.7, color = "white") +
    ggplot2::geom_vline(xintercept = t_obs, color = "firebrick",
                         linewidth = 1.2, linetype = "solid") +
    ggplot2::geom_vline(xintercept = -t_obs, color = "firebrick",
                         linewidth = 0.8, linetype = "dashed") +
    ggplot2::annotate("text", x = t_obs, y = Inf,
                       label = paste0("Observed = ", round(t_obs, 3)),
                       vjust = 2, hjust = -0.1, color = "firebrick",
                       fontface = "bold", size = 4) +
    ggplot2::annotate("text", x = max(t_perm) * 0.5, y = Inf,
                       label = paste0("Fisher p = ", round(p_fisher, 3)),
                       vjust = 4, hjust = 0.5, color = "gray30", size = 3.5) +
    ggplot2::labs(
      title = "Permutation Inference: Distribution Under Sharp Null",
      subtitle = paste0(N_PERM,
                        " random permutations of treatment-timing assignment across counties"),
      x = "Permuted TWFE ATT", y = "Count"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

  ggplot2::ggsave(file.path(DIR_OUT_FIGURES, "permutation_distribution.png"),
                  p_hist, width = 9, height = 6, dpi = 300)
  message("  Wrote outputs/figures/permutation_distribution.png")
  message("  Wrote outputs/tables/permutation_inference.csv")

}, error = function(e) {
  message("  Permutation inference failed: ", conditionMessage(e))
})


# ============================================================================
# 2) Stacking Estimator (Cengiz-Dube-Lindner-Zipperer 2019)
# ============================================================================
message("\n=== [2] Stacking Estimator (CDLZ 2019) ===")
message("  For each cohort, construct a clean 2x2 sub-experiment with")
message("  not-yet-treated controls, then stack and estimate with cohort FEs.")

tryCatch({
  K_MAX <- 6  # target months before/after treatment

  cohort_values <- sort(unique(df_main$G_int[df_main$G_int > 0]))
  message(sprintf("  Treatment cohorts: %s",
                  paste(cohort_values, collapse = ", ")))

  stacked_list <- list()

  for (g in cohort_values) {
    treated_ids <- unique(df_main$id_num[df_main$G_int == g])

    # Clean controls: not yet treated within the window [g, g+K]
    # Try K from K_MAX down to 2 until we get >= 2 clean control counties
    k_use <- 0
    clean_control_ids <- integer(0)

    for (k in seq(K_MAX, 2, by = -1)) {
      candidates <- unique(df_main$id_num[df_main$G_int == 0 | df_main$G_int > g + k])
      if (length(candidates) >= 2) {
        clean_control_ids <- candidates
        k_use <- k
        break
      }
    }

    if (length(clean_control_ids) < 2) {
      message(sprintf("  Cohort G=%d: skipping (insufficient clean controls)", g))
      next
    }

    # Time window [g - k_use, g + k_use]
    t_min <- g - k_use
    t_max <- g + k_use

    df_g <- df_main %>%
      dplyr::filter(id_num %in% c(treated_ids, clean_control_ids),
                    t >= t_min, t <= t_max) %>%
      dplyr::mutate(
        cohort_id = g,
        treated_g = as.integer(id_num %in% treated_ids),
        post_g    = as.integer(t >= g),
        did_term  = treated_g * post_g,
        # Cohort-specific identifiers for stacking
        stack_id = paste0(g, "_", id_num),
        stack_t  = paste0(g, "_", t)
      )

    stacked_list[[as.character(g)]] <- df_g
    message(sprintf("  Cohort G=%d: %d treated, %d clean controls, K=%d, %d obs",
                    g, length(treated_ids), length(clean_control_ids),
                    k_use, nrow(df_g)))
  }

  if (length(stacked_list) == 0) stop("No valid sub-experiments for stacking")

  # Stack all sub-experiments
  df_stacked <- dplyr::bind_rows(stacked_list)

  message(sprintf("  Stacked dataset: %d obs from %d sub-experiments",
                  nrow(df_stacked), length(stacked_list)))

  # Estimate: Y ~ DiD | stack_id + stack_t
  # Cohort-specific unit FE + cohort-specific time FE
  m_stack <- fixest::feols(
    Y ~ did_term | stack_id + stack_t,
    data = df_stacked,
    cluster = ~id_num  # cluster at original county level
  )

  stack_att <- coef(m_stack)["did_term"]
  stack_se  <- sqrt(vcov(m_stack)["did_term", "did_term"])
  stack_ci  <- stack_att + c(-1, 1) * 1.96 * stack_se

  message(sprintf("  Stacking ATT = %.4f (SE = %.4f)", stack_att, stack_se))
  message(sprintf("  95%% CI: [%.4f, %.4f]", stack_ci[1], stack_ci[2]))
  message(sprintf("  vs CS-DID ATT = %.4f (SE = %.4f)", main_att, main_se))

  # Also estimate with two-way clustering (county + cohort)
  m_stack_2way <- fixest::feols(
    Y ~ did_term | stack_id + stack_t,
    data = df_stacked,
    vcov = ~id_num + cohort_id
  )
  stack_se_2way <- sqrt(vcov(m_stack_2way)["did_term", "did_term"])

  stack_result <- data.frame(
    estimator = c("Stacking CDLZ (county cluster)",
                  "Stacking CDLZ (county+cohort cluster)",
                  "CS-DID (baseline)"),
    att = c(stack_att, coef(m_stack_2way)["did_term"], main_att),
    se = c(stack_se, stack_se_2way, main_se),
    ci_lower = c(stack_ci[1],
                 coef(m_stack_2way)["did_term"] - 1.96 * stack_se_2way,
                 main_att - 1.96 * main_se),
    ci_upper = c(stack_ci[2],
                 coef(m_stack_2way)["did_term"] + 1.96 * stack_se_2way,
                 main_att + 1.96 * main_se),
    n_obs = c(nrow(df_stacked), nrow(df_stacked), nrow(df_main)),
    n_subexperiments = c(length(stacked_list), length(stacked_list), NA_integer_),
    stringsAsFactors = FALSE
  )

  readr::write_csv(stack_result, file.path(DIR_OUT_TABLES, "stacking_estimator.csv"))
  message("  Wrote outputs/tables/stacking_estimator.csv")

}, error = function(e) {
  message("  Stacking estimator failed: ", conditionMessage(e))
})


# ============================================================================
# 3) Two-Way Clustering (County + Time)
# ============================================================================
message("\n=== [3] Two-Way Clustering (County + Time) ===")
message("  Accounts for both within-county serial correlation and")
message("  within-period cross-sectional correlation (Cameron-Gelbach-Miller 2011).")

tryCatch({
  df_twfe <- df_main
  df_twfe$post <- as.integer(df_twfe$G_int > 0 & df_twfe$t >= df_twfe$G_int)

  # One-way: county only (baseline TWFE)
  m_1way <- fixest::feols(Y ~ post | id_num + t, data = df_twfe, cluster = ~id_num)

  # Two-way: county + time (Cameron-Gelbach-Miller)
  m_2way <- fixest::feols(Y ~ post | id_num + t, data = df_twfe, vcov = ~id_num + t)

  # Heteroskedasticity-robust only (no clustering)
  m_hc1 <- fixest::feols(Y ~ post | id_num + t, data = df_twfe, vcov = "hetero")

  twoway_result <- data.frame(
    clustering = c("County only (baseline)",
                   "County + Time (two-way)",
                   "HC1 (heteroskedasticity-robust)"),
    att = c(coef(m_1way)["post"],
            coef(m_2way)["post"],
            coef(m_hc1)["post"]),
    se = c(sqrt(vcov(m_1way)["post", "post"]),
           sqrt(vcov(m_2way)["post", "post"]),
           sqrt(vcov(m_hc1)["post", "post"])),
    n_obs = c(nobs(m_1way), nobs(m_2way), nobs(m_hc1)),
    n_counties = c(length(unique(df_twfe$id_num)),
                   length(unique(df_twfe$id_num)),
                   length(unique(df_twfe$id_num))),
    n_periods = c(length(unique(df_twfe$t)),
                  length(unique(df_twfe$t)),
                  length(unique(df_twfe$t))),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(
      t_stat = att / se,
      p_value = 2 * (1 - pnorm(abs(t_stat))),
      ci_lower = att - 1.96 * se,
      ci_upper = att + 1.96 * se,
      sig = dplyr::case_when(
        p_value < 0.01 ~ "***",
        p_value < 0.05 ~ "**",
        p_value < 0.10 ~ "*",
        TRUE ~ ""
      )
    )

  readr::write_csv(twoway_result, file.path(DIR_OUT_TABLES, "twoway_clustering.csv"))

  for (i in seq_len(nrow(twoway_result))) {
    message(sprintf("  %-40s ATT = %.4f (SE = %.4f) %s",
                    twoway_result$clustering[i],
                    twoway_result$att[i],
                    twoway_result$se[i],
                    twoway_result$sig[i]))
  }

  message(sprintf("  For reference: CS-DID ATT = %.4f (bootstrap SE = %.4f)", main_att, main_se))
  message("  Note: CS-DID uses multiplier bootstrap for inference, which accounts for")
  message("  serial correlation within counties. Two-way clustering additionally accounts")
  message("  for cross-sectional correlation within time periods.")
  message("  Wrote outputs/tables/twoway_clustering.csv")

}, error = function(e) {
  message("  Two-way clustering failed: ", conditionMessage(e))
})


# ============================================================================
# 4) ML-Based Moderator Selection (LASSO)
# ============================================================================
message("\n=== [4] ML-Based Moderator Selection (LASSO) ===")
message("  Uses LASSO to data-drive moderator selection, then compares")
message("  with the hand-picked moderators from the OAT interaction analysis.")

tryCatch({
  # Install glmnet if needed
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    install.packages("glmnet", repos = "https://cran.r-project.org")
  }

  # ------------------------------------------------------------------
  # Strategy:
  # 1. Compute county-level treatment effect proxy: residualized DiD
  #    = mean(Y_resid | post=1) - mean(Y_resid | post=0)
  #    where Y_resid removes common time effects using all counties
  # 2. LASSO-regress proxy on pre-treatment county characteristics
  # 3. Compare selected variables with hand-picked moderators
  # ------------------------------------------------------------------

  df_lasso <- df_main
  df_lasso$post <- as.integer(df_lasso$G_int > 0 & df_lasso$t >= df_lasso$G_int)

  # Step 1: Remove time FEs from Y
  m_time <- fixest::feols(Y ~ 1 | t, data = df_lasso)
  df_lasso$Y_resid <- residuals(m_time)

  # County-level DiD proxy (for treated counties only)
  treated_counties <- df_lasso %>%
    dplyr::filter(G_int > 0) %>%
    dplyr::group_by(id_num, id) %>%
    dplyr::summarise(
      y_pre  = mean(Y_resid[post == 0], na.rm = TRUE),
      y_post = mean(Y_resid[post == 1], na.rm = TRUE),
      did_proxy = y_post - y_pre,
      n_pre  = sum(post == 0),
      n_post = sum(post == 1),
      G_int  = dplyr::first(G_int),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_pre >= 6, n_post >= 3)

  message(sprintf("  %d treated counties with sufficient pre/post observations",
                  nrow(treated_counties)))

  # Step 2: Merge pre-treatment county characteristics
  pre_end <- as.Date("2016-12-31")
  pre_data <- panel_raw %>%
    dplyr::filter(date >= as.Date("2014-01-01"), date <= pre_end)

  char_vars <- c("unemployment_rate", "poverty_rate", "bachelor_share",
                 "svi_total", "urban_dummy")
  char_vars <- intersect(char_vars, names(pre_data))

  # Also add baseline SNAP intensity
  if (Y_MAIN %in% names(pre_data)) {
    char_vars <- c(char_vars, Y_MAIN)
  }

  county_chars_lasso <- pre_data %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(char_vars),
                    ~ mean(as.numeric(.x), na.rm = TRUE),
                    .names = "{.col}"),
      .groups = "drop"
    )

  treated_with_chars <- treated_counties %>%
    dplyr::left_join(county_chars_lasso, by = "id") %>%
    dplyr::filter(complete.cases(dplyr::across(dplyr::all_of(char_vars))))

  message(sprintf("  LASSO sample: %d counties with complete characteristics",
                  nrow(treated_with_chars)))

  # Step 3: LASSO regression
  X_raw <- as.matrix(treated_with_chars[, char_vars])
  X_std <- scale(X_raw)
  # Store centering/scaling for interpretation
  x_center <- attr(X_std, "scaled:center")
  x_scale  <- attr(X_std, "scaled:scale")
  y_vec <- treated_with_chars$did_proxy

  # Cross-validated LASSO (alpha = 1)
  n_folds <- min(10, nrow(X_std))
  cv_fit <- glmnet::cv.glmnet(X_std, y_vec, alpha = 1, nfolds = n_folds)

  # Coefficients at lambda.min and lambda.1se
  coef_min <- as.matrix(coef(cv_fit, s = "lambda.min"))
  coef_1se <- as.matrix(coef(cv_fit, s = "lambda.1se"))

  lasso_result <- data.frame(
    variable = rownames(coef_min),
    coef_lambda_min = as.numeric(coef_min),
    coef_lambda_1se = as.numeric(coef_1se),
    selected_min = as.numeric(coef_min) != 0,
    selected_1se = as.numeric(coef_1se) != 0,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::filter(variable != "(Intercept)") %>%
    dplyr::arrange(dplyr::desc(abs(coef_lambda_min)))

  readr::write_csv(lasso_result, file.path(DIR_OUT_TABLES, "lasso_moderator_selection.csv"))

  message("  LASSO moderator selection results (standardized coefficients):")
  message(sprintf("  %-25s  %12s  %12s  %8s  %8s",
                  "Variable", "Coef(min)", "Coef(1se)", "Sel(min)", "Sel(1se)"))
  message("  ", paste(rep("-", 72), collapse = ""))
  for (i in seq_len(nrow(lasso_result))) {
    message(sprintf("  %-25s  %12.4f  %12.4f  %8s  %8s",
                    lasso_result$variable[i],
                    lasso_result$coef_lambda_min[i],
                    lasso_result$coef_lambda_1se[i],
                    ifelse(lasso_result$selected_min[i], "YES", ""),
                    ifelse(lasso_result$selected_1se[i], "YES", "")))
  }

  # Summary
  selected_min <- lasso_result$variable[lasso_result$selected_min]
  selected_1se <- lasso_result$variable[lasso_result$selected_1se]
  message(sprintf("\n  Selected at lambda.min: %s",
                  ifelse(length(selected_min) > 0,
                         paste(selected_min, collapse = ", "), "(none)")))
  message(sprintf("  Selected at lambda.1se: %s",
                  ifelse(length(selected_1se) > 0,
                         paste(selected_1se, collapse = ", "), "(intercept only)")))

  # Cross-check with hand-picked moderators
  hand_picked <- c("poverty_rate", "bachelor_share")
  overlap_min <- intersect(hand_picked, selected_min)
  overlap_1se <- intersect(hand_picked, selected_1se)
  message(sprintf("  Overlap with hand-picked (poverty, bachelor): %s at lambda.min",
                  ifelse(length(overlap_min) > 0,
                         paste(overlap_min, collapse = ", "), "none")))

  # Also run Elastic Net (alpha = 0.5) as a check
  cv_enet <- glmnet::cv.glmnet(X_std, y_vec, alpha = 0.5, nfolds = n_folds)
  coef_enet <- as.matrix(coef(cv_enet, s = "lambda.min"))
  enet_selected <- rownames(coef_enet)[as.numeric(coef_enet) != 0]
  enet_selected <- setdiff(enet_selected, "(Intercept)")
  message(sprintf("  Elastic net (alpha=0.5) at lambda.min selects: %s",
                  ifelse(length(enet_selected) > 0,
                         paste(enet_selected, collapse = ", "), "(none)")))

  message("  Wrote outputs/tables/lasso_moderator_selection.csv")

}, error = function(e) {
  message("  ML moderator selection failed: ", conditionMessage(e))
})


# ============================================================================
# Summary
# ============================================================================
message("\n=== Nice-to-Have Robustness Complete ===")
message("Outputs:")
message("  outputs/tables/permutation_inference.csv")
message("  outputs/tables/stacking_estimator.csv")
message("  outputs/tables/twoway_clustering.csv")
message("  outputs/tables/lasso_moderator_selection.csv")
message("  outputs/figures/permutation_distribution.png")
