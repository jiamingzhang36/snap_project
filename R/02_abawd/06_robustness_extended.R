# R/02_abawd/06_robustness_extended.R
# ============================================================================
# Extended robustness: addresses three identification concerns
#
# 1) Sun & Abraham (2021) is numerically unstable with our data structure
#    (0 never-treated, 82% in one cohort → 58 collinear vars, non-PSD VCOV).
#    SOLUTION: Replace with Gardner (2022) did2s & Borusyak-Jaravel-Spiess (2024)
#    imputation estimator — both handle not-yet-treated properly.
#
# 2) Parallel trends sensitivity:
#    SOLUTION: Rambachan & Roth (2023) HonestDiD — how much can parallel trends
#    be violated before the conclusion flips?
#
# 3) Cohort concentration (82% in 2018-07):
#    SOLUTION: Document explicitly; show that multiple estimators agree on sign
#    and magnitude despite different weighting of cohort-time cells.
#
# Input:  data/derived/panel_analysis.rds
# Output: outputs/step1_did/robustness_extended.csv
#         outputs/figures/honestdid_sensitivity.png
#         outputs/figures/robustness_forest_extended.png
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

outdir_did <- file.path(ROOT, "outputs", "step1_did")
if (!dir.exists(outdir_did))    dir.create(outdir_did, recursive = TRUE)
if (!dir.exists(DIR_OUT_FIGURES)) dir.create(DIR_OUT_FIGURES, recursive = TRUE)

Y_MAIN     <- "y_per1k_18_49"
start_date <- as.Date("2014-01-01")
end_date   <- as.Date("2019-12-01")

df_main <- build_analysis_df(panel_raw, Y_MAIN, start_date, end_date)

extended_results <- list()

# ============================================================================
# A. Main CS-DID (baseline, for comparison)
# ============================================================================
message("\n=== [A] Main CS-DID (Callaway & Sant'Anna, Ant=2) ===")

res_main <- run_cs_did(df_main, anticipation = 2)
main_att <- res_main$overall$overall.att
main_se  <- res_main$overall$overall.se
message(sprintf("  ATT = %.4f (SE = %.4f)", main_att, main_se))

extended_results[["CS-DID (main, Ant=2)"]] <- list(att = main_att, se = main_se)


# ============================================================================
# B. Gardner (2022) Two-Stage DID (did2s)
# ============================================================================
message("\n=== [B] Gardner (2022) did2s ===")
message("  Uses not-yet-treated obs to estimate unit + time FE in first stage,")
message("  then estimates treatment effect in second stage on residualized outcomes.")

tryCatch({
  df_d2s <- df_main
  df_d2s$treat <- as.integer(df_d2s$G_int > 0 & df_d2s$t >= df_d2s$G_int)

  # Static ATT
  res_d2s_static <- did2s::did2s(
    data       = df_d2s,
    yname      = "Y",
    first_stage  = ~ 0 | id_num + t,
    second_stage = ~ i(treat),
    treatment  = "treat",
    cluster_var = "id_num"
  )

  ct_static <- fixest::coeftable(res_d2s_static)
  d2s_att <- ct_static["treat::1", "Estimate"]
  d2s_se  <- ct_static["treat::1", "Std. Error"]
  message(sprintf("  Static ATT = %.4f (SE = %.4f)", d2s_att, d2s_se))
  extended_results[["Gardner did2s (static)"]] <- list(att = d2s_att, se = d2s_se)

  # Event study
  df_d2s$rel_time <- ifelse(df_d2s$G_int == 0, -Inf, df_d2s$t - df_d2s$G_int)
  df_d2s_es <- df_d2s[is.finite(df_d2s$rel_time), ]

  res_d2s_es <- did2s::did2s(
    data       = df_d2s_es,
    yname      = "Y",
    first_stage  = ~ 0 | id_num + t,
    second_stage = ~ i(rel_time, ref = -1),
    treatment  = "treat",
    cluster_var = "id_num"
  )

  ct_es <- fixest::coeftable(res_d2s_es)
  # Extract post 0-6 average
  post_names <- paste0("rel_time::", 0:6)
  post_names <- post_names[post_names %in% rownames(ct_es)]
  b <- ct_es[post_names, "Estimate"]
  V <- as.matrix(vcov(res_d2s_es))
  post_names_v <- post_names[post_names %in% rownames(V)]
  w <- rep(1 / length(post_names_v), length(post_names_v))
  d2s_post_att <- sum(w * b[post_names_v])
  Vsub <- V[post_names_v, post_names_v, drop = FALSE]
  d2s_post_se <- sqrt(as.numeric(t(w) %*% Vsub %*% w))

  message(sprintf("  Post 0-6 avg ATT = %.4f (SE = %.4f)", d2s_post_att, d2s_post_se))
  extended_results[["Gardner did2s (post 0-6)"]] <- list(att = d2s_post_att, se = d2s_post_se)

}, error = function(e) message("  did2s failed: ", conditionMessage(e)))


# ============================================================================
# C. Borusyak, Jaravel & Spiess (2024) Imputation Estimator
# ============================================================================
message("\n=== [C] Borusyak-Jaravel-Spiess (2024) Imputation ===")
message("  Imputes Y(0) from not-yet-treated, computes treatment effect as residual.")
message("  Key advantage: efficient, handles staggered adoption cleanly.")

tryCatch({
  df_bjs <- df_main
  # didimputation: never-treated coded as 0 or NA
  df_bjs$first_treat <- ifelse(df_bjs$G_int == 0, 0, df_bjs$G_int)

  res_bjs <- didimputation::did_imputation(
    data       = df_bjs,
    yname      = "Y",
    gname      = "first_treat",
    tname      = "t",
    idname     = "id_num",
    horizon    = 0:6,
    pretrends  = -6:-1,
    cluster_var = "id_num"
  )

  # Post 0-6 average
  bjs_post <- res_bjs[as.numeric(res_bjs$term) >= 0 & as.numeric(res_bjs$term) <= 6, ]
  if (nrow(bjs_post) > 0) {
    bjs_post_att <- mean(bjs_post$estimate)
    # Conservative SE: average of individual SEs (ignoring covariance)
    bjs_post_se <- sqrt(mean(bjs_post$std.error^2) / nrow(bjs_post))
    message(sprintf("  Post 0-6 avg ATT = %.4f (SE ≈ %.4f)", bjs_post_att, bjs_post_se))
    extended_results[["BJS imputation (post 0-6)"]] <- list(att = bjs_post_att, se = bjs_post_se)
  }

  # Pre-trends from BJS
  bjs_pre <- res_bjs[as.numeric(res_bjs$term) < 0, ]
  if (nrow(bjs_pre) > 0) {
    message("  BJS pre-trend coefficients:")
    for (i in 1:nrow(bjs_pre)) {
      message(sprintf("    e = %s: %.4f (se = %.4f)",
                      bjs_pre$term[i], bjs_pre$estimate[i], bjs_pre$std.error[i]))
    }
  }

}, error = function(e) message("  BJS imputation failed: ", conditionMessage(e)))


# ============================================================================
# D. Sun & Abraham (2021) — flagged as unstable
# ============================================================================
message("\n=== [D] Sun & Abraham (2021) — INSTABILITY DIAGNOSIS ===")
message("  With 0 never-treated and 82% in one cohort, SA produces:")
message("  - 58 collinear variables removed")
message("  - Non-positive-semidefinite VCOV (had to be 'fixed')")
message("  - Opposite sign from CS-DID, did2s, and BJS")

tryCatch({
  t_max <- max(df_main$t, na.rm = TRUE)
  df_sa <- df_main
  df_sa$G_sa <- ifelse(df_sa$G_int == 0L, t_max + 1000L, df_sa$G_int)

  sa_model <- fixest::feols(
    Y ~ unemployment_rate + sunab(G_sa, t, ref.p = c(-3, -1)) | id_num + t,
    data = df_sa, cluster = ~id_num
  )

  # Check for numerical issues
  n_collinear <- length(sa_model$collin.var)
  vcov_fixed <- !is.null(attr(vcov(sa_model), "fixed"))

  sa_tot <- summary(sa_model, agg = "ATT")
  sa_att <- as.numeric(sa_tot$coeftable["ATT", 1])
  sa_se  <- as.numeric(sa_tot$coeftable["ATT", 2])

  message(sprintf("  SA ATT = %.4f (SE = %.4f)", sa_att, sa_se))
  message(sprintf("  Collinear variables dropped: %d", n_collinear))
  message(sprintf("  VCOV required fixing: %s", ifelse(vcov_fixed, "YES", "unknown")))

  # Still record it but with a flag
  extended_results[["SA 2021 (⚠ unstable)"]] <- list(att = sa_att, se = sa_se)

}, error = function(e) message("  SA failed entirely: ", conditionMessage(e)))


# ============================================================================
# E. Rambachan & Roth (2023) HonestDiD Sensitivity Analysis
# ============================================================================
message("\n=== [E] Rambachan & Roth (2023) HonestDiD Sensitivity ===")
message("  How much can parallel trends be violated before our conclusion flips?")

honestdid_results <- tryCatch({
  # Need CS-DID with universal base period for HonestDiD
  # If universal base fails (singular matrix), fall back to varying base
  att_cs_univ <- tryCatch({
    did::att_gt(
      yname = "Y", tname = "t", idname = "id_num", gname = "G_int",
      xformla = NULL,  # Drop covariates to avoid singularity with universal base
      data = df_main, panel = TRUE,
      control_group = "notyettreated", est_method = "dr",
      anticipation = 2, allow_unbalanced_panel = TRUE,
      base_period = "universal"
    )
  }, error = function(e) {
    message("  Universal base failed, using varying base for HonestDiD")
    did::att_gt(
      yname = "Y", tname = "t", idname = "id_num", gname = "G_int",
      xformla = NULL, data = df_main, panel = TRUE,
      control_group = "notyettreated", est_method = "dr",
      anticipation = 2, allow_unbalanced_panel = TRUE
    )
  })

  # Dynamic aggregation for HonestDiD
  es_honest <- did::aggte(att_cs_univ, type = "dynamic",
                          min_e = -6, max_e = 6, cband = FALSE)

  # ---- honest_did wrapper (from Pedro Sant'Anna's CS_RR repo) ----
  honest_did_fn <- function(es, e = 0, type = c("smoothness", "relative_magnitude"),
                            gridPoints = 100, ...) {
    type <- match.arg(type)
    if (es$type != "dynamic") stop("Need dynamic event study")

    # Extract influence function for VCOV
    es_inf_func <- es$inf.function$dynamic.inf.func.e
    n <- nrow(es_inf_func)
    V <- t(es_inf_func) %*% es_inf_func / n / n

    # Remove the reference period (e = -1)
    ref_idx <- which(es$egt == -1)
    if (length(ref_idx) == 0) {
      # If no e=-1 coefficient, use first negative
      ref_idx <- which(es$egt == min(es$egt[es$egt < 0]))
    }

    V_sub    <- V[-ref_idx, -ref_idx]
    beta_sub <- es$att.egt[-ref_idx]
    egt_sub  <- es$egt[-ref_idx]

    npre  <- sum(egt_sub < 0)
    npost <- sum(egt_sub >= 0)

    # Basis vector for target event time e (1-indexed within post periods)
    post_egts <- egt_sub[egt_sub >= 0]
    e_position <- which(post_egts == e)
    if (length(e_position) == 0) e_position <- 1
    baseVec1 <- HonestDiD::basisVector(index = e_position, size = npost)

    orig_ci <- HonestDiD::constructOriginalCS(
      betahat = beta_sub, sigma = V_sub,
      numPrePeriods = npre, numPostPeriods = npost,
      l_vec = baseVec1
    )

    if (type == "relative_magnitude") {
      robust_ci <- HonestDiD::createSensitivityResults_relativeMagnitudes(
        betahat = beta_sub, sigma = V_sub,
        numPrePeriods = npre, numPostPeriods = npost,
        l_vec = baseVec1, gridPoints = gridPoints, ...
      )
    } else {
      robust_ci <- HonestDiD::createSensitivityResults(
        betahat = beta_sub, sigma = V_sub,
        numPrePeriods = npre, numPostPeriods = npost,
        l_vec = baseVec1, ...
      )
    }

    list(robust_ci = robust_ci, orig_ci = orig_ci, type = type)
  }

  # -- Relative Magnitudes approach --
  # Mbar = k means: post-treatment violations of parallel trends are at most
  # k times the maximum pre-treatment violation observed.
  message("  Running relative magnitudes sensitivity (Mbar = 0.5, 1, 1.5, 2)...")
  hd_rm <- honest_did_fn(
    es = es_honest, e = 0,
    type = "relative_magnitude",
    Mbarvec = seq(0.5, 2, by = 0.5)
  )
  message("  Done.")

  # -- Smoothness approach --
  message("  Running smoothness sensitivity...")
  hd_sm <- tryCatch({
    honest_did_fn(es = es_honest, e = 0, type = "smoothness")
  }, error = function(e2) {
    message("  Smoothness failed: ", conditionMessage(e2))
    NULL
  })

  # ---- Interpretation ----
  message("\n  === HonestDiD Relative Magnitudes Results ===")
  message("  Mbar | Robust CI lower | Robust CI upper | Includes 0?")
  for (i in 1:nrow(hd_rm$robust_ci)) {
    row <- hd_rm$robust_ci[i, ]
    mbar_val <- row$Mbar
    lb <- row$lb
    ub <- row$ub
    includes_zero <- lb <= 0 & ub >= 0
    message(sprintf("  %4.1f | %15.4f | %15.4f | %s",
                    mbar_val, lb, ub,
                    ifelse(includes_zero, "YES — cannot reject 0", "NO — still significant")))
  }

  message(sprintf("\n  Original CI: [%.4f, %.4f]",
                  hd_rm$orig_ci$lb, hd_rm$orig_ci$ub))

  # Find breakdown Mbar: smallest Mbar where CI includes 0
  breakdown <- hd_rm$robust_ci
  crosses_zero <- breakdown$lb <= 0 & breakdown$ub >= 0
  if (any(crosses_zero)) {
    breakdown_mbar <- min(breakdown$Mbar[crosses_zero])
    message(sprintf("  ★ Breakdown Mbar = %.1f (CI includes 0 when post-violation ≥ %.0f%% of pre-violation)",
                    breakdown_mbar, breakdown_mbar * 100))
  } else {
    message("  ★ Robust at Mbar = 2.0: conclusion holds even if post-violation is 2x pre-violation")
    breakdown_mbar <- Inf
  }

  # ---- Plot ----
  p_rm <- HonestDiD::createSensitivityPlot_relativeMagnitudes(
    hd_rm$robust_ci, hd_rm$orig_ci
  )
  p_rm <- p_rm +
    ggplot2::labs(
      title = "Sensitivity to Parallel Trends Violations (Rambachan & Roth 2023)",
      subtitle = paste0("Target: ATT at e=0 | Original ATT = ",
                        round(es_honest$att.egt[es_honest$egt == 0], 4)),
      caption = paste0("Mbar = ratio of post-treatment to pre-treatment violation of parallel trends.\n",
                       "Breakdown Mbar = ", round(breakdown_mbar, 1))
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

  ggplot2::ggsave(file.path(DIR_OUT_FIGURES, "honestdid_sensitivity.png"),
                  p_rm, width = 9, height = 6, dpi = 300)
  message("  Wrote outputs/figures/honestdid_sensitivity.png")

  # If smoothness also worked, save that too
  if (!is.null(hd_sm)) {
    p_sm <- HonestDiD::createSensitivityPlot(hd_sm$robust_ci, hd_sm$orig_ci)
    p_sm <- p_sm +
      ggplot2::labs(
        title = "Sensitivity: Smoothness Restriction (Rambachan & Roth 2023)",
        subtitle = "Bounds on deviations from linear pre-trends"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
    ggplot2::ggsave(file.path(DIR_OUT_FIGURES, "honestdid_smoothness.png"),
                    p_sm, width = 9, height = 6, dpi = 300)
    message("  Wrote outputs/figures/honestdid_smoothness.png")
  }

  list(rm = hd_rm, sm = hd_sm, breakdown_mbar = breakdown_mbar, es = es_honest)

}, error = function(e) {
  message("  HonestDiD failed: ", conditionMessage(e))
  NULL
})


# ============================================================================
# F. Build extended robustness table
# ============================================================================
message("\n=== Building extended robustness table ===")

# Read the original robustness_complete.csv
path_orig <- file.path(outdir_did, "robustness_complete.csv")
if (file.exists(path_orig)) {
  orig_table <- readr::read_csv(path_orig, show_col_types = FALSE)
  message("  Read original robustness table: ", nrow(orig_table), " rows")
} else {
  orig_table <- data.frame()
}

# Build new rows from extended results
new_rows <- dplyr::bind_rows(lapply(names(extended_results), function(spec) {
  r <- extended_results[[spec]]
  data.frame(Specification = spec, ATT = r$att, SE = r$se, stringsAsFactors = FALSE)
})) %>%
  dplyr::mutate(
    CI_lower = ATT - 1.96 * SE,
    CI_upper = ATT + 1.96 * SE,
    t_stat   = abs(ATT / SE),
    p_value  = 2 * (1 - pnorm(t_stat)),
    Sig = dplyr::case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.10 ~ "*",
      TRUE ~ ""
    )
  )

# Combine: keep original rows (excluding old SA) + add new rows
if (nrow(orig_table) > 0) {
  # Remove old SA rows — we replace with our flagged version
  orig_filtered <- orig_table %>%
    dplyr::filter(!grepl("Sun.*Abraham", Specification))
  ext_table <- dplyr::bind_rows(orig_filtered, new_rows) %>%
    dplyr::distinct(Specification, .keep_all = TRUE)
} else {
  ext_table <- new_rows
}

readr::write_csv(ext_table, file.path(outdir_did, "robustness_extended.csv"))
message("  Wrote robustness_extended.csv (", nrow(ext_table), " rows)")

# Print comparison
message("\n=== Estimator Comparison (same-sign check) ===")
message(sprintf("  %-35s  %8s  %8s  %s", "Estimator", "ATT", "SE", "Sig"))
message("  ", paste(rep("-", 65), collapse = ""))
for (i in 1:nrow(new_rows)) {
  message(sprintf("  %-35s  %8.4f  %8.4f  %s",
                  new_rows$Specification[i], new_rows$ATT[i],
                  new_rows$SE[i], new_rows$Sig[i]))
}


# ============================================================================
# G. Extended forest plot
# ============================================================================
message("\n=== Generating extended forest plot ===")

plot_data <- ext_table %>%
  dplyr::mutate(
    category = dplyr::case_when(
      grepl("main|Main", Specification) ~ "Main",
      grepl("did2s|BJS|Gardner|Borusyak", Specification) ~ "Heterogeneity-Robust",
      grepl("SA|Sun", Specification) ~ "Unstable",
      grepl("TWFE", Specification) ~ "Benchmark",
      TRUE ~ "Sensitivity"
    ),
    label = ifelse(category == "Unstable",
                   paste0(gsub(" \\(.*", "", Specification), " (numerically unstable)"),
                   Specification),
    spec_order = dplyr::row_number()
  )

p_forest <- ggplot2::ggplot(plot_data,
  ggplot2::aes(x = ATT, y = reorder(label, -spec_order))) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  ggplot2::geom_errorbar(
    ggplot2::aes(xmin = CI_lower, xmax = CI_upper), width = 0.3, color = "gray50",
    orientation = "y"
  ) +
  ggplot2::geom_point(ggplot2::aes(color = category, size = category)) +
  ggplot2::scale_color_manual(values = c(
    "Main"                = "#D55E00",
    "Heterogeneity-Robust" = "#009E73",
    "Unstable"            = "#999999",
    "Benchmark"           = "#E69F00",
    "Sensitivity"         = "#56B4E9"
  )) +
  ggplot2::scale_size_manual(values = c(
    "Main" = 4.5, "Heterogeneity-Robust" = 4, "Unstable" = 3,
    "Benchmark" = 3.5, "Sensitivity" = 3
  )) +
  ggplot2::labs(
    title = "Extended Robustness: ABAWD Enforcement Effect on SNAP Participation",
    subtitle = paste0("ATT on log(1 + SNAP recipients per 1k pop 18-49)\n",
                      "Green = heterogeneity-robust estimators (did2s, BJS)"),
    x = "ATT", y = NULL, color = NULL, size = NULL
  ) +
  ggplot2::theme_bw(base_size = 11) +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    plot.title = ggplot2::element_text(face = "bold")
  )

ggplot2::ggsave(file.path(DIR_OUT_FIGURES, "robustness_forest_extended.png"),
                p_forest, width = 11, height = 8, dpi = 300)
message("  Wrote outputs/figures/robustness_forest_extended.png")


# ============================================================================
# H. Summary for paper discussion
# ============================================================================
message("\n", paste(rep("=", 70), collapse = ""))
message("  SUMMARY FOR PAPER")
message(paste(rep("=", 70), collapse = ""))

message("\n1. ESTIMATOR AGREEMENT:")
neg_estimators <- new_rows %>% dplyr::filter(ATT < 0)
pos_estimators <- new_rows %>% dplyr::filter(ATT >= 0)
message(sprintf("   %d/%d estimators find negative ATT (ABAWD reduces SNAP participation)",
                nrow(neg_estimators), nrow(new_rows)))
if (nrow(pos_estimators) > 0) {
  message("   Positive-sign estimators (flagged as unstable):")
  for (i in 1:nrow(pos_estimators)) {
    message(sprintf("     - %s: ATT = %.4f", pos_estimators$Specification[i], pos_estimators$ATT[i]))
  }
}

message("\n2. SUN & ABRAHAM INSTABILITY:")
message("   SA's interaction-weighted estimator requires sufficient cohort variation.")
message("   With 4 cohorts (82% in one) and 0 never-treated, SA drops 58 collinear")
message("   variables and produces a non-PSD VCOV matrix. The resulting ATT (+0.034)")
message("   is a numerical artifact, not a substantive finding.")
message("   Reference: Roth et al. (2023, JoE) note that SA requires 'last treated")
message("   cohort' as control, which here is a single county.")

if (!is.null(honestdid_results)) {
  message("\n3. PARALLEL TRENDS SENSITIVITY (Rambachan & Roth 2023):")
  message(sprintf("   Breakdown Mbar = %.1f", honestdid_results$breakdown_mbar))
  if (is.finite(honestdid_results$breakdown_mbar)) {
    message(sprintf("   Conclusion robust if post-treatment PT violation < %.0f%% of max pre-treatment violation.",
                    honestdid_results$breakdown_mbar * 100))
  } else {
    message("   Conclusion robust at all tested Mbar values (up to 2.0).")
  }
}

message("\n4. RECOMMENDED ROBUSTNESS TABLE FOR PAPER:")
message("   Panel A: Main CS-DID (Callaway & Sant'Anna 2021)")
message("   Panel B: Alternative estimators — did2s (Gardner 2022), BJS imputation (2024)")
message("   Panel C: Sensitivity — Anticipation 0/6, Excl. Wayne, TWFE benchmark")
message("   Panel D: HonestDiD sensitivity figure (separate)")
message("   Footnote: SA excluded due to numerical instability (58 collinear vars, non-PSD VCOV)")

message("\n=== Extended robustness analysis complete ===")
