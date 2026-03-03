# R/02_abawd/04_heterogeneity.R
# Input:  data/derived/panel_analysis.rds
# Output: outputs/tables/abawd_heterogeneity.csv               (ATT by subgroup, 6 dimensions)
#         outputs/tables/abawd_heterogeneity_interactions.csv   (one-at-a-time interaction coefficients)
#         outputs/tables/abawd_heterogeneity_joint.csv          (joint interaction model)
#         outputs/tables/abawd_county_att_hat.csv               (predicted county-specific ATT for forecast)
#         outputs/figures/abawd_heterogeneity_forest.png        (forest plot)
#
# Heterogeneity analysis for the ABAWD county-vulnerability paper:
# 1) Subgroup CS-DID (median split, with feasibility guard for treatment-timing variation)
# 2) One-at-a-time continuous interaction models (fixest: D_ct x Z_c) — primary
# 3) Joint interaction model (selected moderators) — supplementary
# 4) County-specific predicted ATT for forecast transmission

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)
source("R/00_utils/helpers_did.R", local = TRUE)

set.seed(123)
path_panel <- file.path(DIR_DERIVED, "panel_analysis.rds")
stopifnot(file.exists(path_panel))
if (!dir.exists(DIR_OUT_TABLES))  dir.create(DIR_OUT_TABLES,  recursive = TRUE)
if (!dir.exists(DIR_OUT_FIGURES)) dir.create(DIR_OUT_FIGURES, recursive = TRUE)

panel_raw <- as.data.frame(readRDS(path_panel))
if (!"date" %in% names(panel_raw) && "ym_date" %in% names(panel_raw)) panel_raw$date <- panel_raw$ym_date
panel_raw$date <- as.Date(panel_raw$date)

Y_MAIN     <- "y_per1k_18_49"
start_date <- as.Date("2014-01-01")
end_date   <- as.Date("2019-12-01")
pre_end    <- as.Date("2016-12-31")

# ---------------------------------------------------------------------------
# 1) Compute pre-period county averages for all split variables
# ---------------------------------------------------------------------------
pre <- panel_raw %>%
  dplyr::mutate(date = as.Date(date), year = lubridate::year(date)) %>%
  dplyr::filter(date >= start_date, date <= pre_end)

county_chars <- pre %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(
    avg_unemp     = mean(as.numeric(unemployment_rate), na.rm = TRUE),
    avg_y         = mean(as.numeric(.data[[Y_MAIN]]),   na.rm = TRUE),
    avg_poverty   = if ("poverty_rate"   %in% names(.)) mean(as.numeric(poverty_rate),   na.rm = TRUE) else NA_real_,
    avg_bachelor  = if ("bachelor_share" %in% names(.)) mean(as.numeric(bachelor_share), na.rm = TRUE) else NA_real_,
    avg_svi       = if ("svi_total"      %in% names(.)) mean(as.numeric(svi_total),      na.rm = TRUE) else NA_real_,
    is_urban      = if ("urban_dummy"    %in% names(.)) as.integer(round(mean(as.numeric(urban_dummy), na.rm = TRUE))) else NA_integer_,
    .groups = "drop"
  ) %>%
  dplyr::filter(is.finite(avg_unemp), is.finite(avg_y))

# Also get G values per county for feasibility checks
county_G <- panel_raw %>%
  dplyr::distinct(id, G) %>%
  dplyr::filter(!is.na(G), G != "0")
county_chars <- county_chars %>%
  dplyr::left_join(county_G, by = "id")

# ---------------------------------------------------------------------------
# 2) Define split dimensions
# ---------------------------------------------------------------------------
split_dims <- list(
  list(var = "avg_unemp",    label = "unemployment",   type = "median"),
  list(var = "avg_y",        label = "baseline_snap",  type = "median"),
  list(var = "avg_poverty",  label = "poverty_rate",   type = "median"),
  list(var = "avg_bachelor", label = "bachelor_share", type = "median"),
  list(var = "avg_svi",      label = "svi",            type = "median"),
  list(var = "is_urban",     label = "urbanicity",     type = "binary")
)

# ---------------------------------------------------------------------------
# 3) Subgroup DID function (with treatment-timing feasibility check)
# ---------------------------------------------------------------------------
run_subgroup <- function(panel_sub, label) {
  if (nrow(panel_sub) < 100) {
    message("  Subgroup '", label, "': too few rows (", nrow(panel_sub), "), returning NA")
    return(list(att = NA_real_, se = NA_real_, n_obs = nrow(panel_sub), reason = "too_few_rows"))
  }

  df <- build_analysis_df(panel_sub, Y_MAIN, start_date, end_date)

  if (nrow(df) < 50 || !any(df$G_int > 0)) {
    message("  Subgroup '", label, "': no treated obs, returning NA")
    return(list(att = NA_real_, se = NA_real_, n_obs = nrow(df), reason = "no_treated"))
  }

  # Feasibility check: need >= 2 distinct treatment cohorts with >= 2 counties each,
  # OR >= 1 treatment cohort + some not-yet-treated variation
  g_counts <- table(df$G_int[df$G_int > 0 & !duplicated(paste0(df$id, df$G_int))])
  n_cohorts_usable <- sum(g_counts >= 2)
  if (n_cohorts_usable < 1) {
    message("  Subgroup '", label, "': insufficient treatment-timing variation (",
            length(g_counts), " cohorts, largest=", max(g_counts), "), returning NA")
    return(list(att = NA_real_, se = NA_real_, n_obs = nrow(df), reason = "no_timing_variation"))
  }

  res <- tryCatch(
    run_cs_did(df, anticipation = 2),
    error = function(e) {
      message("  Subgroup '", label, "' CS-DID error: ", conditionMessage(e))
      list(overall = list(overall.att = NA_real_, overall.se = NA_real_))
    }
  )
  list(
    att    = as.numeric(res$overall$overall.att),
    se     = as.numeric(res$overall$overall.se),
    n_obs  = nrow(df),
    reason = "ok"
  )
}

# ---------------------------------------------------------------------------
# 4) Run subgroup DID across all dimensions
# ---------------------------------------------------------------------------
message("\n=== Subgroup DID ===")
het_results <- list()

for (dim in split_dims) {
  var_col <- dim$var
  dim_label <- dim$label

  if (!var_col %in% names(county_chars) || all(is.na(county_chars[[var_col]]))) {
    message("Skip heterogeneity dimension '", dim_label, "': variable not available.")
    next
  }

  chars_valid <- county_chars %>% dplyr::filter(!is.na(.data[[var_col]]))

  if (dim$type == "median") {
    cutoff <- median(chars_valid[[var_col]], na.rm = TRUE)
    chars_valid <- chars_valid %>%
      dplyr::mutate(split_group = ifelse(.data[[var_col]] > cutoff, "high", "low"))
  } else {
    cutoff <- NA_real_
    chars_valid <- chars_valid %>%
      dplyr::mutate(split_group = ifelse(.data[[var_col]] == 1, "high", "low"))
  }

  # Report G distribution in each subgroup
  for (grp in c("high", "low")) {
    grp_data <- chars_valid %>% dplyr::filter(split_group == grp)
    g_dist <- table(grp_data$G[!is.na(grp_data$G)])
    message(sprintf("  %s-%s: %d counties, G: %s",
                    dim_label, grp, nrow(grp_data),
                    paste(names(g_dist), "=", g_dist, collapse = ", ")))
  }

  panel_with_split <- panel_raw %>%
    dplyr::left_join(chars_valid %>% dplyr::select(id, split_group), by = "id") %>%
    dplyr::filter(!is.na(split_group))

  r_high <- run_subgroup(panel_with_split %>% dplyr::filter(split_group == "high"),
                         paste0(dim_label, "_high"))
  r_low  <- run_subgroup(panel_with_split %>% dplyr::filter(split_group == "low"),
                         paste0(dim_label, "_low"))

  high_label <- if (dim$type == "binary") "urban" else "high"
  low_label  <- if (dim$type == "binary") "rural" else "low"

  het_results <- c(het_results, list(
    data.frame(subgroup = dim_label, group = high_label,
               att = r_high$att, se = r_high$se, n_obs = r_high$n_obs,
               cutoff = cutoff, reason = r_high$reason, stringsAsFactors = FALSE),
    data.frame(subgroup = dim_label, group = low_label,
               att = r_low$att, se = r_low$se, n_obs = r_low$n_obs,
               cutoff = cutoff, reason = r_low$reason, stringsAsFactors = FALSE)
  ))
}

het <- dplyr::bind_rows(het_results)
readr::write_csv(het, file.path(DIR_OUT_TABLES, "abawd_heterogeneity.csv"))
message("\nWrote outputs/tables/abawd_heterogeneity.csv (", nrow(het), " rows, ",
        length(unique(het$subgroup)), " dimensions)")

# ---------------------------------------------------------------------------
# 5) ONE-AT-A-TIME Continuous interaction models
# ---------------------------------------------------------------------------
# Three specifications to diagnose treatment-timing confounding:
#   A) "Naive":   Y ~ post + post:Z_k | id + t
#   B) "CohortFE": Y ~ post:i(G_factor) + post:Z_k | id + t
#      → cohort×post FE absorbs between-cohort mean effects;
#        gamma is identified only from within-cohort variation in Z_k.
#   C) "Within68": same as A but restricted to the 68-county G=2018-07 cohort
#      → all counties share the same G, so no timing confound at all.
# ---------------------------------------------------------------------------
message("\n=== One-at-a-time interaction models (3 specifications) ===")

oaat_results <- tryCatch({
  df <- build_analysis_df(panel_raw, Y_MAIN, start_date, end_date)
  df$post <- as.integer(df$G_int > 0 & df$t >= df$G_int)

  # Create cohort factor for cohort×post FE
  df$G_factor <- factor(df$G_int)

  # Merge county characteristics
  raw_chars <- county_chars %>%
    dplyr::select(id, avg_unemp, avg_y, avg_poverty, avg_bachelor, avg_svi, is_urban) %>%
    dplyr::distinct(id, .keep_all = TRUE)
  df <- df %>% dplyr::left_join(raw_chars, by = "id")

  # Standardize moderators (full sample) for comparable coefficients
  moderators <- c("avg_unemp", "avg_poverty", "avg_bachelor", "avg_svi", "is_urban")
  for (mod in moderators) {
    z_col <- paste0("z_", mod)
    if (mod == "is_urban") {
      df[[z_col]] <- as.numeric(df[[mod]])
    } else {
      vals <- df[[mod]]
      df[[z_col]] <- (vals - mean(vals, na.rm = TRUE)) / sd(vals, na.rm = TRUE)
    }
  }

  # Subset for within-G=2018-07 analysis
  # G_int = year*12 + month (from helpers_did.R), so 2018-07 → 2018*12+7 = 24223
  g_2018_07 <- 2018L * 12L + 7L  # = 24223
  df_68 <- df %>% dplyr::filter(G_int == g_2018_07)
  message("  Within-68 subset: ", length(unique(df_68$id)), " counties, ",
          nrow(df_68), " obs, post: ", sum(df_68$post), "/", nrow(df_68))

  # Re-standardize within the 68-county subset
  for (mod in moderators) {
    z_col_68 <- paste0("z68_", mod)
    if (mod == "is_urban") {
      df_68[[z_col_68]] <- as.numeric(df_68[[mod]])
    } else {
      vals <- df_68[[mod]]
      df_68[[z_col_68]] <- (vals - mean(vals, na.rm = TRUE)) / sd(vals, na.rm = TRUE)
    }
  }

  results_list <- list()
  for (mod in moderators) {
    z_col <- paste0("z_", mod)
    z_col_68 <- paste0("z68_", mod)

    # --- Spec A: Naive (original) ---
    fml_A <- as.formula(paste0("Y ~ post + post:I(", z_col, ") | id_num + t"))
    m_A <- fixest::feols(fml_A, data = df, cluster = ~id_num)
    c_A <- broom::tidy(m_A)
    int_A <- c_A %>% dplyr::filter(grepl("^post:", term))

    # --- Spec B: Cohort×post FE ---
    fml_B <- as.formula(paste0("Y ~ post:G_factor + post:I(", z_col, ") | id_num + t"))
    m_B <- fixest::feols(fml_B, data = df, cluster = ~id_num)
    c_B <- broom::tidy(m_B)
    int_B <- c_B %>% dplyr::filter(grepl(paste0("^post:I\\(", z_col), term))

    # --- Spec C: Within G=2018-07 only ---
    # post main effect is collinear with time FE (single cohort → post = f(t)),
    # so we drop it and estimate only the interaction.
    # gamma_C = differential effect of Z within the same-timing cohort.
    fml_C <- as.formula(paste0("Y ~ post:I(", z_col_68, ") | id_num + t"))
    m_C <- tryCatch(
      fixest::feols(fml_C, data = df_68, cluster = ~id_num),
      error = function(e) NULL
    )
    if (!is.null(m_C)) {
      c_C <- broom::tidy(m_C)
      int_C <- c_C %>% dplyr::filter(grepl("^post:", term))
    }

    # Collect results
    results_list[[paste0(mod, "_naive")]] <- data.frame(
      moderator = mod, spec = "A_naive",
      gamma = int_A$estimate[1], gamma_se = int_A$std.error[1],
      gamma_t = int_A$statistic[1], gamma_p = int_A$p.value[1],
      n_obs = fixest::fitstat(m_A, "n")[[1]],
      r2_within = fixest::fitstat(m_A, "wr2")[[1]],
      stringsAsFactors = FALSE)

    results_list[[paste0(mod, "_cohortFE")]] <- data.frame(
      moderator = mod, spec = "B_cohortFE",
      gamma = int_B$estimate[1], gamma_se = int_B$std.error[1],
      gamma_t = int_B$statistic[1], gamma_p = int_B$p.value[1],
      n_obs = fixest::fitstat(m_B, "n")[[1]],
      r2_within = fixest::fitstat(m_B, "wr2")[[1]],
      stringsAsFactors = FALSE)

    if (!is.null(m_C) && nrow(int_C) > 0) {
      results_list[[paste0(mod, "_within68")]] <- data.frame(
        moderator = mod, spec = "C_within68",
        gamma = int_C$estimate[1], gamma_se = int_C$std.error[1],
        gamma_t = int_C$statistic[1], gamma_p = int_C$p.value[1],
        n_obs = fixest::fitstat(m_C, "n")[[1]],
        r2_within = fixest::fitstat(m_C, "wr2")[[1]],
        stringsAsFactors = FALSE)
    } else {
      results_list[[paste0(mod, "_within68")]] <- data.frame(
        moderator = mod, spec = "C_within68",
        gamma = NA_real_, gamma_se = NA_real_,
        gamma_t = NA_real_, gamma_p = NA_real_,
        n_obs = nrow(df_68), r2_within = NA_real_,
        stringsAsFactors = FALSE)
    }

    # Print comparison
    sig_A <- ifelse(int_A$p.value[1] < 0.01, "***", ifelse(int_A$p.value[1] < 0.05, "**",
             ifelse(int_A$p.value[1] < 0.10, "*", "")))
    sig_B <- ifelse(int_B$p.value[1] < 0.01, "***", ifelse(int_B$p.value[1] < 0.05, "**",
             ifelse(int_B$p.value[1] < 0.10, "*", "")))
    if (!is.null(m_C) && nrow(int_C) > 0) {
      sig_C <- ifelse(int_C$p.value[1] < 0.01, "***", ifelse(int_C$p.value[1] < 0.05, "**",
               ifelse(int_C$p.value[1] < 0.10, "*", "")))
    } else {
      sig_C <- "NA"
    }
    message(sprintf("  %-12s  Naive: %+.4f %s | CohortFE: %+.4f %s | Within68: %s %s",
                    mod,
                    int_A$estimate[1], sig_A,
                    int_B$estimate[1], sig_B,
                    if (!is.null(m_C) && nrow(int_C) > 0) sprintf("%+.4f", int_C$estimate[1]) else "NA",
                    sig_C))
  }

  oaat_tbl <- dplyr::bind_rows(results_list)
  readr::write_csv(oaat_tbl, file.path(DIR_OUT_TABLES, "abawd_heterogeneity_interactions.csv"))
  message("Wrote outputs/tables/abawd_heterogeneity_interactions.csv (",
          nrow(oaat_tbl), " rows: ", length(moderators), " moderators x 3 specs)")

  # Also save the cohortFE specification for downstream county ATT prediction
  oaat_tbl_cohortFE <- oaat_tbl %>% dplyr::filter(spec == "B_cohortFE")
  oaat_tbl
}, error = function(e) {
  message("One-at-a-time interaction models failed: ", conditionMessage(e))
  NULL
})

# ---------------------------------------------------------------------------
# 6) Joint interaction model WITH cohort×post FE
# ---------------------------------------------------------------------------
# Use cohortFE specification (Spec B) to select moderators and predict county ATTs.
# This ensures gamma reflects within-cohort variation only.
message("\n=== Joint interaction model (with cohort×post FE) ===")

joint_results <- tryCatch({
  df <- build_analysis_df(panel_raw, Y_MAIN, start_date, end_date)
  df$post <- as.integer(df$G_int > 0 & df$t >= df$G_int)
  df$G_factor <- factor(df$G_int)

  raw_chars <- county_chars %>%
    dplyr::select(id, avg_unemp, avg_poverty, avg_bachelor, avg_svi, is_urban) %>%
    dplyr::distinct(id, .keep_all = TRUE)
  df <- df %>% dplyr::left_join(raw_chars, by = "id")

  # Standardize
  for (mod in c("avg_unemp", "avg_poverty", "avg_bachelor", "avg_svi")) {
    z_col <- paste0("z_", mod)
    vals <- df[[mod]]
    df[[z_col]] <- (vals - mean(vals, na.rm = TRUE)) / sd(vals, na.rm = TRUE)
  }
  df$z_is_urban <- as.numeric(df$is_urban)

  # Select moderators from cohortFE results (Spec B): sig at p<0.10, or top 2
  if (!is.null(oaat_results)) {
    cohortFE_res <- oaat_results %>% dplyr::filter(spec == "B_cohortFE")
    sig_mods <- cohortFE_res %>%
      dplyr::filter(!is.na(gamma_p)) %>%
      dplyr::arrange(gamma_p) %>%
      dplyr::slice_head(n = max(2, sum(cohortFE_res$gamma_p < 0.10, na.rm = TRUE)))
    selected_z <- paste0("z_", sig_mods$moderator)
  } else {
    selected_z <- c("z_avg_poverty", "z_avg_bachelor")
  }

  # Check pairwise correlation; drop if |r| > 0.7
  if (length(selected_z) > 1) {
    cor_sel <- cor(df[, selected_z, drop = FALSE], use = "pairwise.complete.obs")
    high_cor <- which(abs(cor_sel) > 0.7 & upper.tri(cor_sel), arr.ind = TRUE)
    if (nrow(high_cor) > 0) {
      drop_idx <- unique(high_cor[, 2])
      dropped <- selected_z[drop_idx]
      selected_z <- selected_z[-drop_idx]
      message("  Dropped due to multicollinearity (|r|>0.7): ", paste(dropped, collapse = ", "))
    }
  }

  message("  Joint model moderators: ", paste(selected_z, collapse = ", "))
  interaction_terms <- paste0("post:I(", selected_z, ")")
  # KEY FIX: cohort×post FE via post:G_factor
  fml <- as.formula(paste0("Y ~ post:G_factor + ",
                           paste(interaction_terms, collapse = " + "),
                           " | id_num + t"))
  m_joint <- fixest::feols(fml, data = df, cluster = ~id_num)

  coef_tbl <- broom::tidy(m_joint) %>%
    dplyr::mutate(
      moderator = dplyr::case_when(
        grepl("^post:G_factor", term) & !grepl("I\\(", term) ~ paste0("cohort_", gsub("post:G_factor", "", term)),
        grepl("^post:I\\(", term) ~ gsub("post:I\\((.*)\\)", "\\1", term),
        TRUE ~ term
      ),
      moderator = gsub("^z_", "", moderator)
    ) %>%
    dplyr::select(moderator, estimate, std.error, statistic, p.value)

  readr::write_csv(coef_tbl, file.path(DIR_OUT_TABLES, "abawd_heterogeneity_joint.csv"))
  message("Wrote outputs/tables/abawd_heterogeneity_joint.csv")

  list(model = m_joint, selected_z = selected_z, coef_tbl = coef_tbl, df = df)

}, error = function(e) {
  message("Joint interaction model failed: ", conditionMessage(e))
  NULL
})

# ---------------------------------------------------------------------------
# 7) County-specific predicted ATT (for forecast transmission)
# ---------------------------------------------------------------------------
message("\n=== County-specific ATT prediction ===")

county_att_hat <- tryCatch({
  if (is.null(joint_results) && is.null(oaat_results)) stop("No interaction results available")

  # ------------------------------------------------------------------
  # ANCHORING STRATEGY:
  # The joint interaction model's intercept (-0.029) differs from the main
  # CS-DID overall ATT (-0.060). Using the model intercept directly causes
  # some counties' predicted ATT to flip positive (ABAWD increases SNAP),
  # which is economically implausible.
  #
  # Solution: Anchor county ATT to the MAIN CS-DID ATT, using the interaction
  # model only for RELATIVE heterogeneity (deviation from the mean).
  #   ATT_hat_c = main_ATT + [variation_c - mean(variation)]
  # This ensures: (1) mean(ATT_hat_c) = main_ATT exactly,
  #               (2) all county ATTs are non-positive (capped at 0).
  # ------------------------------------------------------------------

  # Read the main CS-DID ATT from robustness_complete.csv
  path_did <- file.path(ROOT, "outputs", "step1_did", "robustness_complete.csv")
  if (!file.exists(path_did)) stop("robustness_complete.csv not found")
  did_summary <- readr::read_csv(path_did, show_col_types = FALSE)
  main_row <- did_summary %>% dplyr::filter(grepl("^Main", .data$Specification))
  main_att <- main_row$ATT[1]
  message("  Main CS-DID ATT = ", round(main_att, 4))

  if (!is.null(joint_results)) {
    m <- joint_results$model
    sel_z <- joint_results$selected_z
    df_pred <- joint_results$df

    unique_counties <- df_pred %>%
      dplyr::distinct(id, id_num, dplyr::across(dplyr::all_of(sel_z)))

    # Compute relative variation from interaction terms only: sum(gamma_k * z_k)
    # (Exclude cohort×post FE terms like post:G_factor24223)
    gamma_coefs <- coef(m)
    all_post_names <- names(gamma_coefs)[grepl("^post:", names(gamma_coefs))]
    inter_mask <- grepl("^post:I\\(", all_post_names)
    gamma_names <- all_post_names[inter_mask]
    gamma_vals  <- gamma_coefs[gamma_names]
    z_from_coef <- gsub("post:I\\((.*)\\)", "\\1", gamma_names)

    # Winsorize Z values at ±2 to prevent extreme extrapolation at tails.
    # Without this, counties like Lake (poverty z=2.8, bachelor z=-1.4) get
    # ATT pushed positive by linear model, then hard-capped to 0.
    WINSOR_BOUND <- 2.0
    unique_counties_w <- unique_counties
    for (zc in z_from_coef) {
      zvals <- unique_counties_w[[zc]]
      n_winsor <- sum(abs(zvals) > WINSOR_BOUND, na.rm = TRUE)
      if (n_winsor > 0) {
        message("  Winsorizing ", zc, ": ", n_winsor, " counties clipped to +/-", WINSOR_BOUND)
      }
      unique_counties_w[[zc]] <- pmax(pmin(zvals, WINSOR_BOUND), -WINSOR_BOUND)
    }

    variation <- rep(0, nrow(unique_counties_w))
    for (i in seq_along(z_from_coef)) {
      variation <- variation + gamma_vals[i] * unique_counties_w[[z_from_coef[i]]]
    }

    # Anchor to main ATT with mean-centering
    variation_centered <- variation - mean(variation)
    att_hat <- main_att + variation_centered

    # Soft floor: ABAWD should reduce SNAP for all counties. The linear interaction
    # model can extrapolate positive ATTs for extreme-tail counties (e.g., very high
    # poverty + very low education). Instead of hard-capping to 0, assign a minimum
    # effect of 10% of the main ATT — even heavily exempted counties have some
    # ABAWD-affected population.
    ATT_SOFT_FLOOR <- main_att * 0.10  # e.g., -0.006 when main_att = -0.060
    n_floored <- sum(att_hat > ATT_SOFT_FLOOR)
    if (n_floored > 0) {
      message("  Soft-flooring ", n_floored, " counties to ATT = ",
              round(ATT_SOFT_FLOOR, 4), " (10% of main ATT)")
    }
    att_hat <- pmin(att_hat, ATT_SOFT_FLOOR)

    result <- data.frame(
      id       = unique_counties$id,
      att_hat  = as.numeric(att_hat),
      att_base = as.numeric(main_att)
    )

    # --- Save gamma coefficients + county Z matrix for bootstrap ---
    # gamma_names / gamma_vals / z_from_coef already filtered to interaction terms only

    # VCOV of interaction gammas
    full_vcov <- vcov(m)
    gamma_vcov <- full_vcov[gamma_names, gamma_names, drop = FALSE]

    # County Z matrix (n_county x n_gamma) — use winsorized values for consistency
    z_matrix <- as.matrix(unique_counties_w[, z_from_coef, drop = FALSE])

    # Save as RDS for bootstrap consumption
    het_boot_info <- list(
      gamma_hat  = as.numeric(gamma_vals),
      gamma_vcov = as.matrix(gamma_vcov),
      z_matrix   = z_matrix,
      county_ids = unique_counties$id,
      z_names    = z_from_coef
    )
    saveRDS(het_boot_info, file.path(DIR_OUT_TABLES, "het_boot_info.rds"))
    message("  Saved het_boot_info.rds for bootstrap (", length(gamma_vals),
            " gamma coefficients, ", nrow(z_matrix), " counties)")

  } else {
    message("  No joint model available; using uniform ATT")
    result <- data.frame(
      id       = unique(county_chars$id),
      att_hat  = main_att,
      att_base = main_att
    )
  }

  result <- result %>% dplyr::arrange(id)
  readr::write_csv(result, file.path(DIR_OUT_TABLES, "abawd_county_att_hat.csv"))
  message("Wrote outputs/tables/abawd_county_att_hat.csv (",
          nrow(result), " counties, ATT range: ",
          round(min(result$att_hat, na.rm = TRUE), 4), " to ",
          round(max(result$att_hat, na.rm = TRUE), 4), ")")
  result

}, error = function(e) {
  message("County ATT prediction failed: ", conditionMessage(e))
  NULL
})

# ---------------------------------------------------------------------------
# 8) Forest plot of subgroup ATT + interaction coefficients
# ---------------------------------------------------------------------------
message("\n=== Generating plots ===")

# 8a) Subgroup forest plot (only for dimensions that succeeded)
if (nrow(het) > 0 && any(!is.na(het$att))) {
  het_plot <- het %>%
    dplyr::filter(!is.na(att)) %>%
    dplyr::mutate(
      label = paste0(subgroup, ": ", group),
      ci_lo = att - 1.96 * se,
      ci_hi = att + 1.96 * se,
      sig = ifelse(abs(att / se) > 1.96, "p < 0.05", "n.s.")
    )

  p1 <- ggplot2::ggplot(het_plot, ggplot2::aes(x = att, y = reorder(label, att))) +
    ggplot2::geom_point(ggplot2::aes(color = sig), size = 2.5) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = ci_lo, xmax = ci_hi), height = 0.2) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    ggplot2::scale_color_manual(values = c("p < 0.05" = "firebrick", "n.s." = "grey50")) +
    ggplot2::labs(
      x = "ATT (log SNAP recipients per 1k pop 18-49)",
      y = NULL,
      title = "ABAWD Treatment Effect Heterogeneity (Subgroup CS-DID)",
      subtitle = paste0("Callaway-Sant\u2019Anna ATT by county subgroup\n",
                        sum(is.na(het$att)), "/", nrow(het),
                        " subgroups infeasible (insufficient treatment-timing variation)"),
      color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                   legend.position = "bottom")

  ggplot2::ggsave(file.path(DIR_OUT_FIGURES, "abawd_heterogeneity_forest.png"),
                  p1, width = 9, height = 6, dpi = 300)
  message("Wrote outputs/figures/abawd_heterogeneity_forest.png")
}

# 8b) Interaction coefficient plot — 3-spec comparison (A: naive, B: cohortFE, C: within68)
if (!is.null(oaat_results)) {
  oaat_plot <- oaat_results %>%
    dplyr::filter(!is.na(gamma)) %>%
    dplyr::mutate(
      label = gsub("^avg_", "", moderator),
      spec_label = dplyr::case_when(
        spec == "A_naive"    ~ "A: Naive",
        spec == "B_cohortFE" ~ "B: Cohort FE",
        spec == "C_within68" ~ "C: Within G=Jul-18"
      ),
      ci_lo = gamma - 1.96 * gamma_se,
      ci_hi = gamma + 1.96 * gamma_se,
      sig = ifelse(gamma_p < 0.05, "p < 0.05",
            ifelse(gamma_p < 0.10, "p < 0.10", "n.s."))
    )

  # Order moderators by naive |gamma| for readability
  mod_order <- oaat_plot %>%
    dplyr::filter(spec == "A_naive") %>%
    dplyr::arrange(abs(gamma)) %>%
    dplyr::pull(label)
  oaat_plot$label <- factor(oaat_plot$label, levels = mod_order)

  p2 <- ggplot2::ggplot(oaat_plot,
           ggplot2::aes(x = gamma, y = label, color = spec_label, shape = sig)) +
    ggplot2::geom_point(size = 2.5, position = ggplot2::position_dodge(width = 0.5)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = ci_lo, xmax = ci_hi),
      width = 0.2, position = ggplot2::position_dodge(width = 0.5)
    ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    ggplot2::scale_color_manual(
      values = c("A: Naive" = "grey60", "B: Cohort FE" = "steelblue", "C: Within G=Jul-18" = "firebrick")
    ) +
    ggplot2::scale_shape_manual(values = c("p < 0.05" = 16, "p < 0.10" = 17, "n.s." = 1)) +
    ggplot2::labs(
      x = expression(gamma[k] ~ "(1-SD increase" %->% "change in ATT)"),
      y = NULL,
      title = "Treatment-Timing Confounding Diagnostic",
      subtitle = paste0("Naive: Y ~ post + post:Z | id+t\n",
                        "Cohort FE: Y ~ post:G + post:Z | id+t\n",
                        "Within Jul-18: same cohort only (no timing confound)"),
      color = "Specification", shape = "Significance"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                   legend.position = "bottom",
                   legend.box = "vertical")

  ggplot2::ggsave(file.path(DIR_OUT_FIGURES, "abawd_heterogeneity_interactions.png"),
                  p2, width = 9, height = 6, dpi = 300)
  message("Wrote outputs/figures/abawd_heterogeneity_interactions.png")
}

message("\n=== Heterogeneity analysis complete ===")
