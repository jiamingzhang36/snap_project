# R/02_abawd/05_stabilizer.R
# Input:  data/derived/panel_analysis.rds
#         outputs/tables/abawd_county_att_hat.csv  (preferred, from 04_heterogeneity.R)
#         outputs/tables/abawd_heterogeneity_interactions.csv (fallback)
# Output: outputs/tables/abawd_stabilizer_correlation.csv
#         outputs/figures/abawd_stabilizer_scatter.png
#
# Automatic stabilizer interpretation:
# 1) Estimate county-level SNAP responsiveness to unemployment (pre-period elasticity)
# 2) Correlate with ABAWD treatment effect heterogeneity
# Interpretive exercise: counties where SNAP acts as a stronger automatic stabilizer
# may be more vulnerable under stricter work requirements.

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)

path_panel <- file.path(DIR_DERIVED, "panel_analysis.rds")
stopifnot(file.exists(path_panel))
if (!dir.exists(DIR_OUT_TABLES))  dir.create(DIR_OUT_TABLES,  recursive = TRUE)
if (!dir.exists(DIR_OUT_FIGURES)) dir.create(DIR_OUT_FIGURES, recursive = TRUE)

panel_raw <- as.data.frame(readRDS(path_panel))
if (!"date" %in% names(panel_raw) && "ym_date" %in% names(panel_raw)) panel_raw$date <- panel_raw$ym_date
panel_raw$date <- as.Date(panel_raw$date)

Y_MAIN <- "y_per1k_18_49"

# ---------------------------------------------------------------------------
# 1) Estimate county-level SNAP-unemployment elasticity (pre-period)
# ---------------------------------------------------------------------------
# Pre-period: before any ABAWD reinstatement (2014-01 to 2016-12)
pre <- panel_raw %>%
  dplyr::filter(date >= as.Date("2014-01-01"), date <= as.Date("2016-12-31")) %>%
  dplyr::mutate(
    y_log = log(as.numeric(.data[[Y_MAIN]])),
    unemp = as.numeric(unemployment_rate)
  ) %>%
  dplyr::filter(!is.na(y_log), !is.na(unemp), is.finite(y_log), is.finite(unemp))

# County-specific OLS: y_log ~ unemp (within county, over time)
county_elasticity <- pre %>%
  dplyr::group_by(id) %>%
  dplyr::filter(dplyr::n() >= 12) %>%
  dplyr::do({
    m <- tryCatch(lm(y_log ~ unemp, data = .), error = function(e) NULL)
    if (!is.null(m)) {
      data.frame(
        beta_unemp = coef(m)["unemp"],
        se_unemp   = summary(m)$coefficients["unemp", "Std. Error"],
        r_squared  = summary(m)$r.squared,
        n_months   = nrow(.),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(beta_unemp = NA_real_, se_unemp = NA_real_,
                 r_squared = NA_real_, n_months = nrow(.),
                 stringsAsFactors = FALSE)
    }
  }) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(beta_unemp))

message("Estimated SNAP-unemployment elasticity for ", nrow(county_elasticity), " counties")

# ---------------------------------------------------------------------------
# 2) Obtain county-level ATT variation
# ---------------------------------------------------------------------------
# Preferred: read county_att_hat.csv (produced by 04_heterogeneity.R)
# Fallback: reconstruct from interaction coefficients

county_att_pred <- NULL

path_att_hat <- file.path(DIR_OUT_TABLES, "abawd_county_att_hat.csv")
het_path <- file.path(DIR_OUT_TABLES, "abawd_heterogeneity_interactions.csv")

if (file.exists(path_att_hat)) {
  att_hat_data <- readr::read_csv(path_att_hat, show_col_types = FALSE)
  if (nrow(att_hat_data) > 0 && any(!is.na(att_hat_data$att_hat))) {
    county_att_pred <- att_hat_data %>%
      dplyr::mutate(
        id = as.character(id),
        att_variation = att_hat - mean(att_hat, na.rm = TRUE)
      ) %>%
      dplyr::select(id, att_variation)
    message("Loaded county ATT hat (", nrow(county_att_pred), " counties)")
  }
}

if (is.null(county_att_pred) && file.exists(het_path)) {
  message("Falling back to interaction coefficients for ATT variation...")
  interaction_coefs <- readr::read_csv(het_path, show_col_types = FALSE)

  # Handle both old (estimate) and new (gamma) column names
  gamma_col <- if ("gamma" %in% names(interaction_coefs)) "gamma" else "estimate"

  # Reconstruct county-level predicted ATT from interaction terms
  pre_chars <- panel_raw %>%
    dplyr::filter(date >= as.Date("2014-01-01"), date <= as.Date("2016-12-31")) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      avg_unemp    = mean(as.numeric(unemployment_rate), na.rm = TRUE),
      avg_y        = mean(as.numeric(.data[[Y_MAIN]]),   na.rm = TRUE),
      avg_poverty  = if ("poverty_rate"   %in% names(.)) mean(as.numeric(poverty_rate),   na.rm = TRUE) else NA_real_,
      avg_bachelor = if ("bachelor_share" %in% names(.)) mean(as.numeric(bachelor_share), na.rm = TRUE) else NA_real_,
      avg_svi      = if ("svi_total"      %in% names(.)) mean(as.numeric(svi_total),      na.rm = TRUE) else NA_real_,
      is_urban     = if ("urban_dummy"    %in% names(.)) as.integer(round(mean(as.numeric(urban_dummy), na.rm = TRUE))) else NA_integer_,
      .groups = "drop"
    )

  # Standardize numeric columns; keep is_urban as binary
  pre_chars_z <- pre_chars %>%
    dplyr::mutate(dplyr::across(
      dplyr::starts_with("avg_"),
      ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE),
      .names = "z_{.col}"
    ))
  if ("is_urban" %in% names(pre_chars_z)) {
    pre_chars_z$z_is_urban <- as.numeric(pre_chars_z$is_urban)
  }

  county_att_pred <- pre_chars_z %>% dplyr::select(id)
  county_att_pred$att_variation <- 0

  for (i in seq_len(nrow(interaction_coefs))) {
    mod_name <- interaction_coefs$moderator[i]
    z_col <- paste0("z_", mod_name)
    if (z_col %in% names(pre_chars_z)) {
      county_att_pred$att_variation <- county_att_pred$att_variation +
        interaction_coefs[[gamma_col]][i] * pre_chars_z[[z_col]]
    } else {
      message("  Stabilizer: z-col '", z_col, "' not found, skipping moderator '", mod_name, "'")
    }
  }
}

if (is.null(county_att_pred)) {
  message("No county ATT variation data available; skipping stabilizer analysis.")
}

# ---------------------------------------------------------------------------
# 3) Correlation: SNAP-unemployment elasticity vs ABAWD vulnerability
# ---------------------------------------------------------------------------
if (!is.null(county_att_pred)) {
  merged <- county_elasticity %>%
    dplyr::inner_join(county_att_pred, by = "id")

  if (nrow(merged) >= 10) {
    cor_test <- cor.test(merged$beta_unemp, merged$att_variation,
                         method = "pearson", use = "complete.obs")

    stabilizer_result <- data.frame(
      metric      = c("pearson_r", "p_value", "n_counties",
                       "mean_elasticity", "sd_elasticity",
                       "mean_att_variation", "sd_att_variation"),
      value       = c(cor_test$estimate, cor_test$p.value, nrow(merged),
                       mean(merged$beta_unemp, na.rm = TRUE),
                       sd(merged$beta_unemp, na.rm = TRUE),
                       mean(merged$att_variation, na.rm = TRUE),
                       sd(merged$att_variation, na.rm = TRUE)),
      stringsAsFactors = FALSE
    )

    readr::write_csv(stabilizer_result,
                     file.path(DIR_OUT_TABLES, "abawd_stabilizer_correlation.csv"))
    message("Stabilizer correlation: r = ", round(cor_test$estimate, 3),
            " (p = ", round(cor_test$p.value, 3), ", n = ", nrow(merged), ")")

    # Scatter plot
    p <- ggplot2::ggplot(merged, ggplot2::aes(x = beta_unemp, y = att_variation)) +
      ggplot2::geom_point(alpha = 0.6, size = 2) +
      ggplot2::geom_smooth(method = "lm", se = TRUE, color = "steelblue", linewidth = 0.8) +
      ggplot2::labs(
        x = "SNAP-Unemployment Elasticity (pre-period)",
        y = "Predicted ABAWD ATT Variation (interaction model)",
        title = "Automatic Stabilizer and ABAWD Vulnerability",
        subtitle = paste0("r = ", round(cor_test$estimate, 3),
                          ", p = ", round(cor_test$p.value, 3),
                          ", n = ", nrow(merged), " counties")
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

    ggplot2::ggsave(file.path(DIR_OUT_FIGURES, "abawd_stabilizer_scatter.png"),
                    p, width = 7, height = 5, dpi = 300)
    message("Wrote outputs/figures/abawd_stabilizer_scatter.png")
  } else {
    message("Too few counties for correlation (", nrow(merged), "); skipping stabilizer analysis.")
  }
} else {
  message("County ATT predictions not available; skipping stabilizer scatter.")
}
