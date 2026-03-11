# R/02_abawd/04_heterogeneity.R
# Input:  data/derived/panel_analysis.rds
# Output: outputs/tables/abawd_heterogeneity.csv           (ATT by subgroup, 6 dimensions)
#         outputs/tables/abawd_heterogeneity_interactions.csv (continuous interaction coefficients)
#         outputs/figures/abawd_heterogeneity_forest.png     (forest plot)
#
# Heterogeneity analysis for the ABAWD county-vulnerability paper:
# 1) Subgroup CS-DID (median split on pre-period county characteristics)
# 2) Continuous interaction model (fixest: D_ct x Z_c)

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

# ---------------------------------------------------------------------------
# 2) Define split dimensions
# ---------------------------------------------------------------------------
# Each dimension: variable name, high/low labels, split type
split_dims <- list(
  list(var = "avg_unemp",    label = "unemployment",        type = "median"),
  list(var = "avg_y",        label = "baseline_snap",       type = "median"),
  list(var = "avg_poverty",  label = "poverty_rate",        type = "median"),
  list(var = "avg_bachelor", label = "bachelor_share",      type = "median"),
  list(var = "avg_svi",      label = "svi",                 type = "median"),
  list(var = "is_urban",     label = "urbanicity",          type = "binary")
)

# ---------------------------------------------------------------------------
# 3) Subgroup DID function
# ---------------------------------------------------------------------------
run_subgroup <- function(panel_sub, label) {
  if (nrow(panel_sub) < 100) return(list(att = NA_real_, se = NA_real_, n_obs = nrow(panel_sub)))
  df <- build_analysis_df(panel_sub, Y_MAIN, start_date, end_date)
  if (nrow(df) < 50 || !any(df$G_int > 0)) return(list(att = NA_real_, se = NA_real_, n_obs = nrow(df)))
  res <- tryCatch(
    run_cs_did(df, anticipation = 2),
    error = function(e) list(overall = list(overall.att = NA_real_, overall.se = NA_real_))
  )
  list(
    att   = as.numeric(res$overall$overall.att),
    se    = as.numeric(res$overall$overall.se),
    n_obs = nrow(df)
  )
}

# ---------------------------------------------------------------------------
# 4) Run subgroup DID across all dimensions
# ---------------------------------------------------------------------------
het_results <- list()

for (dim in split_dims) {
  var_col <- dim$var
  dim_label <- dim$label

  # Skip if variable not available
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
    # Binary (e.g., urban_dummy: 1 = urban, 0 = rural)
    cutoff <- NA_real_
    chars_valid <- chars_valid %>%
      dplyr::mutate(split_group = ifelse(.data[[var_col]] == 1, "high", "low"))
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
               cutoff = cutoff, stringsAsFactors = FALSE),
    data.frame(subgroup = dim_label, group = low_label,
               att = r_low$att, se = r_low$se, n_obs = r_low$n_obs,
               cutoff = cutoff, stringsAsFactors = FALSE)
  ))
}

het <- dplyr::bind_rows(het_results)
readr::write_csv(het, file.path(DIR_OUT_TABLES, "abawd_heterogeneity.csv"))
message("Wrote outputs/tables/abawd_heterogeneity.csv (", nrow(het), " rows, ",
        length(unique(het$subgroup)), " dimensions)")

# ---------------------------------------------------------------------------
# 5) Continuous interaction model (fixest)
# ---------------------------------------------------------------------------
# Y_ct = a_c + g_t + beta * D_ct + gamma_k * (D_ct x Z_c_k) + e_ct
# gamma_k: how ATT varies with county characteristic Z_c_k

interaction_results <- tryCatch({
  df <- build_analysis_df(panel_raw, Y_MAIN, start_date, end_date)

  # Merge standardized county characteristics
  std_chars <- county_chars %>%
    dplyr::mutate(dplyr::across(
      dplyr::where(is.numeric),
      ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE),
      .names = "z_{.col}"
    )) %>%
    dplyr::select(id, dplyr::starts_with("z_"))

  df <- df %>% dplyr::left_join(std_chars, by = "id")
  df$post <- as.integer(df$G_int > 0 & df$t >= df$G_int)

  # Build formula: post interacted with each z_ variable
  z_vars <- grep("^z_", names(df), value = TRUE)
  z_vars <- z_vars[sapply(z_vars, function(v) sum(!is.na(df[[v]])) > nrow(df) * 0.5)]

  if (length(z_vars) > 0) {
    interaction_terms <- paste0("post:I(", z_vars, ")")
    fml <- as.formula(paste0("Y ~ post + ", paste(interaction_terms, collapse = " + "),
                             " | id_num + t"))
    m <- fixest::feols(fml, data = df, cluster = ~id_num)

    coef_tbl <- broom::tidy(m) %>%
      dplyr::filter(grepl("^post:", term)) %>%
      dplyr::mutate(
        moderator = gsub("post:I\\((.*)\\)", "\\1", term),
        moderator = gsub("^z_", "", moderator)
      ) %>%
      dplyr::select(moderator, estimate, std.error, statistic, p.value)

    readr::write_csv(coef_tbl, file.path(DIR_OUT_TABLES, "abawd_heterogeneity_interactions.csv"))
    message("Wrote outputs/tables/abawd_heterogeneity_interactions.csv (", nrow(coef_tbl), " moderators)")
    coef_tbl
  } else {
    message("No valid z_ moderators for interaction model.")
    NULL
  }
}, error = function(e) {
  message("Interaction model failed: ", conditionMessage(e))
  NULL
})

# ---------------------------------------------------------------------------
# 6) Forest plot of subgroup ATT
# ---------------------------------------------------------------------------
if (nrow(het) > 0 && any(!is.na(het$att))) {
  het_plot <- het %>%
    dplyr::filter(!is.na(att)) %>%
    dplyr::mutate(
      label = paste0(subgroup, ": ", group),
      ci_lo = att - 1.96 * se,
      ci_hi = att + 1.96 * se
    )

  p <- ggplot2::ggplot(het_plot, ggplot2::aes(x = att, y = reorder(label, att))) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = ci_lo, xmax = ci_hi), width = 0.2, orientation = "y") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    ggplot2::labs(
      x = "ATT (log1p SNAP recipients per 1k pop 18-49)",
      y = NULL,
      title = "ABAWD Treatment Effect Heterogeneity",
      subtitle = "Callaway-Sant'Anna ATT by county subgroup"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

  ggplot2::ggsave(file.path(DIR_OUT_FIGURES, "abawd_heterogeneity_forest.png"),
                  p, width = 8, height = 5, dpi = 300)
  message("Wrote outputs/figures/abawd_heterogeneity_forest.png")
}
