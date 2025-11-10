############################################################
# Callaway-Sant'Anna DR-DiD:
# Main Specification + Robustness Checks
#
# Outcome:
#   log(1 + SNAP recipients per 1,000, ages 18–49)
#
# Robustness:
#   (1) Drop very small treatment cohorts
#   (2) Alternative outcome: log(1 + recipients per 100 labor force)
#   (3) Alternative time window
#   (4) Alternative control group ("nevertreated")
#   (5) Drop large/special counties (e.g., Wayne)
#   (6) Simple placebo test (pre-period only)
############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(readr)
  library(did)
  library(ggplot2)
})

# ---------------- 0. Paths & data ----------------
root   <- "/Users/jiamingzhang/Desktop/snap_project"
infile <- file.path(root, "data_clean", "panel_with_G.csv")
stopifnot(file.exists(infile))

panel_raw <- read_csv(infile, show_col_types = FALSE)

############################################################
# Helper functions (all inline to keep single-file workflow)
############################################################

build_analysis_df <- function(Y_COL,
                              start_date = as.Date("2014-01-01"),
                              end_date   = as.Date("2019-12-01")) {
  # Time-varying covariates to include (only)
  COVARS <- c("unemployment_rate", "log_lf")
  
  df <- panel_raw %>%
    mutate(
      date  = as.Date(date),
      t     = year(date) * 12L + month(date),
      id_num = as.integer(factor(id)),
      G_int = ifelse(
        is.na(G) | G == "0",
        0L,
        as.integer(substr(G, 1, 4)) * 12L +
          as.integer(substr(G, 6, 7))
      ),
      Y = .data[[Y_COL]],
      unemployment_rate = as.numeric(unemployment_rate),
      log_lf = log1p(as.numeric(labor_force))
    ) %>%
    filter(
      date >= start_date,
      date <= end_date
    ) %>%
    filter(!is.na(Y)) %>%
    filter(complete.cases(across(all_of(COVARS))))
  
  cat("\n[build] Rows:", nrow(df),
      "| Units:", n_distinct(df$id_num),
      "| Any treated?:", any(df$G_int > 0), "\n")
  
  if (!any(df$G_int > 0)) {
    stop("No treated units (G_int > 0) in this sample. Check G coding or date window.")
  }
  
  # Quick duplicate check (defensive)
  dup <- df %>%
    count(id_num, t) %>%
    filter(n > 1)
  if (nrow(dup) > 0) {
    warning("Duplicate id_num-t rows detected. Consider resolving before estimation.")
  }
  
  df
}

run_cs_did <- function(df,
                       anticipation = 2,
                       control_group = "notyettreated") {
  form_x <- ~ unemployment_rate + log_lf
  
  att <- att_gt(
    yname                  = "Y",
    tname                  = "t",
    idname                 = "id_num",
    gname                  = "G_int",
    xformla                = form_x,
    data                   = df,
    panel                  = TRUE,
    control_group          = control_group,
    est_method             = "dr",
    allow_unbalanced_panel = TRUE,
    anticipation           = anticipation
  )
  
  overall <- aggte(att, type = "simple")
  es      <- aggte(att, type = "dynamic", cband = TRUE)
  
  list(att = att, overall = overall, es = es)
}

event_study_df <- function(es) {
  data.frame(
    event_time = es$egt,
    estimate   = es$att.egt,
    se         = es$se.egt
  ) %>%
    mutate(
      ci_lower = estimate - 1.96 * se,
      ci_upper = estimate + 1.96 * se
    )
}

pretrend_test <- function(es_df, cutoff = -2, label = "") {
  pre <- es_df %>%
    filter(event_time < cutoff, !is.na(estimate), !is.na(se))
  if (nrow(pre) == 0) {
    cat("[pretrend", label, "] No pre-treatment periods for test.\n")
    return(invisible(NULL))
  }
  wald_stat <- sum((pre$estimate / pre$se)^2)
  p_wald <- 1 - pchisq(wald_stat, df = nrow(pre))
  cat("[pretrend", label, "] Wald =", round(wald_stat, 3),
      "| df =", nrow(pre),
      "| p =", round(p_wald, 4), "\n")
  invisible(p_wald)
}

make_es_plot <- function(es_df,
                         title,
                         subtitle,
                         file_out = NULL,
                         color = "#0072B2",
                         min_pre = -24,
                         max_post = 18) {
  es_trim <- es_df %>%
    filter(event_time >= min_pre,
           event_time <= max_post)
  
  p <- ggplot(es_trim, aes(x = event_time, y = estimate)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
                fill = "gray85", alpha = 0.7) +
    geom_line(linewidth = 1.2, color = color) +
    geom_point(size = 2.6, color = color) +
    geom_vline(xintercept = 0, color = "black", linewidth = 0.9) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    scale_x_continuous(
      breaks = seq(min_pre, max_post, by = 6),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    labs(
      title    = title,
      subtitle = subtitle,
      x        = "Months relative to first enforcement (event time)",
      y        = "ATT on log(1 + recipients per 1,000, age 18–49)"
    ) +
    theme_bw(base_size = 14) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 40, hjust = 1),
      plot.title = element_text(face = "bold")
    )
  
  print(p)
  
  if (!is.null(file_out)) {
    ggsave(
      filename = file.path(root, file_out),
      plot     = p,
      width    = 8,
      height   = 5,
      dpi      = 300
    )
    cat("[plot] Saved:", file_out, "\n")
  }
  
  invisible(p)
}

############################################################
# 1. MAIN SPECIFICATION
############################################################

Y_MAIN     <- "y_log1p_per1k_18_49"
START_MAIN <- as.Date("2014-01-01")
END_MAIN   <- as.Date("2019-12-01")

df_main <- build_analysis_df(Y_MAIN, START_MAIN, END_MAIN)

res_main <- run_cs_did(df_main,
                       anticipation = 2,
                       control_group = "notyettreated")

overall_main <- res_main$overall
es_main      <- res_main$es

cat("\n================ MAIN SPEC =================\n")
cat("Outcome:", Y_MAIN, "\n")
cat(sprintf("Overall ATT: %.4f (SE = %.4f)\n",
            overall_main$overall.att, overall_main$overall.se))
cat("============================================\n\n")

es_df_main <- event_study_df(es_main)
pretrend_test(es_df_main, cutoff = -2, label = "main")

make_es_plot(
  es_df_main,
  title    = "Event-Study: ABAWD Enforcement and SNAP Participation",
  subtitle = "Main spec (CS DR-DiD, not-yet-treated control, anticipation = 2)",
  file_out = "fig_es_main_trimmed.png",
  color    = "#0072B2"
)

############################################################
# 2. ROBUSTNESS: Drop small cohorts
############################################################

cohort_sizes <- df_main %>%
  filter(G_int > 0) %>%
  distinct(id_num, G_int) %>%
  count(G_int, name = "n_units") %>%
  arrange(G_int)

cat("\n[RB small cohorts] Cohort sizes:\n")
print(cohort_sizes)

small_G <- cohort_sizes %>%
  filter(n_units <= 4) %>%
  pull(G_int)

cat("[RB small cohorts] Dropping G_int:", paste(small_G, collapse = ", "), "\n")

df_rb_small <- df_main %>%
  filter(!(G_int %in% small_G))

res_rb_small <- run_cs_did(df_rb_small,
                           anticipation = 2,
                           control_group = "notyettreated")

overall_rb_small <- res_rb_small$overall
es_rb_small      <- res_rb_small$es

cat("\n[RB small cohorts] Overall ATT:",
    sprintf("%.4f (SE = %.4f)\n",
            overall_rb_small$overall.att,
            overall_rb_small$overall.se))

es_df_rb_small <- event_study_df(es_rb_small)
pretrend_test(es_df_rb_small, cutoff = -2, label = "rb_small")

make_es_plot(
  es_df_rb_small,
  title    = "Event-Study Robustness: Excluding Small Cohorts",
  subtitle = "Cohorts with <= 4 counties dropped",
  file_out = "fig_es_rb_drop_small.png",
  color    = "#E69F00"
)

############################################################
# 3. ROBUSTNESS: Alternative outcome (per 100 labor force)
############################################################

Y_ALT <- "y_log1p_per100_lf"

if (Y_ALT %in% names(panel_raw)) {
  df_altY <- build_analysis_df(Y_ALT, START_MAIN, END_MAIN)
  res_altY <- run_cs_did(df_altY,
                         anticipation = 2,
                         control_group = "notyettreated")
  overall_altY <- res_altY$overall
  
  cat("\n[RB alt Y] Outcome:", Y_ALT, "\n")
  cat("[RB alt Y] Overall ATT:",
      sprintf("%.4f (SE = %.4f)\n",
              overall_altY$overall.att,
              overall_altY$overall.se))
} else {
  cat("\n[RB alt Y] Skipped: column", Y_ALT, "not found.\n")
}


############################################################
# 4. ROBUSTNESS: Alternative time window (2015–2018)
############################################################

df_win <- build_analysis_df(
  Y_COL      = Y_MAIN,
  start_date = as.Date("2015-01-01"),
  end_date   = as.Date("2018-12-01")
)

res_win <- run_cs_did(df_win,
                      anticipation = 2,
                      control_group = "notyettreated")

overall_win <- res_win$overall

cat("\n[RB window 2015–2018] Overall ATT:",
    sprintf("%.4f (SE = %.4f)\n",
            overall_win$overall.att,
            overall_win$overall.se))

############################################################
# 5. ROBUSTNESS: Alternative control group ("nevertreated")
############################################################

# Only valid if there exist G_int == 0 units that never adopt.
if (any(df_main$G_int == 0)) {
  res_nc <- run_cs_did(df_main,
                       anticipation = 2,
                       control_group = "nevertreated")
  overall_nc <- res_nc$overall
  
  cat("\n[RB nevertreated] Overall ATT:",
      sprintf("%.4f (SE = %.4f)\n",
              overall_nc$overall.att,
              overall_nc$overall.se))
} else {
  cat("\n[RB nevertreated] Skipped: no never-treated units.\n")
}

############################################################
# 6. ROBUSTNESS: Drop large/special counties (e.g., Wayne)
############################################################

# Adjust this vector to match your county identifiers
drop_ids <- c("Wayne")  # if id is FIPS, replace with corresponding id

df_drop_big <- df_main %>%
  filter(!id %in% drop_ids)

if (n_distinct(df_drop_big$id_num) < n_distinct(df_main$id_num)) {
  res_drop_big <- run_cs_did(df_drop_big,
                             anticipation = 2,
                             control_group = "notyettreated")
  overall_drop_big <- res_drop_big$overall
  
  cat("\n[RB drop big] Dropped:",
      paste(drop_ids, collapse = ", "), "\n")
  cat("[RB drop big] Overall ATT:",
      sprintf("%.4f (SE = %.4f)\n",
              overall_drop_big$overall.att,
              overall_drop_big$overall.se))
} else {
  cat("\n[RB drop big] Skipped: no matching ids to drop.\n")
}

############################################################
# 7. ROBUSTNESS: Simple placebo test (pre-policy only)
############################################################

# Use only pre-policy window and assign a fake treatment date.
# This is a sanity check: we should NOT find strong effects.

df_pb <- build_analysis_df(
  Y_COL      = Y_MAIN,
  start_date = as.Date("2014-01-01"),
  end_date   = as.Date("2016-12-01")
)

set.seed(123)

# Example placebo: randomly select some counties as "treated" at fake date 2015-01
fake_G <- as.integer(2015 * 12 + 1)  # 2015-01
treated_ids <- sample(unique(df_pb$id_num), size = min(20, length(unique(df_pb$id_num))))

df_pb_placebo <- df_pb %>%
  mutate(
    G_int = ifelse(id_num %in% treated_ids, fake_G, 0L)
  )

res_pb <- run_cs_did(df_pb_placebo,
                     anticipation = 0,
                     control_group = "notyettreated")

overall_pb <- res_pb$overall

cat("\n[Placebo] Overall ATT (should be near zero):",
    sprintf("%.4f (SE = %.4f)\n",
            overall_pb$overall.att,
            overall_pb$overall.se))

cat("\n=== ALL DONE: Main spec + robustness checks in one script ===\n")

