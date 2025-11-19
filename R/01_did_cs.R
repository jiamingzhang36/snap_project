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

build_analysis_df <- function(Y_COL,
                              start_date = as.Date("2014-01-01"),
                              end_date   = as.Date("2019-12-01")) {
  COVARS <- c("unemployment_rate", "log_lf")
  
  df <- panel_raw %>%
    mutate(
      date   = as.Date(date),
      t      = year(date) * 12L + month(date),
      id_num = as.integer(factor(id)),
      G_int = case_when(
        is.na(G) ~ 0L,
        G == "0" ~ 0L,
        TRUE ~ {
          g_chr <- as.character(G)
          yy   <- as.integer(substr(g_chr, 1, 4))
          mm   <- as.integer(substr(g_chr, 6, 7))
          ifelse(is.na(yy) | is.na(mm), 0L, yy * 12L + mm)
        }
      ),
      Y  = if (grepl("log", Y_COL, ignore.case = TRUE)) {
        as.numeric(.data[[Y_COL]])
      } else {
        log1p(as.numeric(.data[[Y_COL]]))
      },
      unemployment_rate = as.numeric(unemployment_rate),
      log_lf            = log1p(as.numeric(labor_force)),
      population_18_49  = as.numeric(population_18_49)
    ) %>%
    filter(date >= start_date, date <= end_date, !is.na(Y)) %>%
    filter(complete.cases(across(all_of(COVARS))))
  
  cat(sprintf("[build] Rows: %d | Units: %d | Treated: %s\n",
              nrow(df), n_distinct(df$id_num), any(df$G_int > 0)))
  
  df
}

run_cs_did <- function(df, anticipation = 2, control_group = "notyettreated") {
  att <- att_gt(
    yname          = "Y",
    tname          = "t",
    idname         = "id_num",
    gname          = "G_int",
    xformla        = ~ unemployment_rate + log_lf,
    data           = df,
    panel          = TRUE,
    control_group  = control_group,
    est_method     = "dr",
    anticipation   = anticipation,
    allow_unbalanced_panel = TRUE
  )
  
  list(
    att     = att,
    overall = aggte(att, type = "simple"),
    es      = aggte(att, type = "dynamic", cband = TRUE)
  )
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
  
  if (nrow(pre) == 0) return(invisible(NA_real_))
  
  wald_stat <- sum((pre$estimate / pre$se)^2)
  p_wald    <- 1 - pchisq(wald_stat, df = nrow(pre))
  
  cat(sprintf("[%s] Pre-trend test: Wald=%.3f (df=%d), p=%.4f\n",
              label, wald_stat, nrow(pre), p_wald))
  invisible(p_wald)
}

make_es_plot <- function(es_df, title, subtitle, file_name = NULL, 
                         color = "#0072B2", min_pre = -24, max_post = 18) {
  es_trim <- es_df %>% filter(event_time >= min_pre, event_time <= max_post)
  
  p <- ggplot(es_trim, aes(x = event_time, y = estimate)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "gray85", alpha = 0.7) +
    geom_line(linewidth = 1.2, color = color) +
    geom_point(size = 2.6, color = color) +
    geom_vline(xintercept = 0, color = "black", linewidth = 0.9) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    scale_x_continuous(breaks = seq(min_pre, max_post, by = 6)) +
    labs(title = title, subtitle = subtitle,
         x = "Months relative to first enforcement",
         y = "ATT on log(1+recipients per 1k, 18–49)") +
    theme_bw(base_size = 14) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 40, hjust = 1),
          plot.title = element_text(face = "bold"))
  
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

Y_MAIN <- "y_log1p_per1k_18_49"
df_main <- build_analysis_df(Y_MAIN)

res_main <- run_cs_did(df_main, anticipation = 2)
overall_main <- res_main$overall
es_main <- res_main$es

cat(sprintf("\nOverall ATT: %.4f (SE = %.4f)\n",
            overall_main$overall.att, overall_main$overall.se))

es_df_main <- event_study_df(es_main)
pretrend_test(es_df_main, label = "Main")

make_es_plot(
  es_df_main,
  title = "Event-Study: ABAWD Enforcement and SNAP Participation",
  subtitle = "CS DR-DiD, not-yet-treated control, anticipation=2",
  file_name = "fig_es_main.png"
)

############################################################
# 2. ROBUSTNESS CHECKS
############################################################

cat("\n╔", strrep("═", 60), "╗\n", sep = "")
cat("║", sprintf("%-60s", " ROBUSTNESS CHECKS"), "║\n", sep = "")
cat("╚", strrep("═", 60), "╝\n\n", sep = "")

results_list <- list()

# Store main result
results_list[["Main (anticipation=2)"]] <- list(
  att = overall_main$overall.att,
  se = overall_main$overall.se
)

### A. Anticipation Sensitivity ###
cat("\n[A] Anticipation window sensitivity...\n")
for (k in c(0, 1, 3, 4, 6)) {
  res <- run_cs_did(df_main, anticipation = k)
  results_list[[paste0("Anticipation=", k)]] <- list(
    att = safe_val(res$overall$overall.att),
    se = safe_val(res$overall$overall.se)
  )
}

### B. Drop Small Cohorts ###
cat("\n[B] Drop small cohorts...\n")
cohort_sizes <- df_main %>%
  filter(G_int > 0) %>%
  distinct(id_num, G_int) %>%
  count(G_int, name = "n_units")

small_G <- cohort_sizes %>% filter(n_units <= 4) %>% pull(G_int)
cat(sprintf("Dropping cohorts: %s\n", paste(small_G, collapse = ", ")))

df_rb_small <- df_main %>% filter(!(G_int %in% small_G))
res_rb_small <- run_cs_did(df_rb_small)
results_list[["Drop small cohorts (≤4)"]] <- list(
  att = safe_val(res_rb_small$overall$overall.att),
  se = safe_val(res_rb_small$overall$overall.se)
)

### C. Drop Large Counties ###
cat("\n[C] Drop large counties...\n")
# Check what county names actually exist
cat("Sample county IDs:\n")
print(head(unique(df_main$id), 10))

drop_ids <- c("Wayne", "Wayne County", "26163")  # Try multiple formats
df_drop_big <- df_main %>% filter(!id %in% drop_ids)

if (nrow(df_drop_big) < nrow(df_main)) {
  cat(sprintf("Dropped %d rows\n", nrow(df_main) - nrow(df_drop_big)))
  res_drop_big <- run_cs_did(df_drop_big)
  results_list[["Drop Wayne County"]] <- list(
    att = safe_val(res_drop_big$overall$overall.att),
    se = safe_val(res_drop_big$overall$overall.se)
  )
} else {
  cat("Wayne County not found in sample\n")
}

### D. Alternative Time Window ###
cat("\n[D] Testing 2015-2018 window...\n")
df_win <- build_analysis_df(Y_MAIN, as.Date("2015-01-01"), as.Date("2018-12-01"))
res_win <- run_cs_did(df_win)
results_list[["Time window 2015-2018"]] <- list(
  att = safe_val(res_win$overall$overall.att),
  se = safe_val(res_win$overall$overall.se)
)

### E. Never-Treated Control ###
if (any(df_main$G_int == 0)) {
  cat("\n[E] Testing never-treated control...\n")
  res_nc <- run_cs_did(df_main, control_group = "nevertreated")
  results_list[["Never-treated control"]] <- list(
    att = safe_val(res_nc$overall$overall.att),
    se = safe_val(res_nc$overall$overall.se)
  )
}

### F. Restricted Pre-Period ###
cat("\n[F] Restricted pre-period (1 year)...\n")
min_treat_time <- df_main %>% filter(G_int > 0) %>% pull(G_int) %>% min()
restricted_start <- min_treat_time - 12
df_restricted <- df_main %>% filter(t >= restricted_start)
cat(sprintf("Sample: %d → %d rows\n", nrow(df_main), nrow(df_restricted)))
res_restricted <- run_cs_did(df_restricted)
results_list[["1-year pre-period only"]] <- list(
  att = safe_val(res_restricted$overall$overall.att),
  se = safe_val(res_restricted$overall$overall.se)
)

### G. Event-Study TWFE ###
cat("\n[G] Event-study with TWFE...\n")
df_es <- df_main %>%
  mutate(
    ever_treated = G_int > 0,
    event_time = t - min_treat_time
  ) %>%
  filter(event_time >= -24, event_time <= 18)

tryCatch({
  es_model <- feols(
    Y ~ i(event_time, ever_treated, ref = c(-1, -2)) + 
      unemployment_rate + log_lf | id_num + t,
    data = df_es,
    cluster = ~ id_num
  )
  
  coefs <- coef(es_model)
  post_coef_names <- grep("event_time::[0-9]+:1$", names(coefs), value = TRUE)
  post_coefs <- coefs[post_coef_names]
  
  if (length(post_coefs) > 0) {
    att_es <- mean(post_coefs, na.rm = TRUE)
    vcv <- vcov(es_model)
    post_ses <- sqrt(diag(vcv)[post_coef_names])
    se_es <- sqrt(mean(post_ses^2, na.rm = TRUE))
    
    results_list[["Event-study TWFE"]] <- list(att = att_es, se = se_es)
    cat(sprintf("Average post ATT: %.4f (SE=%.4f)\n", att_es, se_es))
    
    # Save plot
    png(file.path(outdir, "fig_es_twfe.png"), width = 900, height = 600)
    iplot(es_model, main = "Event-Study: TWFE Dynamic Effects")
    abline(h = 0, lty = 2, col = "gray50")
    abline(v = -0.5, lty = 2, col = "red")
    dev.off()
    cat("[✓] TWFE plot saved\n")
  }
}, error = function(e) cat("[✗] TWFE failed:", conditionMessage(e), "\n"))

### H. Simple DiD ###
cat("\n[H] Simple difference-in-differences...\n")
df_did <- df_main %>%
  mutate(
    ever_treated = G_int > 0,
    post = t >= min_treat_time
  )

did_summary <- df_did %>%
  group_by(ever_treated, post) %>%
  summarise(Y = mean(Y, na.rm = TRUE), .groups = "drop")

if (nrow(did_summary) == 4) {
  treated_pre <- did_summary %>% filter(ever_treated, !post) %>% pull(Y)
  treated_post <- did_summary %>% filter(ever_treated, post) %>% pull(Y)
  control_pre <- did_summary %>% filter(!ever_treated, !post) %>% pull(Y)
  control_post <- did_summary %>% filter(!ever_treated, post) %>% pull(Y)
  
  att_did <- (treated_post - treated_pre) - (control_post - control_pre)
  results_list[["Simple DiD"]] <- list(att = att_did, se = NA_real_)
  cat(sprintf("Simple DiD ATT: %.4f\n", att_did))
}

### I. County-Specific Trends ###
cat("\n[I] County-specific linear trends...\n")
df_trends <- df_main %>%
  mutate(
    ever_treated = G_int > 0,
    post = t >= min_treat_time,
    treat_post = ever_treated * post
  )

tryCatch({
  trend_model <- feols(
    Y ~ treat_post + unemployment_rate + log_lf | id_num + t + id_num[t],
    data = df_trends,
    cluster = ~ id_num
  )
  
  att_trends <- coef(trend_model)["treat_post"]
  se_trends <- sqrt(vcov(trend_model)["treat_post", "treat_post"])
  
  results_list[["With county trends"]] <- list(att = att_trends, se = se_trends)
  cat(sprintf("ATT with trends: %.4f (SE=%.4f)\n", att_trends, se_trends))
  
}, error = function(e) cat("[✗] Trends failed:", conditionMessage(e), "\n"))

### J. Pre-Trend Tests ###
cat("\n[J] Pre-trend diagnostic tests...\n")
df_trend_full <- df_main %>%
  filter(t < min_treat_time) %>%
  mutate(ever_treated = G_int > 0)

if (nrow(df_trend_full) > 100) {
  trend_full <- feols(
    Y ~ ever_treated * t + unemployment_rate + log_lf | id_num,
    data = df_trend_full,
    cluster = ~ id_num
  )
  
  interact_name <- grep("ever_treated.*:t|t:.*ever_treated", 
                        names(coef(trend_full)), value = TRUE)[1]
  
  if (!is.na(interact_name)) {
    trend_coef <- coef(trend_full)[interact_name]
    trend_se <- sqrt(vcov(trend_full)[interact_name, interact_name])
    trend_pval <- 2 * pnorm(-abs(trend_coef / trend_se))
    
    cat(sprintf("Full pre-period diff trend: %.4f (SE=%.4f, p=%.4f) %s\n",
                trend_coef, trend_se, trend_pval,
                ifelse(trend_pval < 0.05, "✗", "✓")))
  }
}

### K. Balance Check ###
cat("\n[K] Covariate balance check...\n")
balance_df <- df_main %>%
  filter(t < min_treat_time) %>%
  mutate(treated = G_int > 0) %>%
  group_by(id_num, treated) %>%
  summarise(
    unemployment_rate = mean(unemployment_rate, na.rm = TRUE),
    log_lf = mean(log_lf, na.rm = TRUE),
    baseline_Y = mean(Y, na.rm = TRUE),
    .groups = "drop"
  )

for (var in c("unemployment_rate", "log_lf", "baseline_Y")) {
  t_test <- t.test(
    balance_df[[var]][balance_df$treated],
    balance_df[[var]][!balance_df$treated]
  )
  
  cat(sprintf("%-20s: Treated=%.3f, Control=%.3f, p=%.3f %s\n",
              var,
              mean(balance_df[[var]][balance_df$treated], na.rm = TRUE),
              mean(balance_df[[var]][!balance_df$treated], na.rm = TRUE),
              t_test$p.value,
              ifelse(t_test$p.value < 0.05, "✗", "✓")))
}

### L. Placebo Test ###
cat("\n[L] Placebo test (pre-period only)...\n")
df_pb <- build_analysis_df(Y_MAIN, as.Date("2014-01-01"), as.Date("2016-12-01"))
fake_G <- as.integer(2015 * 12 + 1)
treated_ids <- sample(unique(df_pb$id_num), size = min(20, length(unique(df_pb$id_num))))
df_pb_placebo <- df_pb %>% mutate(G_int = ifelse(id_num %in% treated_ids, fake_G, 0L))
res_pb <- run_cs_did(df_pb_placebo, anticipation = 0)
results_list[["Placebo (pre-period)"]] <- list(
  att = safe_val(res_pb$overall$overall.att),
  se = safe_val(res_pb$overall$overall.se)
)

############################################################
# 3. RESULTS TABLE
############################################################

cat("\n╔", strrep("═", 60), "╗\n", sep = "")
cat("║", sprintf("%-60s", " COMPREHENSIVE RESULTS"), "║\n", sep = "")
cat("╚", strrep("═", 60), "╝\n\n", sep = "")

results_df <- bind_rows(lapply(names(results_list), function(spec) {
  data.frame(
    Specification = spec,
    ATT = results_list[[spec]]$att,
    SE = results_list[[spec]]$se
  )
})) %>%
  mutate(
    CI_lower = ATT - 1.96 * SE,
    CI_upper = ATT + 1.96 * SE,
    t_stat = abs(ATT / SE),
    p_value = 2 * (1 - pnorm(t_stat)),
    Sig = case_when(
      is.na(p_value) ~ "",
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      p_value < 0.10 ~ "†",
      TRUE ~ ""
    )
  )

print(kable(results_df, digits = 4, format = "simple"))

write_csv(results_df, file.path(outdir, "robustness_complete.csv"))
cat("\n[✓] Results saved: robustness_complete.csv\n")

### Summary Statistics ###
cat("\n", strrep("-", 60), "\n", sep = "")
cat("SUMMARY\n")
cat(strrep("-", 60), "\n")

non_placebo <- results_df %>% filter(Specification != "Placebo (pre-period)")
cat(sprintf("Mean ATT:     %.4f\n", mean(non_placebo$ATT, na.rm = TRUE)))
cat(sprintf("Median ATT:   %.4f\n", median(non_placebo$ATT, na.rm = TRUE)))
cat(sprintf("Range:        [%.4f, %.4f]\n", 
            min(non_placebo$ATT, na.rm = TRUE),
            max(non_placebo$ATT, na.rm = TRUE)))
cat(sprintf("Significant (p<0.05): %d/%d (%.0f%%)\n",
            sum(non_placebo$p_value < 0.05, na.rm = TRUE),
            nrow(non_placebo),
            100 * mean(non_placebo$p_value < 0.05, na.rm = TRUE)))

############################################################
# 4. FOREST PLOT
############################################################

cat("\n[✓] Generating forest plot...\n")

plot_data <- results_df %>%
  filter(Specification != "Placebo (pre-period)", !is.na(ATT)) %>%
  mutate(
    spec_num = row_number(),
    is_main = Specification == "Main (anticipation=2)"
  )

p_forest <- ggplot(plot_data, aes(x = ATT, y = reorder(Specification, -spec_num))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  geom_errorbar(aes(xmin = CI_lower, xmax = CI_upper), 
                orientation = "y", width = 0.4, color = "gray50", linewidth = 0.6) +
  geom_point(aes(color = is_main, size = is_main)) +
  scale_color_manual(values = c("FALSE" = "#56B4E9", "TRUE" = "#D55E00")) +
  scale_size_manual(values = c("FALSE" = 3, "TRUE" = 4.5)) +
  labs(
    title = "Forest Plot: Robustness Checks",
    subtitle = "Effect of ABAWD enforcement on log SNAP participation",
    x = "Average Treatment Effect on Treated (ATT)",
    y = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(outdir, "fig_forest_plot.png"), p_forest, width = 10, height = 8, dpi = 300)
cat("[✓] Forest plot saved: fig_forest_plot.png\n")

cat("\n╔", strrep("═", 60), "╗\n", sep = "")
cat("║", sprintf("%-60s", " ANALYSIS COMPLETE"), "║\n", sep = "")
cat("╚", strrep("═", 60), "╝\n\n", sep = "")