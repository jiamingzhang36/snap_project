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
# 2. ROBUSTNESS CHECKS (FINAL & CLEAN)
############################################################

cat("\n╔", strrep("═", 60), "╗\n", sep = "")
cat("║", sprintf("%-60s", " 2. ROBUSTNESS CHECKS"), "║\n", sep = "")
cat("╚", strrep("═", 60), "╝\n\n", sep = "")

results_list <- list()

# Store Baseline Result (from Main Specification)
results_list[["Main (CS, Ant=2)"]] <- list(
  att = overall_main$overall.att, 
  se  = overall_main$overall.se
)

# =========================================================
# [A] Anticipation Sensitivity Scan (0 to 6 months)
#     Goal: Validate that the effect stabilizes at Ant=2
# =========================================================
cat("\n[A] Anticipation Sensitivity Scan (0-6 months)...\n")

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
# [B] Sensitivity: Control Group Selection
#     Goal: Ensure results don't depend on control definition
# =========================================================
cat("\n[B] Sensitivity: Never-treated Control...\n")

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
# [C] Sensitivity: Outliers (Drop Wayne County)
#     Goal: Ensure results aren't driven by one large county
# =========================================================
cat("\n[C] Sensitivity: Excluding Wayne County (Outlier)...\n")

drop_ids <- c("Wayne", "Wayne County", "26163") 
df_drop <- df_main %>% filter(!id %in% drop_ids)

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
# [D] Method: Sun & Abraham (2021)
#     Goal: Robustness to alternative staggered estimator
# =========================================================
cat("\n[D] Method: Sun & Abraham (2021) with Anticipation...\n")

# Prepare data: sunab needs numeric G. Assign 10000 to never-treated.
df_sa <- df_main %>%
  mutate(G_sa = ifelse(G_int == 0, 10000, G_int))

tryCatch({
  # CRITICAL: Set ref.p = -3 to allow estimation of t=-2 and t=-1
  # This aligns the SA model with the anticipation hypothesis.
  sa_model <- feols(
    Y ~ sunab(G_sa, t, ref.p = -3) + unemployment_rate + log_lf | id_num + t,
    data = df_sa,
    cluster = ~ id_num
  )
  
  # Aggregate Post-treatment effects
  sa_agg <- summary(sa_model, agg = "att")
  
  results_list[["Sun & Abraham (2021)"]] <- list(
    att = sa_agg$coeftable[1, 1],
    se  = sa_agg$coeftable[1, 2]
  )
  
  # Plot SA to visualize the anticipation drop
  png(file.path(outdir, "fig_es_sunab_ant.png"), width = 900, height = 600)
  iplot(sa_model, main = "Sun & Abraham (Ref = -3): Evidence of Anticipation",
        xlab = "Months relative to enforcement")
  dev.off()
  
}, error = function(e) cat("[!] SA Failed:", conditionMessage(e), "\n"))


# =========================================================
# [E] Method: Static TWFE (Naive Benchmark)
#     Goal: Show bias of traditional method vs CS
# =========================================================
cat("\n[E] Method: Static TWFE (Biased Benchmark)...\n")

# Treatment indicator: 1 if treated AND post-date
df_twfe <- df_main %>%
  mutate(treat_post = ifelse(G_int > 0 & t >= G_int, 1, 0))

tryCatch({
  twfe_model <- feols(
    Y ~ treat_post + unemployment_rate + log_lf | id_num + t,
    data = df_twfe, cluster = ~ id_num
  )
  
  results_list[["Static TWFE (Biased)"]] <- list(
    att = coef(twfe_model)["treat_post"],
    se  = se(twfe_model)["treat_post"]
  )
}, error = function(e) cat("[!] TWFE Failed:", conditionMessage(e), "\n"))


# =========================================================
# [F] Functional Form: Participation Rate (Level)
#     Goal: Ensure results hold for Rate, not just Log
# =========================================================
cat("\n[F] Functional Form: Participation Rate (Level)...\n")

# Reverse log(1+x) -> exp(x)-1 to get raw recipients, then calc rate per 1k
df_rate <- df_main %>%
  mutate(
    recipients_est = exp(Y) - 1,
    pop_safe       = ifelse(population_18_49 < 1, 1, population_18_49),
    Y_rate         = (recipients_est / pop_safe) * 1000
  ) %>%
  mutate(Y = Y_rate) %>% # Swap outcome variable
  filter(is.finite(Y))

tryCatch({
  res_rate <- run_cs_did(df_rate, anticipation = 2)
  
  cat(sprintf("    Rate ATT: %.4f (SE=%.4f)\n", 
              res_rate$overall$overall.att, res_rate$overall$overall.se))
  
  # Note: We do NOT add this to results_list for the Forest Plot 
  # because the y-axis scale is completely different (Level vs Log).
}, error = function(e) cat("[!] Rate Check Failed:", conditionMessage(e), "\n"))


############################################################
# 3. RESULTS TABLE & EXPORT
############################################################

cat("\n╔", strrep("═", 60), "╗\n", sep = "")
cat("║", sprintf("%-60s", " 3. FINAL RESULTS TABLE"), "║\n", sep = "")
cat("╚", strrep("═", 60), "╝\n\n", sep = "")

results_df <- bind_rows(lapply(names(results_list), function(spec) {
  data.frame(
    Specification = spec,
    ATT = results_list[[spec]]$att,
    SE  = results_list[[spec]]$se
  )
})) %>%
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

cat("\n╔", strrep("═", 60), "╗\n", sep = "")
cat("║", sprintf("%-60s", " ANALYSIS COMPLETE"), "║\n", sep = "")
cat("╚", strrep("═", 60), "╝\n\n", sep = "")

