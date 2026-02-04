############################################################
# DiD Analysis: Early ABAWD Enforcement Effect on SNAP
#
# IDENTIFICATION STRATEGY:
# - Sample trimmed to 2018-06 (before 68-county expansion)
# - Treated cohorts: 2017-01 (4 counties), 2018-01 (10 counties)
# - Controls: 69 counties not yet treated through June 2018
#   (including 68 that would be treated in July + Wayne in Oct)
# - This avoids control exhaustion and monotone-adoption issues
#
# Main estimand: Post 1-5 month average ATT (anticipation = 2)
# (2018-01 cohort can contribute through June 2018 = 5 post months)
############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(readr)
  library(did)
  library(fixest)
  library(ggplot2)
  library(tidyr)
})

SEED <- 123
set.seed(SEED)
# Set to FALSE to inspect warnings
SUPPRESS_WARNINGS <- TRUE
# Bootstrap iterations for CS inference
BOOT_ITERS_FAST <- 199
BOOT_ITERS_FINAL <- 999
BOOT_ITERS <- BOOT_ITERS_FAST  # switch to BOOT_ITERS_FINAL for final tables

maybe_suppress <- function(expr) {
  if (SUPPRESS_WARNINGS) {
    suppressWarnings(eval(substitute(expr), parent.frame()))
  } else {
    eval(substitute(expr), parent.frame())
  }
}

# Paths
datadir <- "data_clean"
outdir  <- "outputs/step1_did"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# Load data
panel_raw <- read_csv(file.path(datadir, "panel_with_G.csv"), show_col_types = FALSE)

############################################################
# HELPER FUNCTIONS
############################################################

# Build analysis dataframe with trimming
build_df <- function(data, y_col, start_date = as.Date("2014-01-01"), 

                     end_date = as.Date("2018-06-01")) {
  
  df <- data %>%
    mutate(
      date = as.Date(paste0(year, "-", sprintf("%02d", month), "-01")),
      t = year * 12L + (month - 1L),
      id_num = as.integer(factor(county)),
      Y = .data[[y_col]]
    ) %>%
    filter(date >= start_date, date <= end_date, !is.na(Y))
  
  # Use G_int from file; set to 0 if missing
  if ("G_int" %in% names(df)) {
    df <- df %>% mutate(G_int = as.integer(ifelse(is.na(G_int), 0L, G_int)))
  } else {
    df <- df %>% mutate(G_int = 0L)
  }
  
  # Recode late-treated as never-treated within trimmed sample
  t_max <- max(df$t)
  df <- df %>%
    mutate(G_trim = ifelse(G_int > t_max | G_int == 0, 0L, G_int))
  
  cat(sprintf("[build] Sample: %s to %s (%d rows)\n", 
              min(df$date), max(df$date), nrow(df)))
  cat(sprintf("[build] Treated cohorts (G_trim > 0): %s\n",
              paste(sort(unique(df$G_trim[df$G_trim > 0])), collapse = ", ")))
  cat(sprintf("[build] Counties as controls (G_trim = 0): %d\n",
              n_distinct(df$county[df$G_trim == 0])))
  
  df
}

# Post-window mean ATT (cell average)
post_window_att_only <- function(att_obj, e_min = 1, e_max = 5) {
  att_raw <- data.frame(idx = seq_along(att_obj$att),
                        g = att_obj$group, t = att_obj$t, att = att_obj$att)
  att_raw$e <- att_raw$t - att_raw$g
  keep <- which(att_raw$e >= e_min & att_raw$e <= e_max & !is.na(att_raw$att))
  if (length(keep) == 0) return(list(att = NA_real_, n_cells = 0L, idx = integer(0)))
  list(att = mean(att_raw$att[keep], na.rm = TRUE), n_cells = length(keep), idx = keep)
}

# Post-window mean ATT (cohort-weighted by treated counties)
post_window_att_weighted_only <- function(att_obj, df, gname = "G_trim", e_min = 1, e_max = 5) {
  att_raw <- data.frame(idx = seq_along(att_obj$att),
                        g = att_obj$group, t = att_obj$t, att = att_obj$att)
  att_raw$e <- att_raw$t - att_raw$g
  post <- att_raw %>% filter(e >= e_min, e <= e_max, !is.na(att))
  if (nrow(post) == 0) return(list(att = NA_real_, n_cells = 0L))
  
  g_sizes <- df %>%
    filter(.data[[gname]] > 0) %>%
    distinct(county, .data[[gname]]) %>%
    count(.data[[gname]], name = "n_treated")
  
  post <- post %>%
    group_by(g) %>%
    mutate(n_cells_g = n()) %>%
    ungroup() %>%
    left_join(g_sizes, by = c("g" = gname)) %>%
    filter(!is.na(n_treated))
  
  if (nrow(post) == 0) return(list(att = NA_real_, n_cells = 0L))
  
  w_g <- post %>% distinct(g, n_treated, n_cells_g) %>%
    mutate(w_g = n_treated / sum(n_treated))
  post <- post %>% left_join(w_g %>% select(g, w_g), by = "g") %>%
    mutate(w = w_g / n_cells_g)
  
  list(att = sum(post$w * post$att), n_cells = nrow(post))
}

# County cluster bootstrap for post-window mean ATT
boot_post_window <- function(df, gname = "G_trim", anticipation = 2L,
                             xformla = ~ unemployment_rate,
                             est_method = "ipw",
                             e_min = 1, e_max = 5,
                             weight = c("cell", "cohort"),
                             B = 499, seed = 123) {
  if (!is.null(seed)) set.seed(seed)
  weight <- match.arg(weight)
  boot_att <- rep(NA_real_, B)
  
  for (b in seq_len(B)) {
    # Stratified county bootstrap by cohort (keeps each cohort present)
    g_vals <- sort(unique(df[[gname]]))
    boot_counties <- unlist(lapply(g_vals, function(gv) {
      ids_g <- unique(df$county[df[[gname]] == gv])
      if (length(ids_g) == 0) return(character(0))
      sample(ids_g, size = length(ids_g), replace = TRUE)
    }))
    df_b <- bind_rows(lapply(seq_along(boot_counties), function(i) {
      df %>% filter(county == boot_counties[i]) %>% mutate(id_num = i)
    }))
    
    att_b <- tryCatch({
      maybe_suppress(
        did::att_gt(
          yname = "Y", tname = "t", idname = "id_num", gname = gname,
          data = df_b, xformla = xformla, control_group = "notyettreated",
          anticipation = anticipation, est_method = est_method,
          allow_unbalanced_panel = TRUE, bstrap = FALSE
        )
      )
    }, error = function(e) NULL)
    
    if (!is.null(att_b)) {
      if (weight == "cell") {
        boot_att[b] <- post_window_att_only(att_b, e_min, e_max)$att
      } else {
        boot_att[b] <- post_window_att_weighted_only(att_b, df_b, gname, e_min, e_max)$att
      }
    }
  }
  
  boot_att <- boot_att[is.finite(boot_att)]
  se <- sd(boot_att)
  ci <- stats::quantile(boot_att, probs = c(0.025, 0.975), na.rm = TRUE)
  list(se = se, ci_lo = ci[[1]], ci_hi = ci[[2]], n_eff = length(boot_att))
}

# Run CS (point estimates; bootstrap handled externally)
run_cs <- function(df, gname = "G_trim", anticipation = 2L, 
                   xformla = ~ unemployment_rate, weightsname = NULL,
                   est_method = "ipw", bstrap = FALSE, biters = 0) {
  att <- tryCatch({
    maybe_suppress(did::att_gt(
      yname = "Y", tname = "t", idname = "id_num", gname = gname,
      data = df,
      xformla = xformla, control_group = "notyettreated",
      anticipation = anticipation, est_method = est_method,
      allow_unbalanced_panel = TRUE, weightsname = weightsname,
      bstrap = bstrap, biters = biters, clustervars = "id_num"
    ))
  }, error = function(e) {
    if (grepl("singular", e$message, ignore.case = TRUE) && est_method == "dr") {
      cat("[!] DR failed (singular), trying IPW...\n")
      maybe_suppress(did::att_gt(
        yname = "Y", tname = "t", idname = "id_num", gname = gname,
        data = df,
        xformla = xformla, control_group = "notyettreated",
        anticipation = anticipation, est_method = "ipw",
        allow_unbalanced_panel = TRUE, weightsname = weightsname,
      bstrap = bstrap, biters = biters, clustervars = "id_num"
      ))
    } else if (grepl("singular", e$message, ignore.case = TRUE)) {
      cat("[!] IPW failed, trying no covariates...\n")
      maybe_suppress(did::att_gt(
        yname = "Y", tname = "t", idname = "id_num", gname = gname,
        data = df,
        xformla = NULL, control_group = "notyettreated",
        anticipation = anticipation, est_method = "ipw",
        allow_unbalanced_panel = TRUE, weightsname = weightsname,
      bstrap = bstrap, biters = biters, clustervars = "id_num"
      ))
    } else {
      stop(e)
    }
  })
  att
}

############################################################
# 1. MAIN SPECIFICATION: CS IPW-DiD (trimmed to 2018-06)
############################################################

cat("\n", strrep("=", 60), "\n")
cat(" MAIN: CS IPW-DiD (Early Cohorts, Trim to 2018-06)\n")
cat(strrep("=", 60), "\n\n")

# Build main dataframe (trimmed)
# Use log1p outcome for percentage interpretation (recommended)
df_main <- build_df(panel_raw, y_col = "y_log1p_per1k_18_49", 
                    end_date = as.Date("2018-06-01"))

# Verify Y scale
cat(sprintf("[Y check] min=%.2f, median=%.2f, max=%.2f\n",
            min(df_main$Y, na.rm=TRUE), median(df_main$Y, na.rm=TRUE), 
            max(df_main$Y, na.rm=TRUE)))

# Main CS estimation (IPW with bootstrap inference)
ANT_USE <- 2L
att_cs <- run_cs(df_main, gname = "G_trim", anticipation = ANT_USE,
                 est_method = "ipw", bstrap = FALSE)

# Overall ATT (point estimate only; inference via external bootstrap)
overall_cs <- aggte(att_cs, type = "simple")
cat(sprintf("CS Overall ATT (point estimate): %.4f\n", overall_cs$overall.att))

# VCOV sanity check (bootstrap alignment)
# DIAGNOSTIC: Check att_gt raw output
cat("\n[DIAGNOSTIC] att_gt raw (g, t, att):\n")
att_raw <- data.frame(g = att_cs$group, t = att_cs$t, att = att_cs$att)
att_raw$e <- att_raw$t - att_raw$g
cat(sprintf("  Unique event times in att_gt: %s\n", 
            paste(sort(unique(att_raw$e)), collapse = ", ")))
cat(sprintf("  Post-treatment cells (e >= 0): %d\n", sum(att_raw$e >= 0)))
cat(sprintf("  e >= 1 cells: %d\n", sum(att_raw$e >= 1)))

# Show some post-treatment cells if they exist
post_cells <- att_raw %>% filter(e >= 1) %>% head(10)
if (nrow(post_cells) > 0) {
  cat("  First 10 post-treatment cells:\n")
  print(post_cells)
} else {
  cat("  ⚠️ NO POST-TREATMENT CELLS (e >= 1) FOUND!\n")
  cat("  This explains why aggte(dynamic) lacks post horizons.\n")
}

# Event study (diagnostic)
es_cs <- aggte(att_cs, type = "dynamic", na.rm = TRUE, min_e = -6, max_e = 6, 
               cband = FALSE, balance_e = 0)
es_diag <- data.frame(e = es_cs$egt, att = es_cs$att.egt, se = es_cs$se.egt)
cat("\n[DIAGNOSTIC] aggte dynamic coefficients:\n")
print(es_diag)

# Main estimand: cohort-weighted post 1-5 / 1-3 + bootstrap inference
cs_1_5_cell <- post_window_att_only(att_cs, e_min = 1, e_max = 5)
cs_1_3_cell <- post_window_att_only(att_cs, e_min = 1, e_max = 3)
cs_1_5_w <- post_window_att_weighted_only(att_cs, df_main, e_min = 1, e_max = 5)
cs_1_3_w <- post_window_att_weighted_only(att_cs, df_main, e_min = 1, e_max = 3)

boot_cs_1_5_w <- boot_post_window(df_main, anticipation = ANT_USE, e_min = 1, e_max = 5,
                                  est_method = "ipw", weight = "cohort",
                                  B = BOOT_ITERS, seed = SEED + 2)
boot_cs_1_3_w <- boot_post_window(df_main, anticipation = ANT_USE, e_min = 1, e_max = 3,
                                  est_method = "ipw", weight = "cohort",
                                  B = BOOT_ITERS, seed = SEED + 3)

boot_cs_1_5_cell <- boot_post_window(df_main, anticipation = ANT_USE, e_min = 1, e_max = 5,
                                     est_method = "ipw", weight = "cell",
                                     B = BOOT_ITERS, seed = SEED)
boot_cs_1_3_cell <- boot_post_window(df_main, anticipation = ANT_USE, e_min = 1, e_max = 3,
                                     est_method = "ipw", weight = "cell",
                                     B = BOOT_ITERS, seed = SEED + 1)

cat(sprintf("CS Post 1-5 avg (MAIN, cohort-weighted): %.4f (boot SE = %.4f, n_eff=%d)\n",
            cs_1_5_w$att, boot_cs_1_5_w$se, boot_cs_1_5_w$n_eff))
cat(sprintf("CS Post 1-3 avg (cohort-weighted): %.4f (boot SE = %.4f, n_eff=%d)\n",
            cs_1_3_w$att, boot_cs_1_3_w$se, boot_cs_1_3_w$n_eff))
cat(sprintf("CS Post 1-5 avg (cell-average): %.4f (boot SE = %.4f, n_eff=%d)\n",
            cs_1_5_cell$att, boot_cs_1_5_cell$se, boot_cs_1_5_cell$n_eff))
cat(sprintf("CS Post 1-3 avg (cell-average): %.4f (boot SE = %.4f, n_eff=%d)\n",
            cs_1_3_cell$att, boot_cs_1_3_cell$se, boot_cs_1_3_cell$n_eff))

# Results storage
results_list <- list()
results_list[["Main: CS (post 1-5, cohort-weighted)"]] <- list(
  att = cs_1_5_w$att, se = boot_cs_1_5_w$se,
  ci_lower = boot_cs_1_5_w$ci_lo, ci_upper = boot_cs_1_5_w$ci_hi, is_boot = TRUE
)
results_list[["Main: CS (post 1-3, cohort-weighted)"]] <- list(
  att = cs_1_3_w$att, se = boot_cs_1_3_w$se,
  ci_lower = boot_cs_1_3_w$ci_lo, ci_upper = boot_cs_1_3_w$ci_hi, is_boot = TRUE
)
results_list[["Robust: CS (post 1-5, cell-avg)"]] <- list(
  att = cs_1_5_cell$att, se = boot_cs_1_5_cell$se,
  ci_lower = boot_cs_1_5_cell$ci_lo, ci_upper = boot_cs_1_5_cell$ci_hi, is_boot = TRUE
)
results_list[["Robust: CS (post 1-3, cell-avg)"]] <- list(
  att = cs_1_3_cell$att, se = boot_cs_1_3_cell$se,
  ci_lower = boot_cs_1_3_cell$ci_lo, ci_upper = boot_cs_1_3_cell$ci_hi, is_boot = TRUE
)

############################################################
# 2. EVENT STUDY PLOT
############################################################

g_sizes <- df_main %>%
  filter(G_trim > 0) %>%
  distinct(county, G_trim) %>%
  count(G_trim, name = "n_g")

es_pts <- att_raw %>%
  left_join(g_sizes, by = c("g" = "G_trim")) %>%
  filter(e >= -6, e <= 6) %>%
  group_by(e) %>%
  summarise(att = weighted.mean(att, w = n_g, na.rm = TRUE), .groups = "drop")

p_es <- ggplot(es_pts, aes(x = e, y = att)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "red") +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  labs(
    title = "Event Study: Early ABAWD Enforcement Effect",
    subtitle = "Cohort-weighted points from att_gt (no CI)",
    x = "Months relative to enforcement",
    y = "ATT (log1p SNAP per 1k pop 18-49)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(outdir, "fig_es_main.png"), p_es, width = 8, height = 5, dpi = 150)
cat("[+] Event study plot saved.\n")

############################################################
# 3. ALTERNATIVE: Stacked Event Study (K=5)
############################################################

cat("\n", strrep("=", 60), "\n")
cat(" ALT: Stacked Event Study (K=5)\n")
cat(strrep("=", 60), "\n\n")

build_stacked <- function(df, gname = "G_trim", L = 6, K = 5) {
  treated_g <- sort(unique(df[[gname]][df[[gname]] > 0]))
  
  out <- lapply(treated_g, function(g) {
    win <- df %>% filter(t >= g - L, t <= g + K)
    controls <- win %>% 
      filter(.data[[gname]] == 0 | .data[[gname]] > g + K) %>%
      mutate(treat = 0L)
    treated <- win %>% 
      filter(.data[[gname]] == g) %>%
      mutate(treat = 1L)
    bind_rows(controls, treated) %>%
      mutate(
        stack_g = g,
        rel = t - g,
        unit_stack = paste0(id_num, "_", g),
        time_stack = paste0(t, "_", g)
      )
  })
  bind_rows(out)
}

stk <- build_stacked(df_main)
cat(sprintf("[Stacked] %d rows, %d stacks\n", nrow(stk), n_distinct(stk$stack_g)))

# Stacked regression
stk_mod <- feols(Y ~ i(rel, treat, ref = -1) | unit_stack + time_stack,
                 data = stk, cluster = ~county)

# Extract coefficients
stk_coef <- coef(stk_mod)
stk_se   <- sqrt(diag(vcov(stk_mod)))
stk_names <- names(stk_coef)

stk_df <- data.frame(
  term = stk_names,
  estimate = stk_coef,
  se = stk_se,
  stringsAsFactors = FALSE
) %>%
  filter(grepl("rel::", term)) %>%
  mutate(
    rel = as.integer(sub("rel::(-?\\d+):treat", "\\1", term))
  ) %>%
  filter(!is.na(rel)) %>%
  arrange(rel)

# Post 1-5 average (use vcov for linear combination)
post_stk <- stk_df %>% filter(rel >= 1, rel <= 5)
if (nrow(post_stk) > 0) {
  terms_15 <- post_stk$term
  b_15 <- stk_coef[terms_15]
  V_15 <- vcov(stk_mod)[terms_15, terms_15, drop = FALSE]
  w_15 <- rep(1 / length(terms_15), length(terms_15))
  att_stk_1_5 <- sum(w_15 * b_15)
  se_stk_1_5  <- sqrt(as.numeric(t(w_15) %*% V_15 %*% w_15))
  cat(sprintf("Stacked Post 1-5 avg: %.4f (SE = %.4f)\n", att_stk_1_5, se_stk_1_5))
  results_list[["Alt: Stacked (post 1-5)"]] <- list(att = att_stk_1_5, se = se_stk_1_5)
}

# Post 1-3
post_stk_13 <- stk_df %>% filter(rel >= 1, rel <= 3)
if (nrow(post_stk_13) > 0) {
  terms_13 <- post_stk_13$term
  b_13 <- stk_coef[terms_13]
  V_13 <- vcov(stk_mod)[terms_13, terms_13, drop = FALSE]
  w_13 <- rep(1 / length(terms_13), length(terms_13))
  att_stk_1_3 <- sum(w_13 * b_13)
  se_stk_1_3  <- sqrt(as.numeric(t(w_13) %*% V_13 %*% w_13))
  cat(sprintf("Stacked Post 1-3 avg: %.4f (SE = %.4f)\n", att_stk_1_3, se_stk_1_3))
  results_list[["Alt: Stacked (post 1-3)"]] <- list(att = att_stk_1_3, se = se_stk_1_3)
}

############################################################
# 4. ALTERNATIVE: Sun-Abraham (comparison only)
############################################################

cat("\n", strrep("=", 60), "\n")
cat(" ALT: Sun-Abraham (comparison)\n")
cat(strrep("=", 60), "\n\n")

df_sa <- df_main %>% filter(G_trim > 0 | G_trim == 0)
df_sa$cohort <- df_sa$G_trim

sa_mod <- tryCatch({
  feols(Y ~ sunab(cohort, t, ref.p = c(-6, -3)) | id_num + t,
        data = df_sa, cluster = ~county)
}, error = function(e) {
  cat("[!] Sun-Abraham failed:", e$message, "\n")
  NULL
})

if (!is.null(sa_mod)) {
  cat("Sun-Abraham model fit.\n")
  
  # Extract coefficients directly from model
  sa_coef <- tryCatch({
    cf <- coef(sa_mod)
    se <- sqrt(diag(vcov(sa_mod)))
    data.frame(
      term = names(cf),
      estimate = as.numeric(cf),
      se = as.numeric(se),
      stringsAsFactors = FALSE
    )
  }, error = function(e) NULL)
  
  if (!is.null(sa_coef) && nrow(sa_coef) > 0) {
    # Parse event time from term names (format: "t::X:cohort::Y")
    sa_coef$event_time <- as.integer(gsub(".*t::(-?\\d+):.*", "\\1", sa_coef$term))
    sa_df <- sa_coef %>% filter(!is.na(event_time))
    
    if (nrow(sa_df) > 0) {
      cat(sprintf("  SA event times found: %s\n", 
                  paste(sort(unique(sa_df$event_time)), collapse = ", ")))
      
      post_sa <- sa_df %>% filter(event_time >= 1, event_time <= 5)
      if (nrow(post_sa) > 0) {
        terms_sa <- post_sa$term
        b_sa <- coef(sa_mod)[terms_sa]
        V_sa <- vcov(sa_mod)[terms_sa, terms_sa, drop = FALSE]
        w_sa <- rep(1 / length(terms_sa), length(terms_sa))
        att_sa_1_5 <- sum(w_sa * b_sa)
        se_sa_1_5  <- sqrt(as.numeric(t(w_sa) %*% V_sa %*% w_sa))
        cat(sprintf("Sun-Abraham Post 1-5 avg: %.4f (SE = %.4f)\n", 
                    att_sa_1_5, se_sa_1_5))
        results_list[["Alt: Sun-Abraham (post 1-5)"]] <- list(att = att_sa_1_5, se = se_sa_1_5)
      } else {
        cat("  No post-treatment (e=1-5) coefficients found.\n")
      }
    }
  }
}

############################################################
# 5. ROBUSTNESS CHECKS
############################################################

cat("\n", strrep("=", 60), "\n")
cat(" ROBUSTNESS CHECKS\n")
cat(strrep("=", 60), "\n\n")

# A. Trim end date sensitivity (2018-05)
cat("--- Trim to 2018-05 ---\n")
df_trim05 <- build_df(panel_raw, y_col = "y_log1p_per1k_18_49", 
                      end_date = as.Date("2018-05-01"))
att_trim05 <- tryCatch(run_cs(df_trim05, gname = "G_trim", anticipation = ANT_USE,
                              est_method = "ipw", bstrap = FALSE),
                       error = function(e) NULL)
if (!is.null(att_trim05)) {
  cs_trim05 <- post_window_att_only(att_trim05, e_min = 1, e_max = 4)
  boot_trim05 <- boot_post_window(df_trim05, anticipation = ANT_USE, e_min = 1, e_max = 4,
                                  est_method = "ipw", B = BOOT_ITERS, seed = SEED + 10)
  cat(sprintf("CS Post 1-4 (trim 2018-05): %.4f (boot SE = %.4f, n_eff=%d)\n", 
              cs_trim05$att, boot_trim05$se, boot_trim05$n_eff))
  results_list[["Robust: Trim 2018-05 (post 1-4)"]] <- list(
    att = cs_trim05$att, se = boot_trim05$se,
    ci_lower = boot_trim05$ci_lo, ci_upper = boot_trim05$ci_hi, is_boot = TRUE)
}

# B. Anticipation sensitivity
cat("\n--- Anticipation sensitivity ---\n")
for (ant in c(0L, 1L, 3L)) {
  att_ant <- tryCatch(run_cs(df_main, gname = "G_trim", anticipation = ant,
                             est_method = "ipw", bstrap = FALSE),
                      error = function(e) NULL)
  if (!is.null(att_ant)) {
    cs_ant <- post_window_att_only(att_ant, e_min = 1, e_max = 5)
    boot_ant <- boot_post_window(df_main, anticipation = ant, e_min = 1, e_max = 5,
                                 est_method = "ipw", B = BOOT_ITERS, seed = SEED + 20 + ant)
    cat(sprintf("Anticipation = %d: Post 1-5 = %.4f (boot SE = %.4f, n_eff=%d)\n", 
                ant, cs_ant$att, boot_ant$se, boot_ant$n_eff))
    results_list[[sprintf("Robust: Ant=%d (post 1-5)", ant)]] <- list(
      att = cs_ant$att, se = boot_ant$se,
      ci_lower = boot_ant$ci_lo, ci_upper = boot_ant$ci_hi, is_boot = TRUE)
  }
}

# C. Exclude Wayne
cat("\n--- Exclude Wayne ---\n")
df_no_wayne <- df_main %>% filter(county != "Wayne")
att_no_wayne <- tryCatch(run_cs(df_no_wayne, gname = "G_trim", anticipation = ANT_USE,
                                est_method = "ipw", bstrap = FALSE),
                         error = function(e) NULL)
if (!is.null(att_no_wayne)) {
  cs_no_wayne <- post_window_att_only(att_no_wayne, e_min = 1, e_max = 5)
  boot_no_wayne <- boot_post_window(df_no_wayne, anticipation = ANT_USE, e_min = 1, e_max = 5,
                                    est_method = "ipw", B = BOOT_ITERS, seed = SEED + 30)
  cat(sprintf("Excl. Wayne: Post 1-5 = %.4f (boot SE = %.4f, n_eff=%d)\n", 
              cs_no_wayne$att, boot_no_wayne$se, boot_no_wayne$n_eff))
  results_list[["Robust: Excl. Wayne"]] <- list(
    att = cs_no_wayne$att, se = boot_no_wayne$se,
    ci_lower = boot_no_wayne$ci_lo, ci_upper = boot_no_wayne$ci_hi, is_boot = TRUE)
}

# D. Leave-one-out for early cohort (2017-01, 4 counties)
cat("\n--- Leave-one-out (2017-01 cohort) ---\n")
g_2017 <- min(unique(df_main$G_trim[df_main$G_trim > 0]))
counties_2017 <- unique(df_main$county[df_main$G_trim == g_2017])

if (length(counties_2017) > 1) {
  loo_results <- sapply(counties_2017, function(cty) {
    df_loo <- df_main %>% filter(county != cty)
    att_loo <- tryCatch(run_cs(df_loo, gname = "G_trim", anticipation = ANT_USE,
                               est_method = "ipw", bstrap = FALSE),
                        error = function(e) NULL)
    if (!is.null(att_loo)) {
      post_window_att_only(att_loo, e_min = 1, e_max = 5)$att
    } else NA_real_
  })
  
  loo_range <- range(loo_results, na.rm = TRUE)
  loo_mean <- mean(loo_results, na.rm = TRUE)
  loo_sd <- sd(loo_results, na.rm = TRUE)
  cat(sprintf("LOO range: [%.4f, %.4f]\n", loo_range[1], loo_range[2]))
  cat(sprintf("LOO mean (SD): %.4f (%.4f)\n", loo_mean, loo_sd))
  results_list[["Robust: LOO mean"]] <- list(att = loo_mean, se = loo_sd, is_loo = TRUE)
}

# E. Population-weighted
cat("\n--- Population-weighted ---\n")
weights_col <- intersect(c("w_pop_18_49", "population_18_49"), names(df_main))[1]
if (!is.na(weights_col)) {
att_weighted <- tryCatch(
    run_cs(df_main, gname = "G_trim", anticipation = ANT_USE, weightsname = weights_col,
           est_method = "ipw", bstrap = FALSE),
    error = function(e) NULL
  )
  if (!is.null(att_weighted)) {
    cs_weighted <- post_window_att_only(att_weighted, e_min = 1, e_max = 5)
    boot_weighted <- boot_post_window(df_main, anticipation = ANT_USE, e_min = 1, e_max = 5,
                                      est_method = "ipw", B = BOOT_ITERS,
                                      seed = SEED + 40)
    cat(sprintf("Pop-weighted: Post 1-5 = %.4f (boot SE = %.4f, n_eff=%d)\n", 
                cs_weighted$att, boot_weighted$se, boot_weighted$n_eff))
    results_list[["Robust: Pop-weighted"]] <- list(
      att = cs_weighted$att, se = boot_weighted$se,
      ci_lower = boot_weighted$ci_lo, ci_upper = boot_weighted$ci_hi, is_boot = TRUE)
  }
}

############################################################
# 6. RESULTS SUMMARY
############################################################

cat("\n", strrep("=", 60), "\n")
cat(" RESULTS SUMMARY\n")
cat(strrep("=", 60), "\n\n")
cat("Note: LOO mean is descriptive (no CI/p-value).\n\n")

results_df <- tibble(
  Specification = names(results_list),
  ATT = sapply(results_list, function(x) x$att),
  SE  = sapply(results_list, function(x) x$se),
  CI_lower = sapply(results_list, function(x) if (!is.null(x$ci_lower)) x$ci_lower else NA_real_),
  CI_upper = sapply(results_list, function(x) if (!is.null(x$ci_upper)) x$ci_upper else NA_real_),
  is_boot  = sapply(results_list, function(x) isTRUE(x$is_boot)),
  is_loo   = sapply(results_list, function(x) isTRUE(x$is_loo))
) %>%
  mutate(
    CI_lower = ifelse(is.na(CI_lower) & !is_boot & !is_loo & is.finite(SE), ATT - 1.96 * SE, CI_lower),
    CI_upper = ifelse(is.na(CI_upper) & !is_boot & !is_loo & is.finite(SE), ATT + 1.96 * SE, CI_upper),
    p_value  = ifelse(!is_boot & !is_loo & is.finite(SE), 2 * pnorm(-abs(ATT / SE)), NA_real_),
    Sig = case_when(
      is.na(p_value) ~ "",
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.10 ~ "*",
      TRUE ~ ""
    )
  )

# Main results
cat("=== MAIN RESULTS ===\n")
main_tbl <- results_df %>% 
  filter(grepl("^Main:", Specification)) %>%
  select(Specification, ATT, SE, CI_lower, CI_upper, Sig)
print(main_tbl)

# Alternatives
cat("\n=== ALTERNATIVE ESTIMATORS ===\n")
alt_tbl <- results_df %>%
  filter(grepl("^Alt:", Specification)) %>%
  select(Specification, ATT, SE, CI_lower, CI_upper, Sig)
print(alt_tbl)

# Robustness
cat("\n=== ROBUSTNESS CHECKS ===\n")
rob_tbl <- results_df %>%
  filter(grepl("^Robust:", Specification)) %>%
  select(Specification, ATT, SE, CI_lower, CI_upper, Sig)
print(rob_tbl)

# Save
write_csv(results_df, file.path(outdir, "did_results_main.csv"))
cat("\n[+] Results saved to did_results_main.csv\n")

cat("\n", strrep("=", 60), "\n")
cat(" INTERPRETATION\n")
cat(strrep("=", 60), "\n")
sample_start <- min(df_main$date, na.rm = TRUE)
sample_end <- max(df_main$date, na.rm = TRUE)
cat(sprintf("
Main finding: Effect of early ABAWD enforcement (2017-01 and 2018-01 
cohorts) on SNAP participation, using counties not yet enforced through 
June 2018 as controls.

Sample: %s to %s (trimmed before 68-county expansion)
Treated: 14 counties (4 in 2017-01, 10 in 2018-01)
Controls: 69 counties (not yet treated within sample window)
Anticipation: 2 months excluded before enforcement

Main estimand: Post 1-5 month average ATT (cohort-weighted)
Inference: county-cluster bootstrap (external resampling)
", sample_start, sample_end))
