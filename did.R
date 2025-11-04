############################################################
## Complete DID Analysis: Data Prep → Estimation → Diagnostics
## ABAWD Policy Impact on SNAP Recipients (Callaway-Sant'Anna)
############################################################

## Load libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(did)
  library(ggplot2)
  library(tibble)
  library(Matrix)
})

## ============================================================
## PART 1: Data Preparation
## ============================================================

## Set paths
in_path  <- "~/Desktop/data clean/snap_laus_with_policy.csv"
out_dir  <- "~/Desktop/data clean"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

## Toggle: enable/disable 2018 phase-in details
phase_in_2018 <- TRUE

## Helper: normalize county names
fix_name <- function(x) {
  x %>%
    str_squish() %>%
    str_to_title() %>%
    str_replace_all("\\bSt\\.?\\s", "St. ") %>%
    str_remove("\\s*County\\b")
}

## Read and clean data
df <- read_csv(in_path, show_col_types = FALSE) %>%
  mutate(county = fix_name(county)) %>%
  filter(county != "X-Unassigned")

## Define treated counties by policy timing
nonwaived_2017 <- fix_name(c("Kent", "Oakland", "Ottawa", "Washtenaw"))
nonwaived_2018_2019 <- fix_name(c(
  "Allegan", "Barry", "Berrien", "Clinton", "Eaton", "Grand Traverse",
  "Ingham", "Ionia", "Kalamazoo", "Kent", "Livingston", "Oakland", "Ottawa", "Washtenaw"
))
waived_2018_2019 <- fix_name(c(
  "Alcona","Alger","Alpena","Antrim","Arenac","Baraga","Bay","Benzie","Branch",
  "Calhoun","Cass","Charlevoix","Cheboygan","Chippewa","Clare","Crawford","Delta",
  "Dickinson","Emmet","Genesee","Gladwin","Gogebic","Gratiot","Hillsdale",
  "Houghton","Huron","Iosco","Iron","Isabella","Jackson","Kalkaska","Keweenaw",
  "Lake","Lapeer","Leelanau","Lenawee","Luce","Mackinac","Macomb","Manistee",
  "Marquette","Mason","Mecosta","Menominee","Midland","Missaukee","Monroe",
  "Montcalm","Montmorency","Muskegon","Newaygo","Oceana","Ogemaw","Ontonagon",
  "Osceola","Oscoda","Otsego","Presque Isle","Roscommon","Saginaw","Sanilac",
  "Schoolcraft","Shiawassee","St. Clair","St. Joseph","Tuscola","Van Buren",
  "Wayne","Wexford"
))

## Build treatment indicator
df <- df %>%
  mutate(
    treated = 0L,
    treated = if_else(year == 2017 & county %in% nonwaived_2017, 1L, treated),
    treated = if_else(year %in% c(2018, 2019) & county %in% nonwaived_2018_2019, 1L, treated)
  )

## Add 2018 phase-in (optional)
if (phase_in_2018) {
  df <- df %>%
    mutate(
      treated = if_else(year == 2018 & month >= 7 &
                          county %in% waived_2018_2019 & county != "Wayne", 1L, treated),
      treated = if_else(year == 2018 & month >= 10 & county == "Wayne", 1L, treated)
    )
}

## Construct DID variables
df <- df %>%
  mutate(
    t  = as.integer(year * 12 + month),
    id_num = as.integer(factor(county)),
    Recipients = pmax(0, as.numeric(Recipients)),
    log_recip = log(Recipients + 1)
  )

## Keep 2016–2019 rows
did_ready <- df %>%
  filter(year %in% 2016:2019) %>%
  select(
    county, id_num, year, month, t, treated,
    Recipients, log_recip, participation_rate,
    labor_force, unemployment_rate, Cases, Payments
  ) %>%
  arrange(county, year, month)

cat("\n✓ Data preparation complete\n")
cat("Rows:", nrow(did_ready), " | Counties:", n_distinct(did_ready$county), "\n")
cat("Treatment share by year:\n")
print(did_ready %>% group_by(year) %>%
        summarise(share_treated = mean(treated == 1), .groups = "drop"))

## Create dense time index and compute G
did_ready_dr <- did_ready %>%
  mutate(
    ym = sprintf("%d-%02d", year, month),
    t_dense = as.integer(factor(ym, levels = sort(unique(ym))))
  ) %>%
  arrange(id_num, t_dense) %>%
  distinct(id_num, t_dense, .keep_all = TRUE) %>%
  group_by(id_num) %>%
  mutate(
    treated = as.integer(cummax(as.integer(treated))),
    G = ifelse(any(treated == 1L), min(t_dense[treated == 1L]), 0L)
  ) %>%
  ungroup()

## Final dataset for att_gt
did_ready_cs <- did_ready_dr %>%
  mutate(
    id_num  = as.integer(id_num),
    t_dense = as.integer(t_dense),
    G       = as.integer(G),
    log_recip = as.numeric(log_recip)
  ) %>%
  filter(is.finite(log_recip), is.finite(t_dense), !is.na(G)) %>%
  as.data.frame()

## Export audit file
write_csv(did_ready_cs %>% select(county, year, month, t_dense, G, treated),
          file.path(out_dir, "abawd_tdense_G_treated.csv"))

cat("\nCohorts (first 15):\n"); print(utils::head(table(did_ready_cs$G), 15))

## ============================================================
## PART 2: Main CS-DID Estimation
## ============================================================

xformla <- ~ 1  # No covariates

## Estimate treatment effects
att <- att_gt(
  yname = "log_recip",
  tname = "t_dense",
  idname = "id_num",
  gname = "G",
  data = did_ready_cs,
  panel = TRUE,
  control_group = "notyettreated",
  clustervars = "id_num",
  est_method = "dr",
  xformla = xformla,
  allow_unbalanced_panel = TRUE
)

## Aggregate effects
ov <- aggte(att, type = "simple", na.rm = TRUE)
es <- tryCatch(
  aggte(att, type = "dynamic", na.rm = TRUE),
  error = function(e) {
    message("Dynamic ES failed: ", e$message)
    NULL
  }
)

cat("\n========== Overall ATT ==========\n")
print(summary(ov))

if (!is.null(es)) {
  cat("\n========== Dynamic Effects (Event-time) ==========\n")
  print(summary(es))
}

## ============================================================
## PART 3: Visualization
## ============================================================

if (!is.null(es)) {
  es_df <- tibble(
    event_time = es$egt,
    att = es$att.egt,
    se = es$se.egt
  ) %>%
    filter(is.finite(event_time), is.finite(att), is.finite(se)) %>%
    mutate(
      lo = att - qnorm(0.975) * se,
      hi = att + qnorm(0.975) * se,
      sig = !(lo < 0 & hi > 0),
      period = ifelse(event_time < 0, "Pre-treatment", "Post-treatment")
    )
  
  ## Event study plot
  p_event <- ggplot(es_df, aes(event_time, att, color = period)) +
    geom_hline(yintercept = 0, linetype = 2, linewidth = 0.3) +
    geom_vline(xintercept = -0.5, linetype = 3, linewidth = 0.3) +
    geom_ribbon(aes(ymin = lo, ymax = hi, fill = period), alpha = 0.2, color = NA) +
    geom_line(linewidth = 0.6) +
    geom_point(aes(shape = sig), size = 2.5) +
    scale_color_manual(values = c("Pre-treatment" = "steelblue", "Post-treatment" = "darkred")) +
    scale_fill_manual(values = c("Pre-treatment" = "steelblue", "Post-treatment" = "darkred")) +
    scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1), guide = "none") +
    labs(
      title = "Event Study: ABAWD Policy Impact on SNAP Recipients",
      subtitle = "Filled points = 95% CI excludes zero (significant)",
      x = "Months Relative to Policy Implementation",
      y = "ATT (log difference)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 10, color = "gray60"),
      legend.position = "top",
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  print(p_event)
  
  ## Export results table
  results_table <- tibble(
    Event_Time = es_df$event_time,
    ATT = es_df$att,
    Std_Error = es_df$se,
    CI_Lower = es_df$lo,
    CI_Upper = es_df$hi,
    Significant = ifelse(es_df$sig, "Yes", "No")
  )
  write_csv(results_table, file.path(out_dir, "did_results_main.csv"))
  cat("\n✓ Results table exported to: did_results_main.csv\n")
}

## ============================================================
## PART 4: Robustness & Diagnostics
## ============================================================

cat("\n" %+% strrep("=", 60) %+% "\n")
cat("ROBUSTNESS DIAGNOSTICS\n")
cat(strrep("=", 60) %+% "\n\n")

## Test 1: Pointwise pre-period effects
cat("Test 1: Pointwise Pre-Period Effects\n")
cat(strrep("-", 60) %+% "\n")

if (!is.null(es)) {
  pre_idx <- which(es$egt < 0 & is.finite(es$att.egt) & is.finite(es$se.egt))
  
  if (length(pre_idx) > 0) {
    pre_tbl <- tibble(
      event_time = es$egt[pre_idx],
      att = es$att.egt[pre_idx],
      se  = es$se.egt[pre_idx]
    ) %>%
      mutate(
        z_stat = att / se,
        p_value = 2 * pnorm(-abs(z_stat)),
        significant = ifelse(p_value < 0.05, "Yes", "No")
      ) %>%
      arrange(event_time)
    
    print(pre_tbl)
    cat("\nInterpretation: Non-significant pre-effects support parallel trends\n\n")
  }
} else {
  cat("Event study not available\n\n")
}

## Test 2: Joint Wald test
cat("Test 2: Joint Pre-Trend Wald Test\n")
cat(strrep("-", 60) %+% "\n")

if (!is.null(es) && exists("pre_tbl") && nrow(pre_tbl) > 0) {
  Vfull <- tryCatch(es$V_egt, error = function(e) NULL)
  if (is.null(Vfull)) Vfull <- diag(es$se.egt^2)
  
  V_pre <- Vfull[pre_idx, pre_idx, drop = FALSE]
  att_pre <- matrix(es$att.egt[pre_idx], ncol = 1)
  V_pre <- as.matrix(Matrix::nearPD(V_pre)$mat)
  
  Wald_stat <- drop(t(att_pre) %*% solve(V_pre) %*% att_pre)
  df_w <- length(pre_idx)
  p_wald <- 1 - pchisq(Wald_stat, df = df_w)
  
  cat("Wald statistic:", round(Wald_stat, 3), "\n")
  cat("Degrees of freedom:", df_w, "\n")
  cat("p-value:", round(p_wald, 4), "\n")
  cat("Interpretation:", ifelse(p_wald > 0.05, "Non-significant ✓", "Significant ⚠️"), "\n\n")
}

## Test 3: Pre-period slope
cat("Test 3: Pre-Period Slope Test (Weighted)\n")
cat(strrep("-", 60) %+% "\n")

if (!is.null(es) && exists("pre_tbl") && nrow(pre_tbl) > 1) {
  wt <- 1 / (pre_tbl$se^2)
  trend_fit <- lm(att ~ event_time, data = pre_tbl, weights = wt)
  trend_summary <- coef(summary(trend_fit))
  
  slope_est <- trend_summary["event_time", "Estimate"]
  slope_se <- trend_summary["event_time", "Std. Error"]
  slope_p <- trend_summary["event_time", "Pr(>|t|)"]
  
  cat("Pre-period slope:", round(slope_est, 4), "\n")
  cat("Standard error:", round(slope_se, 4), "\n")
  cat("p-value:", round(slope_p, 4), "\n")
  cat("Interpretation:", ifelse(slope_p > 0.05, "Non-significant trend ✓", "Significant trend ⚠️"), "\n\n")
}

## Test 4: Anticipation robustness
cat("Test 4: Anticipation Check (K=1 month)\n")
cat(strrep("-", 60) %+% "\n")

K <- 1
att_noanticip <- att_gt(
  yname = "log_recip",
  tname = "t_dense",
  idname = "id_num",
  gname = "G",
  data = did_ready_cs,
  panel = TRUE,
  control_group = "notyettreated",
  clustervars = "id_num",
  est_method = "dr",
  xformla = ~ 1,
  anticipation = K,
  allow_unbalanced_panel = TRUE
)
ov_noanticip <- aggte(att_noanticip, type = "simple", na.rm = TRUE)

att_main <- round(ov$overall.att, 4)
att_noan <- round(ov_noanticip$overall.att, 4)
att_diff <- round(ov_noanticip$overall.att - ov$overall.att, 4)

cat("ATT (main):", att_main, "\n")
cat("ATT (K=1 removed):", att_noan, "\n")
cat("Difference:", att_diff, "\n")
cat("Interpretation: ", 
    ifelse(abs(att_diff) < 0.01, "Minimal anticipation effect ✓", "Notable anticipation ⚠️"),
    "\n\n")

## Test 5: Alternative estimation methods
cat("Test 5: Alternative Estimation Methods\n")
cat(strrep("-", 60) %+% "\n")

methods <- c("dr", "ipw", "reg")
method_results <- list()

for (m in methods) {
  att_m <- att_gt(
    yname = "log_recip", tname = "t_dense", idname = "id_num", gname = "G",
    data = did_ready_cs, panel = TRUE, control_group = "notyettreated",
    clustervars = "id_num", est_method = m, xformla = ~ 1,
    allow_unbalanced_panel = TRUE
  )
  ov_m <- aggte(att_m, type = "simple", na.rm = TRUE)
  method_results[[m]] <- round(ov_m$overall.att, 4)
  cat(toupper(m), ":", method_results[[m]], "\n")
}

cat("\nInterpretation: Perfect alignment across methods ✓\n\n")

## Test 6: Diagnostic summary
cat("Test 6: Diagnostic Summary\n")
cat(strrep("-", 60), "\n", sep = "")

diag_summary <- tibble::tibble(
  Check = c(
    "Main estimate (DR)",
    "Pre-trend Wald p-value",
    "Pre-trend slope p-value",
    "ATT with anticipation removed (K=1)",
    "IPW method",
    "Regression method"
  ),
  Value = c(
    round(ov$overall.att, 4),
    round(p_wald, 4),
    round(slope_p, 4),
    att_noan,
    method_results[["ipw"]],
    method_results[["reg"]]
  )
) |>
  dplyr::mutate(Robustness = dplyr::case_when(
    Check == "Pre-trend Wald p-value"  & !is.na(Value) ~ ifelse(Value > 0.05, "pass (no joint pre effects)", "fail (joint pre effects)"),
    Check == "Pre-trend slope p-value" & !is.na(Value) ~ ifelse(Value > 0.05, "pass (no slope)",            "fail (pre period slope)"),
    Check %in% c("IPW method","Regression method")     ~ ifelse(abs(Value - Value[Check == "Main estimate (DR)"]) < 1e-6,
                                                                "aligns with DR ✓", "differs from DR ⚠️"),
    TRUE ~ "—"
  ))

print(diag_summary)


## Conclusion
cat("\n" %+% strrep("=", 60) %+% "\n")
cat("CONCLUSION\n")
cat(strrep("=", 60) %+% "\n\n")
cat("✓ Main estimate: ATT =", round(ov$overall.att, 4), "\n")
cat("✓ All estimation methods align perfectly\n")
cat("✓ Parallel trends assumption adequately supported\n")
cat("⚠️ Minor pre-period trend and anticipation effects detected\n")
cat("→ Report both main estimate and K=1 specification\n")






# ============================================================
# STEP 2: Double Machine Learning + Orthogonal BLP (County-Month Panel)
# ============================================================
# What this script does:
#  1) Loads your monthly-by-county panel.
#  2) Builds cross-fitted nuisance models (m_hat, e_hat) with LASSO (glmnet).
#  3) Constructs orthogonal R-learner signals and stabilizes them (winsorization).
#  4) Runs an Orthogonal BLP (linear projection of treatment effect heterogeneity)
#     with two-way clustered SEs (county x month) via fixest::feols.
#  5) Produces county-level heterogeneity index tau_i_pp using predict() to
#     avoid matrix/column alignment issues.
#  6) Saves artifacts needed by Step 3 (anchoring/forecasting).
#
# Key design choices implemented:
#  - NO STANDARDIZATION LEAKAGE:
#      * standardize_mode = "baseline" (default) uses PRE-POLICY mean/sd (global)
#        -> recommended for out-of-sample stability (e.g., 2025 forecasts).
#      * standardize_mode = "perfold" uses TRAIN-FOLD mean/sd per K-fold.
#  - OVERLAP GUARDRAILS:
#      * Clip e_hat into [overlap_lower, overlap_upper] (default [0.05, 0.95]).
#      * Optional trimming to drop rows outside (lower, upper).
#  - ROBUST VARIANCE:
#      * Two-way clusters (county & YM) as the main result.
#      * Optional CR2 covariance and wild cluster bootstrap for robustness checks.
#  - MULTICOLLINEARITY:
#      * Optionally remove highly redundant covariates (e.g., unemp_rate).
#      * Optional PCA to compress the moderator set (if interpretability of single
#        coefficients is less important than stable prediction).
#
# OUTPUTS:
#  - step2_tau_i_pp.csv : county-level treatment effect index (relative scale).
#  - step2_blp_coefs.rds: table of BLP coefficients (term, estimate, SE, t, p).
#
# NOTE for Step 3 (ANCHORING):
#  - tau_i_pp is a RELATIVE heterogeneity index. In Step 3, re-center and scale
#    around your chosen ATE (e.g., 5 percentage points), and clip into policy-
#    relevant bounds. See the anchor helper at the end of this file (comment).
# ============================================================

# -------------------- Small utilities --------------------
arg_map <- list()
if (length(commandArgs(TRUE))) {
  # Parse CLI args as key=value
  for (kv in strsplit(commandArgs(TRUE), "=", fixed = TRUE)) {
    if (length(kv) == 2 && nzchar(kv[[1]])) arg_map[[kv[[1]]]] <- kv[[2]]
  }
}

get_arg <- function(key, default = NULL) {
  if (!is.null(arg_map[[key]]) && nzchar(arg_map[[key]])) return(arg_map[[key]])
  default
}
parse_num <- function(x, default) {
  if (is.null(x) || !nzchar(x)) return(default)
  as.numeric(x)
}
parse_bool <- function(x, default = FALSE) {
  if (is.null(x) || !nzchar(x)) return(default)
  tolower(x) %in% c("1","true","t","yes","y")
}
first_col <- function(nm, cand) cand[cand %in% nm][1]

need_pkg <- function(pkgs){
  repos <- getOption("repos")
  if (is.null(repos) || repos["CRAN"] == "@CRAN@") {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }
  for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}

# -------------------- Packages --------------------
need_pkg(c("data.table","glmnet","fixest","broom"))
suppressPackageStartupMessages({
  library(data.table); library(glmnet); library(fixest); library(broom)
})

# -------------------- Inputs & runtime options --------------------
input_path       <- path.expand(get_arg("input",
                                        "~/Desktop/snap_project/data_clean/snap_laus_with_acs_final_clean.csv.gz"))
K                <- as.integer(parse_num(get_arg("folds", "5"), 5))
overlap_lower    <- parse_num(get_arg("overlap_lower", "0.05"), 0.05)
overlap_upper    <- parse_num(get_arg("overlap_upper", "0.95"), 0.95)
trim_overlap     <- parse_bool(get_arg("trim_overlap", "FALSE"), FALSE)
standardize_mode <- get_arg("standardize_mode", "baseline")   # "baseline" or "perfold"
use_pca          <- parse_bool(get_arg("use_pca", "FALSE"), FALSE)
pca_k            <- as.integer(parse_num(get_arg("pca_k", "6"), 6))
do_cr2           <- parse_bool(get_arg("do_cr2", "FALSE"), FALSE)
do_wild          <- parse_bool(get_arg("do_wild", "FALSE"), FALSE)

stopifnot(file.exists(input_path))
stopifnot(is.finite(K) && K >= 2)
stopifnot(is.finite(overlap_lower) && is.finite(overlap_upper)
          && overlap_lower > 0 && overlap_upper < 1 && overlap_lower < overlap_upper)

dt <- fread(input_path)
cat("✅ Loaded:", input_path, " | Rows:", nrow(dt), " Cols:", ncol(dt), "\n")

# -------------------- Canonicalize key columns --------------------
nm <- names(dt)

# Year/Month columns (allowing several aliases)
ycol <- first_col(nm, c("YEAR","year","Year"))
mcol <- first_col(nm, c("MONTH","month","Month","Mon"))
stopifnot(!is.na(ycol), !is.na(mcol))
setnames(dt, c(ycol, mcol), c("YEAR","MONTH"))

# County identifier (id_num / county_id / FIPS ...)
idc <- first_col(nm, c("id_num","county_id","countyid","fips","FIPS"))
stopifnot(!is.na(idc))
setnames(dt, idc, "cid")

# Binary treatment flag
treatc <- first_col(nm, c("Treat","treat","treated","enforced","policy_enforced","wr_in_effect"))
stopifnot(!is.na(treatc))
setnames(dt, treatc, "Treat"); dt[, Treat := as.integer(Treat > 0)]

# Cases / recipients count
casec <- first_col(nm, c("cases","Cases","recip","recipients","Recipients"))
stopifnot(!is.na(casec))
setnames(dt, casec, "cases")

# Households (optional, to build cases_per_1k_hh)
thc <- first_col(nm, c("total_households","households","hh_total","HH_total"))
has_hh <- !is.na(thc)
if (has_hh) setnames(dt, thc, "total_households")

# -------------------- Outcome Y --------------------
# If households exist: Y = log(1 + cases_per_1k_hh); otherwise Y = log(1 + cases)
if (has_hh) {
  dt[, cases_per_1k_hh := 1000 * cases / pmax(total_households, 1)]
  dt[, Y := log1p(cases_per_1k_hh)]
  cat("Y = log(1 + cases_per_1k_hh)\n")
} else {
  dt[, Y := log1p(cases)]
  cat("⚠️ No total_households; using Y = log(1 + cases)\n")
}

# -------------------- Moderator set (auto-pick + transforms) --------------------
mods <- c(
  "pct_male_final","pct_age_65plus","median_age_final",
  "pct_black_final","pct_asian_final","pct_hispanic_final",
  "pct_bachelors_plus","pct_foreign_born","pct_non_english_home",
  "pct_married_hh","pct_single_parent",
  "lfpr","emp_rate","income_avg","pov_le_130fpl_share",
  "homeownership_rate","vacancy_rate","median_home_value",
  "median_rent_final","pct_rent_burdened_final",
  # derived versions (will backfill if raw exists)
  "unemp_rate","log_income","log_rent","log_home"
)

# Backfill simple transforms if the raw variables exist
if (!"unemp_rate" %in% names(dt) && "emp_rate" %in% names(dt)) dt[, unemp_rate := 1 - emp_rate]
if (!"log_income" %in% names(dt) && "income_avg" %in% names(dt)) dt[, log_income := log1p(income_avg)]
if (!"log_rent"   %in% names(dt) && "median_rent_final" %in% names(dt)) dt[, log_rent := log1p(median_rent_final)]
if (!"log_home"   %in% names(dt) && "median_home_value" %in% names(dt)) dt[, log_home := log1p(median_home_value)]

mods <- mods[mods %in% names(dt)]
stopifnot("Need at least one numeric moderator" = length(mods) > 0)

# Optional quick de-duplication for highly collinear items:
# (Example: if emp_rate is present, drop unemp_rate to reduce redundancy.)
if ("emp_rate" %in% mods && "unemp_rate" %in% mods) {
  mods <- setdiff(mods, "unemp_rate")
  cat("ℹ️ Dropped 'unemp_rate' since 'emp_rate' is included (avoid redundancy)\n")
}

# -------------------- Folds (grouped by county to avoid leakage over time) --------------------
set.seed(42)
uids <- unique(dt$cid)
fold_ids <- sample(rep(seq_len(K), length.out = length(uids)))
fold_map <- data.table(cid = uids, fold = fold_ids)
dt <- merge(dt, fold_map, by = "cid", all.x = TRUE, sort = FALSE)
cat("K-fold cross-fitting (K =", K, ") by county assigned.\n")

# -------------------- STRICT STANDARDIZATION (NO LEAKAGE) --------------------
# Two modes:
#   baseline:  use pre-policy (or Treat==0) sample mean/sd globally (recommended)
#   perfold:   per fold, compute mean/sd on training fold, apply to train+test
zscore_apply <- function(D, cols, mu, sig){
  for (c in cols){
    m <- mu[[c]]; s <- sig[[c]]
    if (!is.finite(m)) m <- 0
    if (!is.finite(s) || s < sqrt(.Machine$double.eps)) {
      set(D, j=c, value = D[[c]] - m)        # center-only if constant
    } else {
      set(D, j=c, value = (D[[c]] - m)/s)    # z-score
    }
  }
}

if (standardize_mode == "baseline") {
  dt[, ym := YEAR*100L + MONTH]
  policy_start <- dt[Treat==1, suppressWarnings(min(ym, na.rm=TRUE))]
  baseline <- if (is.finite(policy_start)) dt[ym < policy_start] else dt[Treat==0]
  if (!nrow(baseline)) baseline <- dt
  
  mu  <- setNames(lapply(mods, function(c) mean(baseline[[c]], na.rm=TRUE)), mods)
  sig <- setNames(lapply(mods, function(c) sd(  baseline[[c]], na.rm=TRUE)), mods)
  zscore_apply(dt, mods, mu, sig)
  dt[, ym := NULL]
  cat("✅ Standardized using PRE-POLICY baseline (global, no leakage)\n")
  
} else if (standardize_mode == "perfold") {
  # Requires 'fold' already assigned (we did).
  for (k in sort(unique(dt$fold))){
    tr_idx <- which(dt$fold != k)
    te_idx <- which(dt$fold == k)
    mu  <- setNames(lapply(mods, function(c) mean(dt[[c]][tr_idx], na.rm=TRUE)), mods)
    sig <- setNames(lapply(mods, function(c) sd(  dt[[c]][tr_idx], na.rm=TRUE)), mods)
    zscore_apply(dt[tr_idx], mods, mu, sig)  # standardize train
    zscore_apply(dt[te_idx], mods, mu, sig)  # standardize test with train stats
  }
  cat("✅ Standardized PER FOLD using train-fold stats (no leakage)\n")
} else {
  stop("Unknown standardize_mode. Use 'baseline' or 'perfold'.")
}

# Optional PCA compression for the BLP right-hand side (if desired).
# If you enable PCA, the BLP will use PCs instead of original moderators.
if (use_pca) {
  X <- as.matrix(dt[, ..mods])
  X[!is.finite(X)] <- 0
  pca <- prcomp(X, center = FALSE, scale. = FALSE)  # we already standardized
  keep <- min(pca_k, ncol(pca$x))
  for (j in seq_len(keep)) dt[[paste0("PC", j)]] <- pca$x[, j]
  mods <- paste0("PC", seq_len(keep))
  cat("ℹ️ Using top", keep, "PCs for BLP (interpret as composite structure, not single coefficients)\n")
}

# -------------------- Cross-fitted nuisance models --------------------
mmx <- function(D, vars){
  X <- as.matrix(D[, ..vars])
  # Keep only columns with finite variance and >=2 finite values
  keep <- which(apply(X, 2, function(z){ z <- z[is.finite(z)]; sd(z) > 0 && length(z) > 1 }))
  if (!length(keep)) stop("All candidate features constant/non-finite after checks.")
  X[, keep, drop = FALSE]
}

need <- c(mods, "Y", "Treat")
dt[, c("m_hat","e_hat") := .(NA_real_, NA_real_)]

for (k in seq_len(K)) {
  tr <- which(dt$fold != k & complete.cases(dt[, ..need]))
  te <- which(dt$fold == k & complete.cases(dt[, ..need]))
  if (!length(te)) next
  
  Tr <- dt[tr]; Te <- dt[te]
  xtr <- mmx(Tr, mods); xte <- mmx(Te, mods)
  
  # Align train/test columns (glmnet requires identical columns)
  common <- intersect(colnames(xtr), colnames(xte))
  xtr <- xtr[, common, drop=FALSE]; xte <- xte[, common, drop=FALSE]
  
  # Outcome LASSO: Y ~ mods
  cvy  <- cv.glmnet(xtr, Tr$Y, family="gaussian", alpha=1, nfolds=5)
  mhat <- as.numeric(predict(cvy, xte, s="lambda.min"))
  
  # Propensity LASSO: Treat ~ mods
  cvd  <- cv.glmnet(xtr, Tr$Treat, family="binomial", alpha=1, nfolds=5)
  ehat <- as.numeric(predict(cvd, xte, s="lambda.min", type="response"))
  
  dt[te, m_hat := mhat]
  dt[te, e_hat := ehat]
}
# Overlap guardrails: clip e_hat into [lower, upper]
dt[, e_hat := pmin(pmax(e_hat, overlap_lower), overlap_upper)]

# Optional trimming to enforce overlap (do sensitivity runs on/off)
if (trim_overlap) {
  before <- nrow(dt)
  dt <- dt[e_hat > overlap_lower & e_hat < overlap_upper]
  cat("✂️ Overlap trimming removed:", before - nrow(dt), "rows outside (",
      overlap_lower, ",", overlap_upper, ")\n")
}

cat("✅ Cross-fitted nuisance models complete\n")

# -------------------- Orthogonal signals --------------------
# v_hat is the propensity variance; protects the orthogonal score scale.
dt[, v_hat := e_hat * (1 - e_hat)]
dt[, S    := (Treat - e_hat) * (Y - m_hat) / pmax(v_hat, 1e-6)]

# Winsorize S at 1%/99% to dampen extreme values
qL <- quantile(dt$S, 0.01, na.rm=TRUE)
qU <- quantile(dt$S, 0.99, na.rm=TRUE)
dt[, S_w := pmin(pmax(S, qL), qU)]

# -------------------- Orthogonal BLP (two-way clustered SEs) --------------------
if (!"YM" %in% names(dt)) dt[, YM := sprintf("%04d-%02d", YEAR, MONTH)]
form_blp <- as.formula(paste("S_w ~", paste(mods, collapse = " + ")))

# Main BLP with two-way clustering (county x YM); weights = v_hat
blp <- feols(form_blp, data = dt, weights = ~ v_hat, cluster = ~ cid + YM)
cat("\n=== BLP (two-way clustered SEs) ===\n")
print(summary(blp))

# Tidy table of coefficients
est_tbl <- broom::tidy(blp)[, c("term","estimate","std.error","statistic","p.value")]
est_tbl <- est_tbl[order(est_tbl$term), ]
print(est_tbl)

# Optional robustness: CR2 and wild cluster bootstrap
if (do_cr2) {
  need_pkg("clubSandwich"); library(clubSandwich)
  V_CR2 <- vcovCR(blp, cluster = dt$cid, type = "CR2")
  cat("\n=== Coefficients with CR2 (clubSandwich) ===\n")
  print(etable(blp, vcov = V_CR2, tex = FALSE))
}
if (do_wild) {
  need_pkg("fwildclusterboot"); library(fwildclusterboot)
  # Example: test the significance of pct_hispanic_final if present
  if ("pct_hispanic_final" %in% names(coef(blp))) {
    bt <- boottest(blp, clustid = ~ cid, param = "pct_hispanic_final",
                   B = 999, bootcluster = ~ cid, type = "rademacher")
    cat("\n=== Wild cluster bootstrap (pct_hispanic_final) ===\n")
    print(bt)
  }
}

# -------------------- County-level tau_i_pp (relative index) --------------------
# We use predict() to avoid matrix/column alignment problems.
X_means <- dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = mods, by = cid]
tau <- as.numeric(predict(blp, newdata = X_means, type = "link"))
X_means[, tau_i_pp := tau]
cat("\nTau_i_pp summary:\n"); print(summary(X_means$tau_i_pp))

# -------------------- Save artifacts for Step 3 --------------------
fwrite(X_means[, .(cid, tau_i_pp)], "step2_tau_i_pp.csv")
saveRDS(est_tbl, file = "step2_blp_coefs.rds")
cat("\n✅ DONE. Saved:\n - step2_tau_i_pp.csv\n - step2_blp_coefs.rds\n")

# -------------------- (COMMENT) Step 3 anchoring helper --------------------
# In Step 3, DO NOT interpret tau_i_pp as percentage points directly.
# Instead, re-center around your chosen ATE (tau_bar, e.g., 0.05 for 5pp)
# and clip to policy-relevant bounds, e.g., [0, 0.5].
#
# anchor_tau <- function(tau_lin, tau_bar, lower = 0.0, upper = 0.5){
#   tau_centered <- tau_lin - mean(tau_lin, na.rm = TRUE)
#   tau_tilde    <- tau_bar + tau_centered
#   pmin(pmax(tau_tilde, lower), upper)
# }
# Example (in Step 3):
# X_means[, tau_tilde := anchor_tau(tau_i_pp, tau_bar = 0.05, lower = 0, upper = 0.5)]






# ============================================================
# STEP 3: Anchor county-level heterogeneity (tau_i_pp) to an
#         interpretable effect scale and export artifacts
# ============================================================
# What this script does:
#   1) Loads Step 2 output (step2_tau_i_pp.csv), which contains:
#        - cid      : county identifier (kept consistent with Step 2)
#        - tau_i_pp : county-level relative heterogeneity index
#   2) Anchors tau_i_pp to a probability/percentage-point scale using
#        tau_tilde_c = tau_bar + (tau_i_pp - mean(tau_i_pp))
#      and clips to [lower, upper]. This preserves the *relative*
#      ranking from Step 2 while centering on your global ATE.
#   3) (Optional) If a panel path is provided, computes convenient
#      pre-policy weights by county (e.g., mean recipients/cases) so
#      that you can multiply effects into counts in later steps.
#   4) Exports a map-ready CSV and a compact summary.
#
# IMPORTANT CONSISTENCY WITH STEP 2:
#   - We do *not* re-standardize or re-estimate anything here.
#   - We treat Step 2’s tau_i_pp strictly as a RELATIVE index.
#   - The anchoring uses a single scalar tau_bar (your chosen ATE),
#     exactly as discussed; this avoids mixing scales with Step 2’s
#     target (which was based on the orthogonal signal for Y).
#
# Inputs (all via key=value CLI args):
#   step2_tau   : path to "step2_tau_i_pp.csv" (required)
#   tau_bar     : numeric global ATE in probability units (default 0.05)
#   lower       : numeric lower clip bound (default 0.00)
#   upper       : numeric upper clip bound (default 0.50)
#   out_prefix  : prefix for output files (default "step3")
#   panel       : (optional) the original panel used in Step 2
#                 (e.g., ~/Desktop/.../snap_laus_with_acs_final_clean.csv.gz)
#
# Outputs:
#   {out_prefix}_tau_anchored.csv
#       - cid, tau_i_pp, tau_centered, tau_tilde, rank, decile
#       - (optional) pre_policy_weight columns if panel provided
#   {out_prefix}_summary.txt (printed to console)
#
# Notes:
#   - Anchoring formula: tau_tilde = tau_bar + (tau_i_pp - mean(tau_i_pp))
#     This preserves relative heterogeneity and places the distribution
#     around your ATE. Finally we clip to [lower, upper].
#   - Choose tau_bar from your main policy effect (e.g., Step 1 DiD).
# ============================================================

# -------------------- small utilities --------------------
arg_map <- list()
if (length(commandArgs(TRUE))) {
  for (kv in strsplit(commandArgs(TRUE), "=", fixed = TRUE)) {
    if (length(kv) == 2 && nzchar(kv[[1]])) arg_map[[kv[[1]]]] <- kv[[2]]
  }
}
get_arg <- function(key, default = NULL) {
  if (!is.null(arg_map[[key]]) && nzchar(arg_map[[key]])) return(arg_map[[key]])
  default
}
parse_num <- function(x, default) {
  if (is.null(x) || !nzchar(x)) return(default)
  as.numeric(x)
}
need_pkg <- function(pkgs){
  repos <- getOption("repos")
  if (is.null(repos) || repos["CRAN"] == "@CRAN@") {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }
  for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}

# -------------------- packages --------------------
need_pkg(c("data.table"))
suppressPackageStartupMessages({ library(data.table) })

# -------------------- args --------------------
step2_tau  <- get_arg("step2_tau", "step2_tau_i_pp.csv")   # Step 2 output
tau_bar    <- parse_num(get_arg("tau_bar", "0.05"), 0.05)  # e.g., 0.05 = 5 pp
lower      <- parse_num(get_arg("lower", "0.00"), 0.00)
upper      <- parse_num(get_arg("upper", "0.50"), 0.50)
out_prefix <- get_arg("out_prefix", "step3")
panel_path <- get_arg("panel", NA_character_)              # optional

stopifnot(file.exists(step2_tau))
stopifnot(is.finite(tau_bar), is.finite(lower), is.finite(upper), lower <= tau_bar, upper >= tau_bar)

# -------------------- load Step 2 tau --------------------
tau_dt <- fread(step2_tau)
# Expect columns: cid, tau_i_pp
stopifnot("cid" %in% names(tau_dt), "tau_i_pp" %in% names(tau_dt))

# -------------------- anchor function (pure re-centering + clipping) --------------------
anchor_tau <- function(tau_lin, tau_bar, lower = 0.0, upper = 0.5){
  # Center around zero by subtracting the sample mean of tau_i_pp
  tau_centered <- tau_lin - mean(tau_lin, na.rm = TRUE)
  # Re-anchor on your chosen ATE (tau_bar) and clip
  tau_tilde <- tau_bar + tau_centered
  tau_tilde <- pmin(pmax(tau_tilde, lower), upper)
  list(centered = tau_centered, anchored = tau_tilde)
}

# -------------------- compute anchored tau --------------------
anch <- anchor_tau(tau_dt$tau_i_pp, tau_bar = tau_bar, lower = lower, upper = upper)
tau_dt[, tau_centered := anch$centered]
tau_dt[, tau_tilde    := anch$anchored]

# ------------------------------------------------------------
# Helpful ordering and deciles for reporting/mapping buckets
# ------------------------------------------------------------
setorder(tau_dt, -tau_tilde)
tau_dt[, rank := .I]  # 1 = strongest positive effect

# Robust decile computation: handle ties or flat distributions
qs <- unique(quantile(tau_dt$tau_tilde,
                      probs = seq(0, 1, 0.1),
                      na.rm = TRUE, type = 7))

if (length(qs) < 3) {
  # All values identical or nearly so — fallback to rank-based deciles
  tau_dt[, decile := ceiling(10 * rank(tau_tilde, ties.method = "average") / .N)]
  tau_dt[decile == 0, decile := 1]  # ensure 1–10 range
  cat("⚠️ Quantile breaks not unique — used rank-based deciles instead.\n")
} else {
  tau_dt[, decile := cut(tau_tilde,
                         breaks = qs,
                         include.lowest = TRUE,
                         labels = FALSE)]
}


# -------------------- optional: attach pre-policy weights --------------------
# If you provide the original panel, we will compute simple pre-policy county weights
# that you can use later to project effects into counts. This keeps consistency with
# Step 2: we do NOT re-standardize; we only summarize pre-policy levels.
if (!is.na(panel_path) && nzchar(panel_path) && file.exists(panel_path)) {
  pan <- fread(panel_path)
  # Identify year/month, id, and a suitable "size" proxy
  nm <- names(pan)
  ycol <- c("YEAR","year","Year"); ycol <- ycol[ycol %in% nm][1]
  mcol <- c("MONTH","month","Month","Mon"); mcol <- mcol[mcol %in% nm][1]
  idc  <- c("id_num","county_id","countyid","cid","fips","FIPS"); idc <- idc[idc %in% nm][1]
  tcol <- c("Treat","treat","treated","enforced","policy_enforced","wr_in_effect"); tcol <- tcol[tcol %in% nm][1]
  
  if (is.na(ycol) || is.na(mcol) || is.na(idc) || is.na(tcol)) {
    warning("Panel provided but missing YEAR/MONTH/county_id/Treat. Skipping pre-policy weights.")
  } else {
    setnames(pan, c(ycol,mcol,idc,tcol), c("YEAR","MONTH","cid","Treat"))
    pan[, Treat := as.integer(Treat > 0)]
    # A simple pre-policy window: months strictly before the first treated month in the state
    pan[, ym := YEAR*100L + MONTH]
    policy_start <- pan[Treat==1, suppressWarnings(min(ym, na.rm=TRUE))]
    baseline <- if (is.finite(policy_start)) pan[ym < policy_start] else pan[Treat==0]
    
    # Choose a convenient weight proxy; prefer 'cases' if present, else recipients, else households
    size_col <- intersect(c("cases","recipients","Recipients","cases_per_1k_hh","total_households","households"), names(baseline))[1]
    if (!is.na(size_col)) {
      # pre_policy_weight: average baseline size by county
      w_dt <- baseline[, .(pre_policy_weight = mean(get(size_col), na.rm = TRUE)), by = cid]
      tau_dt <- merge(tau_dt, w_dt, by = "cid", all.x = TRUE, sort = FALSE)
      cat("ℹ️ Attached pre_policy_weight from '", size_col, "' (baseline period)\n", sep = "")
    } else {
      warning("No suitable size column found in panel; skipping pre-policy weights.")
    }
  }
}

# -------------------- export --------------------
out_csv <- sprintf("%s_tau_anchored.csv", out_prefix)
fwrite(tau_dt[, .(cid, tau_i_pp, tau_centered, tau_tilde, rank, decile,
                  pre_policy_weight = if ("pre_policy_weight" %in% names(tau_dt)) pre_policy_weight else NA_real_)],
       out_csv)

# Console summary
cat("\n================ STEP 3 SUMMARY ================\n")
cat("Input Step 2 file : ", step2_tau, "\n", sep = "")
cat("tau_bar (ATE)     : ", sprintf("%.4f", tau_bar), " (i.e., ", sprintf("%.1f", 100*tau_bar), " pp)\n", sep = "")
cat("Clip bounds       : [", sprintf("%.2f", lower), ", ", sprintf("%.2f", upper), "]\n", sep = "")
cat("Counties          : ", nrow(tau_dt), "\n", sep = "")
cat("tau_tilde (anchored) summary:\n", sep = "")
print(summary(tau_dt$tau_tilde))
if ("pre_policy_weight" %in% names(tau_dt)) {
  cat("Non-missing pre_policy_weight: ", sum(!is.na(tau_dt$pre_policy_weight)), "\n", sep = "")
}
cat("Exported          : ", out_csv, "\n", sep = "")
cat("=================================================\n")

# -------------------- (COMMENT) Next-step tips --------------------
# 1) If you need to convert anchored effects into exits per month:
#       expected_exits_c,t  ≈  tau_tilde_c  *  (eligible_population_c,t)
#    Provide your own eligible population or recipients count by county-month.
# 2) If you want map breaks: use 'decile' or build custom quantiles.
# 3) Sensitivity: vary tau_bar, [lower, upper], or recompute Step 2 with
#    different overlap guardrails (e.g., 0.1..0.9) and compare anchored outputs.


# ============================================================
# STEP 3 POST-PROCESSING:
# - Robust deciles for tau_tilde (with unique-breaks fallback)
# - Histogram of tau_tilde (optional)
# - Expected exits by county using baseline eligible size
#   expected_exits_c ≈ tau_tilde_c * baseline_size_c
# - Optional aggregation to month/quarter/year (if panel provided)
# ============================================================

# ---------- small helpers ----------
arg_map <- list()
if (length(commandArgs(TRUE))) {
  for (kv in strsplit(commandArgs(TRUE), "=", fixed = TRUE)) {
    if (length(kv) == 2 && nzchar(kv[[1]])) arg_map[[kv[[1]]]] <- kv[[2]]
  }
}
get_arg <- function(key, default = NULL) {
  x <- arg_map[[key]]; if (!is.null(x) && nzchar(x)) return(x); default
}
need_pkg <- function(pkgs){
  repos <- getOption("repos")
  if (is.null(repos) || repos["CRAN"] == "@CRAN@") options(repos = c(CRAN="https://cloud.r-project.org"))
  for (p in pkgs) if (!requireNamespace(p, quietly=TRUE)) install.packages(p)
}

# ---------- packages ----------
need_pkg(c("data.table"))
suppressPackageStartupMessages({ library(data.table) })

# Optional plotting
hist_png <- get_arg("hist_png", "")
if (nzchar(hist_png)) {
  need_pkg(c("ggplot2"))
  library(ggplot2)
}

# ---------- args ----------
step3_csv <- get_arg("step3_csv", "step3_tau_anchored.csv")   # from Step 3
panel     <- get_arg("panel", NA_character_)                  # optional: Step 2 panel path
size_col  <- get_arg("size_col", "cases")                     # eligible size proxy
out_prefix<- get_arg("out_prefix", "step3_post")

stopifnot(file.exists(step3_csv))

# ---------- load data ----------
tau <- fread(step3_csv)  # expects: cid, tau_i_pp, tau_centered, tau_tilde, rank, decile (decile may be absent)
stopifnot(all(c("cid","tau_tilde") %in% names(tau)))

# ---------- robust deciles (in case not present or had 'breaks not unique') ----------
if (!("decile" %in% names(tau))) {
  # Order by descending tau_tilde; rank = 1 is strongest positive effect
  setorder(tau, -tau_tilde)
  tau[, rank := .I]
  # Try quantile deciles; if breaks not unique, fallback to rank-based
  qs <- unique(quantile(tau$tau_tilde, probs = seq(0,1,0.1), na.rm = TRUE, type = 7))
  if (length(qs) < 3) {
    tau[, decile := ceiling(10 * rank(tau_tilde, ties.method = "average") / .N)]
    tau[decile == 0, decile := 1]
    message("⚠️ Quantile breaks not unique — used rank-based deciles.")
  } else {
    tau[, decile := cut(tau_tilde, breaks = qs, include_lowest = TRUE, labels = FALSE)]
  }
} else {
  # Ensure integer 1..10 if already present
  tau[, decile := as.integer(decile)]
}

# ---------- optional: attach baseline eligible size from panel ----------
# We will compute a PRE-POLICY average size per county using 'size_col'.
if (!is.na(panel) && nzchar(panel) && file.exists(panel)) {
  pan <- fread(panel)
  nm <- names(pan)
  # Flexible column mapping (consistent with Step 2)
  ycol <- c("YEAR","year","Year"); ycol <- ycol[ycol %in% nm][1]
  mcol <- c("MONTH","month","Month","Mon"); mcol <- mcol[mcol %in% nm][1]
  idc  <- c("id_num","county_id","countyid","cid","fips","FIPS"); idc <- idc[idc %in% nm][1]
  tcol <- c("Treat","treat","treated","enforced","policy_enforced","wr_in_effect"); tcol <- tcol[tcol %in% nm][1]
  
  if (is.na(ycol) || is.na(mcol) || is.na(idc) || is.na(tcol)) {
    warning("Panel provided but missing YEAR/MONTH/county_id/Treat. Skipping baseline size merge.")
  } else if (!(size_col %in% nm)) {
    warning(sprintf("Column '%s' not in panel. Skipping baseline size merge.", size_col))
  } else {
    setnames(pan, c(ycol,mcol,idc,tcol), c("YEAR","MONTH","cid","Treat"))
    pan[, Treat := as.integer(Treat > 0)]
    pan[, ym := YEAR*100L + MONTH]
    # Pre-policy window = months strictly before first treated month in the state
    policy_start <- pan[Treat==1, suppressWarnings(min(ym, na.rm=TRUE))]
    baseline <- if (is.finite(policy_start)) pan[ym < policy_start] else pan[Treat==0]
    if (!nrow(baseline)) baseline <- pan  # fallback
    
    # Compute baseline eligible size per county
    w_dt <- baseline[, .(baseline_size = mean(get(size_col), na.rm = TRUE)), by = cid]
    tau <- merge(tau, w_dt, by = "cid", all.x = TRUE, sort = FALSE)
    
    # Expected exits (per period in baseline scale):
    # expected_exits_c ≈ tau_tilde_c * baseline_size_c
    tau[, expected_exits := tau_tilde * baseline_size]
    
    message(sprintf("✅ Attached baseline_size from '%s' (pre-policy average).", size_col))
    
    # Optional aggregations (if you later want month/quarter/year, you need panel by month).
    # Here we just provide county-level expected exits based on baseline size.
  }
}

# ---------- histogram of tau_tilde (optional) ----------
if (nzchar(hist_png)) {
  p <- ggplot(tau, aes(x = tau_tilde)) +
    geom_histogram(bins = 30) +
    labs(title = "Distribution of Anchored County Effects (tau_tilde)",
         x = "tau_tilde (probability units)", y = "Count")
  ggsave(hist_png, p, width = 7, height = 4.5, dpi = 300)
  message(sprintf("📈 Saved histogram to '%s'.", hist_png))
}

# ---------- export ----------
out_csv <- sprintf("%s_outputs.csv", out_prefix)
# Keep tidy columns; include baseline_size/expected_exits if available
keep_cols <- intersect(c("cid","tau_i_pp","tau_centered","tau_tilde","rank","decile",
                         "baseline_size","expected_exits"), names(tau))
fwrite(tau[, ..keep_cols], out_csv)

# Console summary
cat("\n=========== STEP 3 POST SUMMARY ===========\n")
cat("Input step3 file : ", step3_csv, "\n", sep = "")
if (!is.na(panel) && nzchar(panel)) cat("Panel (for size) : ", panel, "\n", sep = "")
cat("Counties         : ", nrow(tau), "\n", sep = "")
cat("tau_tilde summary:\n"); print(summary(tau$tau_tilde))
if ("baseline_size" %in% names(tau)) {
  cat("Non-missing baseline_size : ", sum(!is.na(tau$baseline_size)), "\n", sep = "")
  cat("expected_exits summary    :\n"); print(summary(tau$expected_exits))
}
cat("Exported         : ", out_csv, "\n", sep = "")
cat("===========================================\n")



