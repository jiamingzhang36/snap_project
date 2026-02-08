############################################################
# STEP 2: Double Machine Learning + Orthogonal BLP
# Treatment Effect Heterogeneity Analysis
############################################################
# PURPOSE:
#   Estimate county-level treatment effect heterogeneity using:
#   1. Cross-fitted nuisance models (outcome & propensity) with LASSO
#   2. Orthogonal R-learner signals to remove confounding bias
#   3. Best Linear Projection (BLP) of CATE on county characteristics
#   4. Two-way clustered standard errors (county × month)
#
# KEY DESIGN:
#   - Uses Step 1 ATT as baseline (no redundant estimation)
#   - BLP estimates DEVIATIONS from Step 1 baseline
#   - Final county effects = Step1_ATT + BLP_deviation
#
# OUTPUTS:
#   - step2_output.rds: Complete results for Step 3
#   - step2_county_effects.csv: County-level treatment effects
#   - step2_blp_coefs.csv: BLP regression coefficients with SEs
############################################################

suppressPackageStartupMessages({
  library(data.table)
  library(glmnet)
  library(fixest)
  library(broom)
})

# -------------------- Command-line Arguments --------------------
arg_map <- list()
if (length(commandArgs(TRUE))) {
  for (kv in strsplit(commandArgs(TRUE), "=", fixed=TRUE)) {
    if (length(kv)==2 && nzchar(kv[[1]])) arg_map[[kv[[1]]]] <- kv[[2]]
  }
}

get_arg <- function(key, default=NULL) {
  if (!is.null(arg_map[[key]]) && nzchar(arg_map[[key]])) return(arg_map[[key]])
  default
}

parse_num <- function(x, default) {
  if (is.null(x) || !nzchar(x)) return(default)
  as.numeric(x)
}

parse_bool <- function(x, default=FALSE) {
  if (is.null(x) || !nzchar(x)) return(default)
  tolower(x) %in% c("1","true","t","yes","y")
}

# Default parameters
input_path       <- path.expand(get_arg("input", "data_clean/snap_laus_with_acs_final_clean.csv.gz"))
step1_results    <- path.expand(get_arg("step1", "outputs/step1_did/robustness_complete.csv"))
K                <- as.integer(parse_num(get_arg("folds", "5"), 5))
overlap_lower    <- parse_num(get_arg("overlap_lower", "0.05"), 0.05)
overlap_upper    <- parse_num(get_arg("overlap_upper", "0.95"), 0.95)
trim_overlap     <- parse_bool(get_arg("trim_overlap", "FALSE"), FALSE)

# Create output directory
outdir <- "outputs/step2_dml"
if (!dir.exists(outdir)) dir.create(outdir, recursive=TRUE)

############################################################
# 1. LOAD STEP 1 BASELINE (CS-DiD ATT)
############################################################

cat("\n", strrep("=", 60), "\n")
cat("STEP 1: LOADING BASELINE ATT FROM CS-DiD\n")
cat(strrep("=", 60), "\n")

# Load Step 1 results table
if (!file.exists(step1_results)) {
  stop("❌ Step 1 results not found at: ", step1_results)
}

step1_df <- fread(step1_results)

# Extract main specification ATT (CS, Anticipation=2)
main_spec <- step1_df[Specification == "Main (CS, Ant=2)"]
if (nrow(main_spec) == 0) {
  stop("❌ Main specification not found in Step 1 results")
}

baseline_att <- main_spec$ATT
baseline_se  <- main_spec$SE

cat(sprintf("✅ Step 1 Baseline ATT: %.4f (SE: %.4f)\n", baseline_att, baseline_se))
cat(sprintf("   95%% CI: [%.4f, %.4f]\n", 
            baseline_att - 1.96*baseline_se, 
            baseline_att + 1.96*baseline_se))
cat(sprintf("   Percentage change: %.2f%%\n", 100*(exp(baseline_att)-1)))
cat("\n   Interpretation: This is the AVERAGE treatment effect across all counties.\n")
cat("   Step 2 will estimate how each county DEVIATES from this average.\n")

############################################################
# 2. LOAD DATA
############################################################

cat("\n", strrep("=", 60), "\n")
cat("STEP 2: LOADING PANEL DATA\n")
cat(strrep("=", 60), "\n")

dt <- fread(input_path)
cat("✅ Loaded data:", nrow(dt), "rows,", ncol(dt), "columns\n")

# -------------------- Identify Key Columns --------------------
first_col <- function(nm, cand) cand[cand %in% nm][1]
nm <- names(dt)

# Year and Month columns
ycol <- first_col(nm, c("YEAR","year","Year"))
mcol <- first_col(nm, c("MONTH","month","Month"))
stopifnot("Need YEAR column" = !is.na(ycol))
stopifnot("Need MONTH column" = !is.na(mcol))
setnames(dt, c(ycol, mcol), c("YEAR","MONTH"))

# County identifier
id_col <- first_col(nm, c("id_num","county_id","countyid","fips","FIPS"))
stopifnot("Need county ID column" = !is.na(id_col))
setnames(dt, id_col, "cid")

# Treatment indicator
treatc <- first_col(nm, c("Treat","treat","enforced","policy_enforced","wr_in_effect"))
stopifnot("Need treatment indicator" = !is.na(treatc))
setnames(dt, treatc, "Treat")
dt[, Treat := as.integer(Treat > 0)]

# SNAP cases/recipients count
casec <- first_col(nm, c("cases","Cases","recip","recipients"))
stopifnot("Need cases/recipients count" = !is.na(casec))
setnames(dt, casec, "cases")

# Total households (optional)
hhc <- first_col(nm, c("total_households","households","hh_total"))
has_hh <- !is.na(hhc)
if (has_hh) setnames(dt, hhc, "total_households")

# Create time identifiers
dt[, YM := sprintf("%04d-%02d", YEAR, MONTH)]
dt[, ym := YEAR*100L + MONTH]

############################################################
# 3. CONSTRUCT OUTCOME VARIABLE
############################################################

cat("\n", strrep("=", 60), "\n")
cat("STEP 3: CONSTRUCTING OUTCOME VARIABLE\n")
cat(strrep("=", 60), "\n")

# Use log transformation for interpretability (% changes)
if (has_hh) {
  dt[, cases_per_1k_hh := 1000 * cases / pmax(total_households, 1)]
  dt[, Y := log1p(cases_per_1k_hh)]
  cat("✅ Outcome: Y = log(1 + cases_per_1k_hh)\n")
} else {
  dt[, Y := log1p(cases)]
  cat("⚠️  No total_households found; using Y = log(1 + cases)\n")
}

############################################################
# 4. DEFINE MODERATORS (County Characteristics)
############################################################

cat("\n", strrep("=", 60), "\n")
cat("STEP 4: DEFINING MODERATOR VARIABLES\n")
cat(strrep("=", 60), "\n")

# Moderators: pre-treatment county characteristics that interact with treatment
mods <- c(
  # Demographics
  "pct_male_final",
  "pct_age_65plus",
  "median_age_final",
  
  # Race/Ethnicity
  "pct_black_final",
  "pct_asian_final",
  "pct_hispanic_final",
  
  # Education & Language
  "pct_bachelors_plus",
  "pct_foreign_born",
  "pct_non_english_home",
  
  # Household Structure
  "pct_married_hh",
  "pct_single_parent",
  
  # Labor Market
  "lfpr",
  "emp_rate",
  
  # Economic Conditions
  "income_avg",
  "pov_le_130fpl_share",
  
  # Housing
  "homeownership_rate",
  "vacancy_rate",
  "median_home_value",
  "median_rent_final",
  "pct_rent_burdened_final"
)

# Create derived variables
if (!"unemp_rate" %in% names(dt) && "emp_rate" %in% names(dt)) {
  dt[, unemp_rate := 1 - emp_rate]
}
if (!"log_income" %in% names(dt) && "income_avg" %in% names(dt)) {
  dt[, log_income := log1p(income_avg)]
}
if (!"log_rent" %in% names(dt) && "median_rent_final" %in% names(dt)) {
  dt[, log_rent := log1p(median_rent_final)]
}
if (!"log_home" %in% names(dt) && "median_home_value" %in% names(dt)) {
  dt[, log_home := log1p(median_home_value)]
}

# Keep only moderators that exist in dataset
mods <- mods[mods %in% names(dt)]
stopifnot("Need at least one moderator" = length(mods) > 0)

# Remove redundant variables to avoid perfect collinearity
if ("emp_rate" %in% mods && "unemp_rate" %in% mods) {
  mods <- setdiff(mods, "unemp_rate")
  cat("ℹ️  Kept emp_rate, removed unemp_rate (avoid collinearity)\n")
}

cat("Using", length(mods), "moderators for heterogeneity analysis\n")

############################################################
# 5. BASELINE STANDARDIZATION (NO DATA LEAKAGE)
############################################################

cat("\n", strrep("=", 60), "\n")
cat("STEP 5: STANDARDIZATION (Pre-policy baseline only)\n")
cat(strrep("=", 60), "\n")

# CRITICAL: Standardize using ONLY pre-policy data to avoid leakage
# This ensures treatment information doesn't contaminate the scaling

# Find when treatment first occurs
policy_start <- dt[Treat==1, suppressWarnings(min(ym, na.rm=TRUE))]

# Define baseline period: all observations before treatment starts
if (is.finite(policy_start)) {
  baseline <- dt[ym < policy_start]
  cat("Policy starts:", policy_start, "\n")
  cat("Baseline period: n =", nrow(baseline), "observations\n")
} else {
  # Fallback: use untreated observations
  baseline <- dt[Treat == 0]
  cat("⚠️  No clear policy start; using Treat==0 as baseline (n =", 
      nrow(baseline), ")\n")
}

if (nrow(baseline) == 0) {
  stop("❌ No baseline data available for standardization")
}

# Compute baseline statistics (mean and SD) for each moderator
baseline_stats <- list()
for (mod in mods) {
  mu <- mean(baseline[[mod]], na.rm=TRUE)
  sigma <- sd(baseline[[mod]], na.rm=TRUE)
  
  # Handle edge cases
  if (!is.finite(mu)) mu <- 0
  if (!is.finite(sigma) || sigma < 1e-10) sigma <- 1
  
  baseline_stats[[mod]] <- list(mean=mu, sd=sigma)
}

# Apply standardization to FULL dataset using baseline parameters
for (mod in mods) {
  mu <- baseline_stats[[mod]]$mean
  sigma <- baseline_stats[[mod]]$sd
  dt[, (mod) := (get(mod) - mu) / sigma]
}

cat("✅ Standardization complete (mean≈0, sd≈1 in baseline)\n")

############################################################
# 6. K-FOLD CROSS-FITTING SETUP
############################################################

cat("\n", strrep("=", 60), "\n")
cat("STEP 6: K-FOLD CROSS-FITTING SETUP\n")
cat(strrep("=", 60), "\n")

# Cross-fitting prevents overfitting in nuisance models
# We fold by COUNTY (not observation) to avoid temporal leakage

set.seed(42)
uids <- unique(dt$cid)
fold_ids <- sample(rep(seq_len(K), length.out=length(uids)))
fold_map <- data.table(cid=uids, fold=fold_ids)
dt <- merge(dt, fold_map, by="cid", all.x=TRUE, sort=FALSE)

cat("✅ K-fold cross-fitting setup (K =", K, ") by county\n")

# Check fold balance
fold_balance <- dt[, .(
  n_obs = .N,
  n_counties = uniqueN(cid),
  n_treated = sum(Treat),
  pct_treated = 100*mean(Treat)
), by=fold]
cat("\nFold balance:\n")
print(fold_balance)

############################################################
# 7. CROSS-FITTED LASSO NUISANCE MODELS
############################################################

cat("\n", strrep("=", 60), "\n")
cat("STEP 7: CROSS-FITTED LASSO MODELS\n")
cat(strrep("=", 60), "\n")

# We need two nuisance models for DML:
# 1. Outcome model: m(X) = E[Y | X]
# 2. Propensity model: e(X) = E[D | X]

# Utility: Create model matrix
mmx <- function(D, vars){
  X <- as.matrix(D[, ..vars])
  # Keep only columns with variance and finite values
  keep <- which(apply(X, 2, function(z){
    z <- z[is.finite(z)]
    sd(z) > 0 && length(z) > 1
  }))
  if (!length(keep)) stop("All features constant or non-finite")
  X[, keep, drop=FALSE]
}

# Initialize prediction columns
dt[, c("m_hat","e_hat") := .(NA_real_, NA_real_)]

need <- c(mods, "Y", "Treat")

for (k in seq_len(K)) {
  # Split: train on folds ≠ k, test on fold k
  tr <- which(dt$fold != k & complete.cases(dt[, ..need]))
  te <- which(dt$fold == k & complete.cases(dt[, ..need]))
  if (!length(te)) next
  
  cat("\nFold", k, "| Train n =", length(tr), "| Test n =", length(te), "\n")
  
  Tr <- dt[tr]; Te <- dt[te]
  xtr <- mmx(Tr, mods)
  xte <- mmx(Te, mods)
  
  # Ensure train and test have same columns
  common <- intersect(colnames(xtr), colnames(xte))
  xtr <- xtr[, common, drop=FALSE]
  xte <- xte[, common, drop=FALSE]
  
  # 1. OUTCOME MODEL: Y ~ X
  cvy <- cv.glmnet(xtr, Tr$Y, family="gaussian", alpha=1, nfolds=5)
  mhat <- as.numeric(predict(cvy, xte, s="lambda.min"))
  
  # 2. PROPENSITY MODEL: D ~ X
  cvd <- cv.glmnet(xtr, Tr$Treat, family="binomial", alpha=1, nfolds=5)
  ehat <- as.numeric(predict(cvd, xte, s="lambda.min", type="response"))
  
  # Store predictions
  dt[te, m_hat := mhat]
  dt[te, e_hat := ehat]
}

############################################################
# 8. OVERLAP PROTECTION
############################################################

cat("\n", strrep("=", 60), "\n")
cat("STEP 8: OVERLAP PROTECTION\n")
cat(strrep("=", 60), "\n")

# Clip propensity scores to avoid extreme weights
dt[, e_hat := pmin(pmax(e_hat, overlap_lower), overlap_upper)]

# Optional: Trim observations with extreme propensity scores
if (trim_overlap) {
  before <- nrow(dt)
  dt <- dt[e_hat > overlap_lower & e_hat < overlap_upper]
  cat("✂️  Trimmed", before-nrow(dt), "rows with extreme propensity scores\n")
}

cat("✅ Cross-fitting complete\n")

# Diagnose overlap quality
cat("\nPropensity score distribution by treatment status:\n")
overlap_diag <- dt[!is.na(e_hat), .(
  min = min(e_hat),
  p25 = quantile(e_hat, 0.25),
  median = median(e_hat),
  p75 = quantile(e_hat, 0.75),
  max = max(e_hat)
), by=Treat]
print(overlap_diag)

############################################################
# 9. ORTHOGONAL R-LEARNER SIGNALS
############################################################

cat("\n", strrep("=", 60), "\n")
cat("STEP 9: ORTHOGONAL SIGNALS\n")
cat(strrep("=", 60), "\n")

# Compute orthogonal score (removes confounding bias)
# Formula: S_i = (D_i - e(X_i)) * (Y_i - m(X_i))
# This is the standard DML "partialling out" score

dt[, S := (Treat - e_hat) * (Y - m_hat)]

# Winsorize at 1% and 99% to handle outliers
qL <- quantile(dt$S, 0.01, na.rm=TRUE)
qU <- quantile(dt$S, 0.99, na.rm=TRUE)
dt[, S_w := pmin(pmax(S, qL), qU)]

cat("Original signal S:\n")
print(summary(dt$S))
cat("\nWinsorized signal S_w (clipped at 1%/99%):\n")
print(summary(dt$S_w))

# Compute propensity variance for weighting
dt[, v_hat := e_hat * (1-e_hat)]

############################################################
# 10. ORTHOGONAL BLP REGRESSION
############################################################

cat("\n", strrep("=", 60), "\n")
cat("STEP 10: ORTHOGONAL BLP - HETEROGENEITY ANALYSIS\n")
cat(strrep("=", 60), "\n")

# Best Linear Projection: approximate CATE(X) ≈ α + X'β
# Regression: S_w ~ X, weighted by v_hat
# Output: β captures how county characteristics modify treatment effects

form_blp <- as.formula(paste("S_w ~", paste(mods, collapse=" + ")))

blp_fit <- feols(
  fml = form_blp,
  data = dt,
  weights = ~v_hat,
  cluster = ~cid + YM
)

cat("\nTwo-way clustered standard errors (county × month):\n")
cat("Interpretation: β̂ = effect of 1-SD change in moderator on treatment effect\n")
cat("                (relative to Step 1 baseline ATT)\n\n")
print(summary(blp_fit))

# Extract coefficient table
blp_coef_table <- broom::tidy(blp_fit)
blp_coef_table <- blp_coef_table[order(-abs(blp_coef_table$statistic)), ]

cat("\nBLP Coefficients (sorted by |t-statistic|):\n")
print(blp_coef_table[, c("term","estimate","std.error","statistic","p.value")])

# Identify significant moderators
sig_mods <- blp_coef_table[blp_coef_table$p.value < 0.05 & 
                             blp_coef_table$term != "(Intercept)", ]
if (nrow(sig_mods) > 0) {
  cat("\n✅ Significant moderators (p < 0.05):\n")
  print(sig_mods[, c("term","estimate","p.value")])
} else {
  cat("\n⚠️  No significant moderators detected\n")
}

############################################################
# 11. COUNTY-LEVEL TREATMENT EFFECTS
############################################################

cat("\n", strrep("=", 60), "\n")
cat("STEP 11: COUNTY-LEVEL TREATMENT EFFECTS\n")
cat(strrep("=", 60), "\n")

# Compute county-level means of standardized moderators
X_means <- dt[, lapply(.SD, mean, na.rm=TRUE), .SDcols=mods, by=cid]

# Extract BLP coefficients
beta_hat <- coef(blp_fit)
intercept <- beta_hat["(Intercept)"]
beta_mods <- beta_hat[mods]

# Predict deviation from baseline using BLP
X_means[, tau_i_deviation := intercept + as.matrix(.SD) %*% beta_mods, .SDcols=mods]

# ⭐ KEY: Add Step 1 baseline to get absolute county-level effects
X_means[, tau_i_total := baseline_att + tau_i_deviation]

# Convert to percentage change
X_means[, pct_change := 100 * (exp(tau_i_total) - 1)]

cat("\nCounty-level treatment effects (anchored to Step 1 baseline):\n")
cat(sprintf("  Baseline ATT: %.4f (%.2f%%)\n", baseline_att, 100*(exp(baseline_att)-1)))
print(summary(X_means[, .(tau_i_deviation, tau_i_total, pct_change)]))

cat("\n📊 Counties with LARGEST negative effects:\n")
print(X_means[order(tau_i_total)][1:5, .(cid, tau_i_total, pct_change)])

cat("\n📊 Counties with SMALLEST negative effects:\n")
print(X_means[order(-tau_i_total)][1:5, .(cid, tau_i_total, pct_change)])

############################################################
# 12. SAVE RESULTS FOR STEP 3
############################################################

cat("\n", strrep("=", 60), "\n")
cat("STEP 12: SAVING OUTPUTS\n")
cat(strrep("=", 60), "\n")

# Package all components for Step 3
step2_output <- list(
  # BLP model components
  blp_coef = coef(blp_fit),
  vcov_blp = vcov(blp_fit),
  moderators = mods,
  
  # County-level predictions
  X_means = X_means,  # Contains tau_i_deviation and tau_i_total
  
  # ⭐ Step 1 baseline (NOT re-estimated)
  baseline_att = baseline_att,
  baseline_se  = baseline_se,
  step1_source = "CS-DiD (Ant=2)",
  
  # Standardization parameters (for new data in Step 3)
  baseline_stats = baseline_stats,
  
  # Metadata
  overlap_bounds = c(lower=overlap_lower, upper=overlap_upper),
  n_obs = nrow(dt),
  n_counties = uniqueN(dt$cid),
  date_created = Sys.time()
)

# Save main output (R object for Step 3)
saveRDS(step2_output, file.path(outdir, "step2_output.rds"))
cat("✅ Saved: step2_output.rds (complete results for Step 3)\n")

# Save county effects as CSV (for inspection)
fwrite(X_means, file.path(outdir, "step2_county_effects.csv"))
cat("✅ Saved: step2_county_effects.csv\n")

# Save BLP coefficients as CSV (for reporting)
fwrite(blp_coef_table, file.path(outdir, "step2_blp_coefs.csv"))
cat("✅ Saved: step2_blp_coefs.csv\n")

############################################################
# 13. INTERPRETATION GUIDE
############################################################

cat("\n", strrep("=", 60), "\n")
cat("⚠️  HOW TO INTERPRET THESE RESULTS\n")
cat(strrep("=", 60), "\n")

cat("\n1. WHAT WE ESTIMATED:\n")
cat("   • Step 1 (CS-DiD): Average treatment effect across all counties\n")
cat("   • Step 2 (BLP):    How each county DEVIATES from this average\n")

cat("\n2. COUNTY-LEVEL EFFECTS:\n")
cat("   • tau_i_deviation: BLP prediction (relative to average)\n")
cat("   • tau_i_total:     Absolute effect = baseline_att + tau_i_deviation\n")
cat("   • pct_change:      Effect in percentage terms\n")

cat("\n3. EXAMPLE INTERPRETATION:\n")
cat(sprintf("   County A: tau_i_total = %.4f → %.2f%% change in SNAP caseload\n",
            X_means[1, tau_i_total], X_means[1, pct_change]))

cat("\n4. FOR STEP 3 (Forecasting):\n")
cat("   • Use tau_i_total for county-specific predictions\n")
cat("   • Apply exp(tau_i_total) - 1 to get multiplicative effect\n")

cat("\n", strrep("=", 60), "\n")
cat("✅ STEP 2 COMPLETE\n")
cat(strrep("=", 60), "\n\n")




