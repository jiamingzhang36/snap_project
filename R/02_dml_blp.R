

# ============================================================
# STEP 2: Double Machine Learning + Orthogonal BLP
# ============================================================
# PURPOSE:
#   Estimate county-level treatment effect heterogeneity using:
#   1. Cross-fitted nuisance models (outcome & propensity) with LASSO
#   2. Orthogonal R-learner signals to remove confounding bias
#   3. Best Linear Projection (BLP) of CATE on county characteristics
#   4. Two-way clustered standard errors (county × month)
#
# OUTPUTS:
#   - step2_output.rds: Complete results for Step 3 (coefficients, vcov, ATE, standardization params)
#   - step2_county_effects.csv: County-level treatment effect indices
#   - step2_blp_coefs.csv: BLP regression coefficients with SEs
#
# KEY FEATURES:
#   - NO DATA LEAKAGE: Standardization uses pre-policy baseline only
#   - CROSS-FITTING: K-fold CV prevents overfitting in nuisance models
#   - OVERLAP PROTECTION: Clips propensity scores to [0.05, 0.95]
#   - ATE ESTIMATION: Provides anchor point for Step 3 forecasting
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(glmnet)
  library(fixest)
  library(broom)
})

# -------------------- Command-line Arguments --------------------
# Allows flexible input paths and hyperparameters via command line
# Example: Rscript step2.R input=/path/to/data.csv folds=10 overlap_lower=0.1

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
input_path       <- path.expand(get_arg("input", "~/Desktop/snap_project/data_clean/snap_laus_with_acs_final_clean.csv.gz"))
K                <- as.integer(parse_num(get_arg("folds", "5"), 5))
overlap_lower    <- parse_num(get_arg("overlap_lower", "0.05"), 0.05)
overlap_upper    <- parse_num(get_arg("overlap_upper", "0.95"), 0.95)
trim_overlap     <- parse_bool(get_arg("trim_overlap", "FALSE"), FALSE)

# -------------------- Load Data --------------------
dt <- fread(input_path)
cat("✅ Loaded data:", nrow(dt), "rows,", ncol(dt), "columns\n")

# -------------------- Identify Key Columns (Flexible Column Names) --------------------
# Automatically detects column names to handle different naming conventions
# This makes the script robust to variations in your dataset structure

first_col <- function(nm, cand) cand[cand %in% nm][1]
nm <- names(dt)

# Year and Month columns (required for time fixed effects)
ycol <- first_col(nm, c("YEAR","year","Year"))
mcol <- first_col(nm, c("MONTH","month","Month"))
stopifnot("Need YEAR column" = !is.na(ycol))
stopifnot("Need MONTH column" = !is.na(mcol))
setnames(dt, c(ycol, mcol), c("YEAR","MONTH"))

# County identifier (required for county fixed effects and clustering)
id_col <- first_col(nm, c("id_num","county_id","countyid","fips","FIPS"))
stopifnot("Need county ID column" = !is.na(id_col))
setnames(dt, id_col, "cid")

# Treatment indicator (binary: 0/1 for policy exposure)
treatc <- first_col(nm, c("Treat","treat","enforced","policy_enforced","wr_in_effect"))
stopifnot("Need treatment indicator" = !is.na(treatc))
setnames(dt, treatc, "Treat")
dt[, Treat := as.integer(Treat > 0)]

# SNAP cases/recipients count
casec <- first_col(nm, c("cases","Cases","recip","recipients"))
stopifnot("Need cases/recipients count" = !is.na(casec))
setnames(dt, casec, "cases")

# Total households (optional, improves outcome measurement)
hhc <- first_col(nm, c("total_households","households","hh_total"))
has_hh <- !is.na(hhc)
if (has_hh) setnames(dt, hhc, "total_households")

# Create year-month identifiers for clustering and time fixed effects
dt[, YM := sprintf("%04d-%02d", YEAR, MONTH)]
dt[, ym := YEAR*100L + MONTH]  # Numeric format for comparisons

# -------------------- Construct Outcome Variable Y --------------------
# We use log transformation for two reasons:
# 1. Stabilizes variance (many counties have very different scales)
# 2. Makes treatment effects interpretable as % changes
# Formula: Y = log(1 + cases_per_1k_hh) or log(1 + cases)

if (has_hh) {
  dt[, cases_per_1k_hh := 1000 * cases / pmax(total_households, 1)]
  dt[, Y := log1p(cases_per_1k_hh)]
  cat("✅ Outcome: Y = log(1 + cases_per_1k_hh)\n")
} else {
  dt[, Y := log1p(cases)]
  cat("⚠️  No total_households found; using Y = log(1 + cases)\n")
}

# -------------------- Define Moderators (County Characteristics) --------------------
# These are the X variables that interact with treatment to create heterogeneity
# They should be:
# - Pre-treatment or slow-moving characteristics
# - Theoretically relevant for program take-up/exit
# - Not affected by the treatment itself (to avoid post-treatment bias)

mods <- c(
  # Demographics
  "pct_male_final",           # Gender composition
  "pct_age_65plus",           # Elderly population share
  "median_age_final",         # Age structure
  
  # Race/Ethnicity
  "pct_black_final",          # Black population share
  "pct_asian_final",          # Asian population share
  "pct_hispanic_final",       # Hispanic population share
  
  # Education & Language
  "pct_bachelors_plus",       # College education rate
  "pct_foreign_born",         # Immigrant share
  "pct_non_english_home",     # Limited English proficiency proxy
  
  # Household Structure
  "pct_married_hh",           # Married household share
  "pct_single_parent",        # Single-parent household share
  
  # Labor Market
  "lfpr",                     # Labor force participation rate
  "emp_rate",                 # Employment rate
  
  # Economic Conditions
  "income_avg",               # Average income
  "pov_le_130fpl_share",      # Poverty rate (≤130% FPL)
  
  # Housing
  "homeownership_rate",       # Homeownership rate
  "vacancy_rate",             # Housing vacancy rate
  "median_home_value",        # Median home value
  "median_rent_final",        # Median rent
  "pct_rent_burdened_final"   # Rent-burdened households (>30% income)
)

# Create derived variables for better model fit
# Log transformations help with skewed distributions
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

# Keep only moderators that exist in the dataset
mods <- mods[mods %in% names(dt)]
stopifnot("Need at least one moderator" = length(mods) > 0)

# Remove redundant variables to avoid perfect collinearity
# emp_rate and unemp_rate are perfectly correlated (unemp = 1 - emp)
if ("emp_rate" %in% mods && "unemp_rate" %in% mods) {
  mods <- setdiff(mods, "unemp_rate")
  cat("ℹ️  Kept emp_rate, removed unemp_rate (avoid perfect collinearity)\n")
}

cat("Using", length(mods), "moderators for heterogeneity analysis\n")

# -------------------- Baseline Standardization (NO DATA LEAKAGE) --------------------
# CRITICAL: We standardize using ONLY pre-policy data to avoid leakage
# 
# Why this matters:
# - If we standardize using the full sample (including post-treatment), 
#   information from treated periods "leaks" into the training process
# - This violates the cross-fitting logic and can bias estimates
#
# Our approach:
# 1. Identify pre-policy period (before any treatment starts)
# 2. Calculate mean & SD using ONLY pre-policy observations
# 3. Apply these fixed parameters to standardize the FULL dataset
# 4. This ensures test folds don't contaminate training folds

cat("\n", strrep("=", 60), "\n")
cat("STANDARDIZATION: Pre-policy baseline (no leakage)\n")
cat(strrep("=", 60), "\n")

# Find when treatment first occurs
policy_start <- dt[Treat==1, suppressWarnings(min(ym, na.rm=TRUE))]

# Define baseline period: all observations before treatment starts
if (is.finite(policy_start)) {
  baseline <- dt[ym < policy_start]
  cat("Policy starts:", policy_start, "\n")
  cat("Baseline period: n =", nrow(baseline), "observations\n")
} else {
  # Fallback: if no clear policy start, use all untreated observations
  baseline <- dt[Treat == 0]
  cat("⚠️  No clear policy start; using Treat==0 as baseline (n =", 
      nrow(baseline), ")\n")
}

# Safety check: ensure we have baseline data
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
  if (!is.finite(sigma) || sigma < 1e-10) sigma <- 1  # Avoid division by zero
  
  baseline_stats[[mod]] <- list(mean=mu, sd=sigma)
}

# Apply standardization to FULL dataset using baseline parameters
# Formula: z = (x - μ_baseline) / σ_baseline
for (mod in mods) {
  mu <- baseline_stats[[mod]]$mean
  sigma <- baseline_stats[[mod]]$sd
  dt[, (mod) := (get(mod) - mu) / sigma]
}

cat("✅ Standardization complete (mean≈0, sd≈1)\n")
cat("First 3 moderators after standardization:\n")
print(dt[1:3, head(mods, 3), with=FALSE])

# -------------------- K-Fold Cross-Fitting Setup --------------------
# Cross-fitting prevents overfitting in nuisance models (m_hat, e_hat)
# 
# Key design choice: FOLD BY COUNTY, not by observation
# - If we fold by observation, the same county appears in both train and test
# - This creates temporal leakage (future predicts past within same county)
# - By folding at county level, we ensure no county appears in both sets
#
# Steps:
# 1. Randomly assign each county to one of K folds
# 2. For fold k: train on counties NOT in fold k, predict for counties IN fold k
# 3. This gives us out-of-sample predictions for ALL observations

set.seed(42)  # Reproducibility
uids <- unique(dt$cid)
fold_ids <- sample(rep(seq_len(K), length.out=length(uids)))
fold_map <- data.table(cid=uids, fold=fold_ids)
dt <- merge(dt, fold_map, by="cid", all.x=TRUE, sort=FALSE)

cat("\n✅ K-fold cross-fitting setup (K =", K, ") by county\n")

# Check fold balance
fold_balance <- dt[, .(
  n_obs = .N,
  n_counties = uniqueN(cid),
  n_treated = sum(Treat),
  pct_treated = 100*mean(Treat)
), by=fold]
cat("\nFold balance:\n")
print(fold_balance)

# -------------------- Cross-Fitted Nuisance Models (LASSO) --------------------
# We need two nuisance models for DML:
# 1. Outcome model: m(X) = E[Y | X] - predicts outcome given covariates
# 2. Propensity model: e(X) = E[D | X] - predicts treatment given covariates
#
# Why LASSO?
# - Handles high-dimensional X (many moderators)
# - Automatic variable selection
# - Prevents overfitting via L1 penalty
# - Cross-validation chooses optimal penalty (lambda)
#
# Cross-fitting procedure:
# For each fold k:
#   1. Train both models on folds ≠ k
#   2. Predict for observations in fold k
#   3. Store out-of-sample predictions (m_hat, e_hat)

# Utility function: Create model matrix and remove constant/non-finite columns
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

cat("\n", strrep("=", 60), "\n")
cat("CROSS-FITTED LASSO MODELS\n")
cat(strrep("=", 60), "\n")

need <- c(mods, "Y", "Treat")  # Required variables

for (k in seq_len(K)) {
  # Split: train on folds ≠ k, test on fold k
  tr <- which(dt$fold != k & complete.cases(dt[, ..need]))
  te <- which(dt$fold == k & complete.cases(dt[, ..need]))
  if (!length(te)) next  # Skip if test set is empty
  
  cat("\nFold", k, "| Train n =", length(tr), "| Test n =", length(te), "\n")
  
  Tr <- dt[tr]; Te <- dt[te]
  xtr <- mmx(Tr, mods)
  xte <- mmx(Te, mods)
  
  # Ensure train and test have same columns (required by glmnet)
  common <- intersect(colnames(xtr), colnames(xte))
  xtr <- xtr[, common, drop=FALSE]
  xte <- xte[, common, drop=FALSE]
  
  # 1. OUTCOME MODEL: Y ~ X (Gaussian family for continuous outcome)
  cvy <- cv.glmnet(xtr, Tr$Y, family="gaussian", alpha=1, nfolds=5)
  mhat <- as.numeric(predict(cvy, xte, s="lambda.min"))
  
  # 2. PROPENSITY MODEL: D ~ X (Binomial family for binary treatment)
  cvd <- cv.glmnet(xtr, Tr$Treat, family="binomial", alpha=1, nfolds=5)
  ehat <- as.numeric(predict(cvd, xte, s="lambda.min", type="response"))
  
  # Store predictions
  dt[te, m_hat := mhat]
  dt[te, e_hat := ehat]
}

# -------------------- Overlap Protection --------------------
# Propensity scores near 0 or 1 indicate weak overlap (positivity violation)
# - e_hat ≈ 0: units almost never treated (can't estimate treatment effect)
# - e_hat ≈ 1: units almost always treated (can't estimate control outcome)
#
# Solution: Clip propensity scores to [overlap_lower, overlap_upper]
# Default: [0.05, 0.95] ensures we have at least 5% probability of both treatment states

dt[, e_hat := pmin(pmax(e_hat, overlap_lower), overlap_upper)]

# Optional: Trim observations with extreme propensity scores
# (Usually not needed if clipping is sufficient, but useful for sensitivity analysis)
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

# Flag potential overlap issues
weak_overlap <- dt[!is.na(e_hat), .(
  n_extreme = sum(e_hat < 0.1 | e_hat > 0.9),
  pct_extreme = 100 * mean(e_hat < 0.1 | e_hat > 0.9)
)]
if (weak_overlap$pct_extreme > 10) {
  cat("⚠️  Warning:", round(weak_overlap$pct_extreme, 1), 
      "% observations have extreme propensity scores (consider trimming)\n")
}

# -------------------- Orthogonal R-Learner Signals --------------------
# The orthogonal score removes confounding bias via Neyman orthogonality
# 
# Formula: S_i = (D_i - e(X_i)) * (Y_i - m(X_i)) / Var(D_i | X_i)
# 
# Intuition:
# - (D_i - e(X_i)): Residualized treatment (removes confounding from X)
# - (Y_i - m(X_i)): Residualized outcome (removes direct effect of X on Y)
# - Divide by Var(D|X): Stabilizes the variance (downweights low-overlap regions)
# - S_i is now approximately mean-zero and unbiased for CATE
#
# Winsorization: Clip extreme values at 1st/99th percentiles
# - Prevents outliers from dominating the BLP regression
# - Common practice in robust machine learning

# Compute propensity variance (used for weighting)
dt[, v_hat := e_hat * (1-e_hat)]

# Compute orthogonal score
dt[, S := (Treat - e_hat) * (Y - m_hat) / pmax(v_hat, 1e-6)]

# Winsorize at 1% and 99% to handle outliers
qL <- quantile(dt$S, 0.01, na.rm=TRUE)
qU <- quantile(dt$S, 0.99, na.rm=TRUE)
dt[, S_w := pmin(pmax(S, qL), qU)]

cat("\n", strrep("=", 60), "\n")
cat("ORTHOGONAL SIGNALS\n")
cat(strrep("=", 60), "\n")
cat("Original signal S:\n")
print(summary(dt$S))
cat("\nWinsorized signal S_w (clipped at 1%/99%):\n")
print(summary(dt$S_w))

# -------------------- Estimate Average Treatment Effect (ATE) --------------------
# The BLP gives us HETEROGENEITY (how effects vary), not the average level
# We need a separate ATE estimate to anchor the heterogeneity index in Step 3
#
# Method: Two-way fixed effects (TWFE) regression
# - Controls for county fixed effects (time-invariant heterogeneity)
# - Controls for month fixed effects (common time trends)
# - Two-way clustering for robust standard errors
#
# Why not use BLP intercept? 
# - BLP is weighted by v_hat, which can bias the average
# - TWFE is more transparent and easier to interpret

cat("\n", strrep("=", 60), "\n")
cat("AVERAGE TREATMENT EFFECT (ATE) - For Step 3 Anchoring\n")
cat(strrep("=", 60), "\n")

ate_model <- feols(
  Y ~ Treat | cid + YM,
  data = dt,
  cluster = ~cid + YM
)

ate_estimate <- coef(ate_model)["Treat"]
ate_se <- se(ate_model)["Treat"]

cat(sprintf("\nATE (log scale): %.4f (SE: %.4f)\n", ate_estimate, ate_se))
cat(sprintf("  95%% CI: [%.4f, %.4f]\n", 
            ate_estimate - 1.96*ate_se, 
            ate_estimate + 1.96*ate_se))
cat(sprintf("\nATE (%% change): %.2f%%\n", 100*(exp(ate_estimate)-1)))
cat(sprintf("  Interpretation: Policy changes SNAP caseload by %.2f%% on average\n",
            100*(exp(ate_estimate)-1)))

# -------------------- Orthogonal BLP Regression --------------------
# Best Linear Projection (BLP) approximates the CATE function:
# 
# CATE(X) ≈ α + X'β
# 
# where X are standardized moderators and β captures heterogeneity
#
# Regression: S_w ~ X, weighted by v_hat
# - S_w: Orthogonal signal (unbiased proxy for CATE)
# - X: Standardized county characteristics
# - Weights: v_hat = e(X)(1-e(X)) gives more weight to regions with good overlap
# - Clustering: Two-way (county × month) for robust inference
#
# Output interpretation:
# - β̂_j: 1-SD increase in moderator j changes treatment effect by β̂_j log points
# - Since moderators are standardized, coefficients are directly comparable

cat("\n", strrep("=", 60), "\n")
cat("ORTHOGONAL BLP: Heterogeneity Analysis\n")
cat(strrep("=", 60), "\n")

form_blp <- as.formula(paste("S_w ~", paste(mods, collapse=" + ")))

blp_fit <- feols(
  fml = form_blp,
  data = dt,
  weights = ~v_hat,
  cluster = ~cid + YM
)

cat("\nTwo-way clustered standard errors (county × month):\n")
cat("Interpretation: β̂ = effect of 1-SD change in moderator on treatment effect\n\n")
print(summary(blp_fit))

# Extract coefficient table
blp_coef_table <- broom::tidy(blp_fit)
blp_coef_table <- blp_coef_table[order(-abs(blp_coef_table$statistic)), ]  # Sort by |t-stat|

cat("\nBLP Coefficients (sorted by |t-statistic|):\n")
print(blp_coef_table[, c("term","estimate","std.error","statistic","p.value")])

# Identify significant moderators (p < 0.05)
sig_mods <- blp_coef_table[blp_coef_table$p.value < 0.05 & 
                             blp_coef_table$term != "(Intercept)", ]
if (nrow(sig_mods) > 0) {
  cat("\n✅ Significant moderators (p < 0.05):\n")
  print(sig_mods[, c("term","estimate","p.value")])
} else {
  cat("\n⚠️  No significant moderators detected (no strong heterogeneity)\n")
}

# -------------------- County-Level Treatment Effect Index --------------------
# Compute τ̂_i for each county i:
# 
# τ̂_i = α̂ + X̄_i' β̂
# 
# where X̄_i = county-level mean of standardized moderators
#
# This gives us a linear heterogeneity index:
# - Positive τ̂_i: Treatment effect stronger than average in this county
# - Negative τ̂_i: Treatment effect weaker than average in this county
#
# IMPORTANT: τ̂_i is on LOG SCALE (since Y = log(cases))
# - To convert to % change: 100 × (exp(τ̂_i) - 1)
# - This is a RELATIVE index; need to anchor around ATE in Step 3

cat("\n", strrep("=", 60), "\n")
cat("COUNTY-LEVEL TREATMENT EFFECT INDEX\n")
cat(strrep("=", 60), "\n")

# Compute county-level means of standardized moderators
X_means <- dt[, lapply(.SD, mean, na.rm=TRUE), .SDcols=mods, by=cid]

# Predict τ̂_i using BLP coefficients
# Using predict() avoids manual matrix multiplication and ensures correct alignment
tau_i_pp <- as.numeric(predict(blp_fit, newdata=X_means, type="link"))
X_means[, tau_i_pp := tau_i_pp]

# Convert to percentage change for easier interpretation
X_means[, pct_change := 100 * (exp(tau_i_pp) - 1)]

cat("\nCounty-level tau_i_pp (log scale):\n")
print(summary(X_means$tau_i_pp))

cat("\nCounty-level treatment effects (% change):\n")
print(summary(X_means$pct_change))

cat("\nDistribution of heterogeneity:\n")
cat(sprintf("  Range: [%.2f%%, %.2f%%]\n", 
            min(X_means$pct_change, na.rm=TRUE),
            max(X_means$pct_change, na.rm=TRUE)))
cat(sprintf("  IQR: [%.2f%%, %.2f%%]\n",
            quantile(X_means$pct_change, 0.25, na.rm=TRUE),
            quantile(X_means$pct_change, 0.75, na.rm=TRUE)))

# Show counties with largest effects (both positive and negative)
cat("\n📊 Top 5 counties (largest effects in magnitude):\n")
print(X_means[order(-abs(tau_i_pp))][1:5, .(cid, tau_i_pp, pct_change)])

cat("\n📊 Bottom 5 counties (smallest effects):\n")
print(X_means[order(abs(tau_i_pp))][1:5, .(cid, tau_i_pp, pct_change)])

# -------------------- Save Results for Step 3 --------------------
cat("\n", strrep("=", 60), "\n")
cat("SAVING OUTPUTS\n")
cat(strrep("=", 60), "\n")

# Package all essential components for Step 3
step2_output <- list(
  # BLP model components
  blp_coef = coef(blp_fit),           # Coefficient vector β̂
  vcov_blp = vcov(blp_fit),           # Variance-covariance matrix for uncertainty
  moderators = mods,                   # List of moderator names (for alignment)
  
  # County-level predictions
  X_means = X_means,                   # County means + tau_i_pp
  
  # Average treatment effect (for anchoring)
  ate = ate_estimate,                  # ATE point estimate
  ate_se = ate_se,                     # ATE standard error
  
  # Standardization parameters (for new data in Step 3)
  baseline_stats = baseline_stats,     # Mean & SD from baseline period
  
  # Metadata
  overlap_bounds = c(lower=overlap_lower, upper=overlap_upper),
  n_obs = nrow(dt),
  n_counties = uniqueN(dt$cid),
  date_created = Sys.time()
)

# Save main output (R object for Step 3)
saveRDS(step2_output, "step2_output.rds")
cat("✅ Saved: step2_output.rds (complete results for Step 3)\n")

# Save county effects as CSV (for inspection)
fwrite(X_means, "step2_county_effects.csv")
cat("✅ Saved: step2_county_effects.csv (county-level predictions)\n")

# Save BLP coefficients as CSV (for reporting)
fwrite(blp_coef_table, "step2_blp_coefs.csv")
cat("✅ Saved: step2_blp_coefs.csv (regression coefficients)\n")

# -------------------- Interpretation Guide --------------------
cat("\n", strrep("=", 60), "\n")
cat("⚠️  HOW TO USE THESE RESULTS\n")
cat(strrep("=", 60), "\n")

cat("\n1. SCALE AND UNITS:\n")
cat("   • tau_i_pp is on LOG SCALE (since Y = log(cases))\n")
cat("   • Convert to % change: pct_change = 100 × (exp(tau_i_pp) - 1)\n")
cat("   • Small tau (<0.1): linear approximation works (% ≈ 100×tau)\n")


