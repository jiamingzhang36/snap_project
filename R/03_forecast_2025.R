# Step 3: 2025 OBBBA forecast (skeleton)
# Comments are in English only
source("R/utils.R")

# 1) Inputs: Step2 tau_i (county-specific effects), county features, 2025 scenarios
out_dir <- safe_dir("outputs/step3_forecast")

# 2) TODO:
#    - Load Step2 outputs (e.g., step2_tau_i_pp.csv, step2_county_effects.csv)
#    - Build baseline 2025 caseload per county
#    - Apply county-specific treatment effects to construct forecasts
#    - Create scenario variants (e.g., unemployment +/- 5pp)
#    - Save: step3_tau_anchored.csv, step3_post_outputs.csv, optional map PNGs

message("[Step3] Forecast skeleton loaded. TODO: implement.")





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



