############################################################
# Callaway-Sant'Anna DR-DiD (Basic, with covariates)
# Y = y_log1p_per1k_18_49
# Covariates: unemployment_rate, log(1 + labor_force)
# Time window: 2016-01 to 2019-12
############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(readr)
  library(did)      # install.packages("did") if not installed
})

# ---------------- Paths & load data ----------------
root   <- "/Users/jiamingzhang/Desktop/snap_project/data_clean"
infile <- file.path(root, "panel_with_G.csv")
stopifnot(file.exists(infile))

raw <- read_csv(infile, show_col_types = FALSE)

# ---------------- 1. Fix choices ----------------
Y_COL  <- "y_log1p_per1k_18_49"             # your chosen outcome
COVARS <- c("unemployment_rate", "log_lf")  # we'll create log_lf below

# ---------------- 2. Build analysis dataset ----------------
df <- raw %>%
  mutate(
    # ensure date is Date
    date = as.Date(date),
    
    # numeric time index for CS-DiD
    t = year(date) * 12L + month(date),
    
    # numeric unit id (required by did)
    id_num = as.integer(factor(id)),
    
    # cohort as numeric: first enforcement month (G is "YYYY-MM" or "0")
    G_int = ifelse(
      is.na(G) | G == "0",
      0L,
      as.integer(substr(G, 1, 4)) * 12L + as.integer(substr(G, 6, 7))
    ),
    
    # outcome
    Y = .data[[Y_COL]],
    
    # covariates
    unemployment_rate = as.numeric(unemployment_rate),
    log_lf = log1p(as.numeric(labor_force))
  ) %>%
  # choose analysis window: rollout + stable enforcement pre-COVID
  filter(date >= as.Date("2016-01-01"),
         date <= as.Date("2018-12-01")) %>%
  # drop missing outcome
  filter(!is.na(Y)) %>%
  # drop rows with missing covariates
  filter(complete.cases(across(all_of(COVARS))))

# ---------------- 3. Quick diagnostics ----------------
cat("Rows after filters:", nrow(df), "\n")
cat("Unique units:", n_distinct(df$id_num), "\n")
cat("Any treated cohorts (G_int > 0)? ", any(df$G_int > 0), "\n")

if (!any(df$G_int > 0)) {
  stop("No treated units (G_int > 0) in this sample. Check G coding or date window.")
}

# ---------------- 4. Run Callaway-Sant'Anna DR-DiD ----------------
form_x <- ~ unemployment_rate + log_lf   # covariate formula

att <- att_gt(
  yname                  = "Y",
  tname                  = "t",
  idname                 = "id_num",
  gname                  = "G_int",
  xformla                = form_x,
  data                   = df,
  panel                  = TRUE,
  control_group          = "notyettreated",  # staggered adoption
  est_method             = "dr",             # doubly robust
  allow_unbalanced_panel = TRUE
)

# ---------------- 5. Aggregate & inspect ----------------
overall <- aggte(att, type = "simple")
es      <- aggte(att, type = "dynamic", cband = TRUE)

cat("\n================ CS DR-DiD (BASIC) ================\n")
cat("Y =", Y_COL, "\n")
cat(sprintf("Overall ATT: %.4f (SE = %.4f)\n",
            overall$overall.att, overall$overall.se))
cat("===================================================\n")




# 1️⃣ 查看 event-study 系数表
summary(es)

# 2️⃣ 提取为数据框，方便检验
es_df <- data.frame(
  egt  = es$egt,
  att  = es$att.egt,
  se   = es$se.egt
) %>%
  mutate(
    tval = att / se,
    pval = 2 * pnorm(-abs(tval))
  )

# 3️⃣ 看 pre-treatment (egt < 0)
es_df %>% filter(egt < 0)


# 简单 joint Wald 检验
library(car)
pre <- es_df %>% filter(egt < 0)
wald_stat <- sum((pre$att / pre$se)^2)
p_wald <- 1 - pchisq(wald_stat, df = nrow(pre))
cat("Joint pre-trend test p-value:", round(p_wald, 4), "\n")







