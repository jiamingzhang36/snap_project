#ACS Data clean

# 0) Required packages
if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
library(data.table)

infile <- "~/Desktop/usa_00003.csv.gz"

# 1) Read header only and remove outer double quotes from column names
get_header <- function(p) {
  con <- gzfile(path.expand(p), open = "rt")
  on.exit(close(con))
  ln <- readLines(con, n = 1)
  cols <- trimws(strsplit(ln, ",", fixed = TRUE)[[1]])
  cols <- gsub('^"|"$', "", cols)   # <-- Key: remove leading/trailing quotes
  cols
}
head_cols <- get_header(infile)

# 2) Required columns (match one-to-one with columns in your file)
wanted <- c(
  "YEAR","SERIAL","PERNUM","PERWT","HHWT",
  "STATEFIP","COUNTYFIP","PUMA","GQ",
  "FOODSTMP","FAMSIZE","SEX","AGE","RACE","HISPAN",
  "EDUCD","EDUC","EMPSTAT","LABFORCE","UHRSWORK","WKSWORK1",
  "INCTOT","INCWAGE","POVERTY"
)
use_cols <- intersect(wanted, head_cols)
if (length(use_cols) == 0L) {
  stop("Still couldn't match required columns; actual column names:\n", paste(head_cols, collapse=", "))
}

# 3) Filter Michigan (STATEFIP==26) in pipeline first, then fread, only read required columns
dec <- if (nzchar(Sys.which("gzcat"))) "gzcat" else "zcat"
state_col <- which(head_cols == "STATEFIP")[1]
if (is.na(state_col)) stop("Could not find STATEFIP column position.")
cmd <- sprintf("%s %s | awk -F, 'NR==1 || $%d==26'", dec, shQuote(path.expand(infile)), state_col)

# 4) Specify types to further save memory
int_cols <- intersect(c("YEAR","SERIAL","PERNUM","STATEFIP","COUNTYFIP","AGE","EMPSTAT","LABFORCE","WKSWORK1","FOODSTMP"), use_cols)
num_cols <- intersect(c("PERWT","HHWT","UHRSWORK","INCTOT","INCWAGE","POVERTY"), use_cols)
cc <- setNames(rep("integer", length(int_cols)), int_cols)
cc <- c(cc, setNames(rep("numeric", length(num_cols)), num_cols))

dt <- fread(cmd = cmd, select = use_cols, colClasses = cc, showProgress = TRUE)
cat("Loaded rows:", nrow(dt), " cols:", ncol(dt), "\n")

# ========= D) Cleaning and derivation (robust version) =========
has <- function(v) v %in% names(dt)

# (Optional) Regular households: GQ==1
if (has("GQ")) dt <- dt[GQ == 1 | is.na(GQ)]

# Michigan + 18–64
if (has("STATEFIP")) dt <- dt[STATEFIP == 26]
if (has("AGE"))      dt <- dt[AGE >= 18 & AGE <= 64]

# Education: EDUCD preferred, otherwise EDUC
if (has("EDUCD")) {
  dt[, educ_cat := fifelse(EDUCD < 62, "BelowHS",
                           fifelse(EDUCD %between% c(62,64), "HS",
                                   fifelse(EDUCD %between% c(65,100), "SomeCollegePlus", NA_character_)))]
} else if (has("EDUC")) {
  dt[, educ_cat := fcase(
    EDUC <= 6, "BelowHS",
    EDUC == 7, "HS",
    EDUC >= 8, "SomeCollegePlus",
    default = NA_character_
  )]
} else dt[, educ_cat := NA_character_]

# Employment/Labor force participation
if (has("EMPSTAT")) {
  dt[, employed   := fifelse(EMPSTAT == 1L, 1L, fifelse(EMPSTAT %in% c(2L,3L), 0L, NA_integer_))]
  dt[, unemployed := fifelse(EMPSTAT == 2L, 1L, fifelse(EMPSTAT %in% c(1L,3L), 0L, NA_integer_))]
} else dt[, `:=`(employed = NA_integer_, unemployed = NA_integer_)]
if (has("LABFORCE")) {
  dt[, in_lf := fifelse(LABFORCE == 2L, 1L, fifelse(LABFORCE == 1L, 0L, NA_integer_))]
} else dt[, in_lf := NA_integer_]

# SNAP (household level)
if (has("FOODSTMP")) {
  dt[, snap_hh := fifelse(FOODSTMP == 1L, 1L, fifelse(FOODSTMP == 2L, 0L, NA_integer_))]
} else dt[, snap_hh := NA_integer_]

# Poverty
if (has("POVERTY")) {
  dt[, pov_ratio := as.numeric(POVERTY)/100]
  dt[, le_130fpl := fifelse(!is.na(pov_ratio) & pov_ratio <= 1.30, 1L,
                            fifelse(!is.na(pov_ratio), 0L, NA_integer_))]
} else dt[, `:=`(pov_ratio = NA_real_, le_130fpl = NA_integer_)]

# Annual work hours
if (has("UHRSWORK") && has("WKSWORK1")) {
  dt[, ann_hours := as.numeric(UHRSWORK) * pmin(pmax(as.numeric(WKSWORK1), 0), 52)]
} else dt[, ann_hours := NA_real_]

# Personal income: INCTOT preferred, otherwise INCWAGE
if (has("INCTOT")) dt[, income_person := as.numeric(INCTOT)]
if (!has("income_person") && has("INCWAGE")) dt[, income_person := as.numeric(INCWAGE)]
if (!has("income_person")) dt[, income_person := NA_real_]

# county_id (remove unidentifiable counties)
dt[, county_id := sprintf("%02d%03d", as.integer(STATEFIP), as.integer(COUNTYFIP))]
dt <- dt[COUNTYFIP > 0 & COUNTYFIP < 999]

# ========= E) County × Year (ACS) =========
person_agg <- dt[, .(
  pop_weighted     = sum(PERWT, na.rm = TRUE),
  emp_rate         = weighted.mean(employed, PERWT, na.rm = TRUE),
  lfpr             = weighted.mean(in_lf, PERWT, na.rm = TRUE),
  le_130fpl_share  = weighted.mean(le_130fpl, PERWT, na.rm = TRUE),
  hrs_year_avg     = weighted.mean(ann_hours, PERWT, na.rm = TRUE),
  income_avg       = weighted.mean(income_person, PERWT, na.rm = TRUE)
), by = .(YEAR, county_id)]

hh <- dt[, .(
  snap_hh   = max(snap_hh, na.rm = TRUE),
  HHWT      = max(HHWT, na.rm = TRUE),
  county_id = first(county_id),
  YEAR      = first(YEAR)
), by = .(YEAR, STATEFIP, COUNTYFIP, SERIAL)]
hh[!is.finite(snap_hh), snap_hh := NA_real_]
hh_agg <- hh[, .(
  households_weighted = sum(HHWT, na.rm = TRUE),
  snap_hh_share       = weighted.mean(snap_hh, HHWT, na.rm = TRUE)
), by = .(YEAR, county_id)]

acs_cy <- merge(person_agg, hh_agg, by = c("YEAR","county_id"), all = TRUE)

# ========= F) Merge to county × month and export =========
# Note: This script processes ACS data to county-year level
# The merge to monthly panel should be done in 01_clean_other.R
# Save county-year ACS data
root <- "~/Desktop/snap_project"
out_dir <- file.path(root, "data_clean")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_path <- file.path(out_dir, "acs_county_year_MI.csv")

acs_cy[, `:=`(YEAR = as.integer(YEAR), county_id = as.character(county_id))]
fwrite(acs_cy, out_path)
cat("✅ Saved ACS county-year data: ", normalizePath(out_path),
    "\nRows:", nrow(acs_cy), " Cols:", ncol(acs_cy), "\n")

