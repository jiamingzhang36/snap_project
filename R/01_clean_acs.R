#ACS Data clean

# 0) 必要包
if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
library(data.table)

infile <- "~/Desktop/usa_00003.csv.gz"

# 1) 只读表头，并去掉列名外层的双引号
get_header <- function(p) {
  con <- gzfile(path.expand(p), open = "rt")
  on.exit(close(con))
  ln <- readLines(con, n = 1)
  cols <- trimws(strsplit(ln, ",", fixed = TRUE)[[1]])
  cols <- gsub('^"|"$', "", cols)   # <-- 关键：去掉开头/结尾引号
  cols
}
head_cols <- get_header(infile)

# 2) 需要的列（和你文件里的列一一对上）
wanted <- c(
  "YEAR","SERIAL","PERNUM","PERWT","HHWT",
  "STATEFIP","COUNTYFIP","PUMA","GQ",
  "FOODSTMP","FAMSIZE","SEX","AGE","RACE","HISPAN",
  "EDUCD","EDUC","EMPSTAT","LABFORCE","UHRSWORK","WKSWORK1",
  "INCTOT","INCWAGE","POVERTY"
)
use_cols <- intersect(wanted, head_cols)
if (length(use_cols) == 0L) {
  stop("还是没匹配到需要的列；实际列名：\n", paste(head_cols, collapse=", "))
}

# 3) 在管道里先筛 Michigan（STATEFIP==26），再 fread，只读所需列
dec <- if (nzchar(Sys.which("gzcat"))) "gzcat" else "zcat"
state_col <- which(head_cols == "STATEFIP")[1]
if (is.na(state_col)) stop("找不到 STATEFIP 列位置。")
cmd <- sprintf("%s %s | awk -F, 'NR==1 || $%d==26'", dec, shQuote(path.expand(infile)), state_col)

# 4) 指定类型，进一步省内存
int_cols <- intersect(c("YEAR","SERIAL","PERNUM","STATEFIP","COUNTYFIP","AGE","EMPSTAT","LABFORCE","WKSWORK1","FOODSTMP"), use_cols)
num_cols <- intersect(c("PERWT","HHWT","UHRSWORK","INCTOT","INCWAGE","POVERTY"), use_cols)
cc <- setNames(rep("integer", length(int_cols)), int_cols)
cc <- c(cc, setNames(rep("numeric", length(num_cols)), num_cols))

dt <- fread(cmd = cmd, select = use_cols, colClasses = cc, showProgress = TRUE)
cat("Loaded rows:", nrow(dt), " cols:", ncol(dt), "\n")

# ========= D) 清洗与派生（稳健版本）=========
has <- function(v) v %in% names(dt)

# (可选) 普通住户：GQ==1
if (has("GQ")) dt <- dt[GQ == 1 | is.na(GQ)]

# Michigan + 18–64
if (has("STATEFIP")) dt <- dt[STATEFIP == 26]
if (has("AGE"))      dt <- dt[AGE >= 18 & AGE <= 64]

# 教育：EDUCD 优先，否则 EDUC
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

# 就业/劳参
if (has("EMPSTAT")) {
  dt[, employed   := fifelse(EMPSTAT == 1L, 1L, fifelse(EMPSTAT %in% c(2L,3L), 0L, NA_integer_))]
  dt[, unemployed := fifelse(EMPSTAT == 2L, 1L, fifelse(EMPSTAT %in% c(1L,3L), 0L, NA_integer_))]
} else dt[, `:=`(employed = NA_integer_, unemployed = NA_integer_)]
if (has("LABFORCE")) {
  dt[, in_lf := fifelse(LABFORCE == 2L, 1L, fifelse(LABFORCE == 1L, 0L, NA_integer_))]
} else dt[, in_lf := NA_integer_]

# SNAP（户层）
if (has("FOODSTMP")) {
  dt[, snap_hh := fifelse(FOODSTMP == 1L, 1L, fifelse(FOODSTMP == 2L, 0L, NA_integer_))]
} else dt[, snap_hh := NA_integer_]

# 贫困
if (has("POVERTY")) {
  dt[, pov_ratio := as.numeric(POVERTY)/100]
  dt[, le_130fpl := fifelse(!is.na(pov_ratio) & pov_ratio <= 1.30, 1L,
                            fifelse(!is.na(pov_ratio), 0L, NA_integer_))]
} else dt[, `:=`(pov_ratio = NA_real_, le_130fpl = NA_integer_)]

# 年工时
if (has("UHRSWORK") && has("WKSWORK1")) {
  dt[, ann_hours := as.numeric(UHRSWORK) * pmin(pmax(as.numeric(WKSWORK1), 0), 52)]
} else dt[, ann_hours := NA_real_]

# 个人收入：INCTOT 优先，否则 INCWAGE
if (has("INCTOT")) dt[, income_person := as.numeric(INCTOT)]
if (!has("income_person") && has("INCWAGE")) dt[, income_person := as.numeric(INCWAGE)]
if (!has("income_person")) dt[, income_person := NA_real_]

# county_id（剔除不可识别县）
dt[, county_id := sprintf("%02d%03d", as.integer(STATEFIP), as.integer(COUNTYFIP))]
dt <- dt[COUNTYFIP > 0 & COUNTYFIP < 999]

# ========= E) 县×年（ACS）=========
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

# ========= F) 合并到县×月并导出 =========
m <- fread(monthly_path)
m[, county_id := sprintf("%05d", as.integer(county_id))]
m[, `:=`(YEAR = as.integer(year), MONTH = as.integer(month))]

acs_cy[, `:=`(YEAR = as.integer(YEAR), county_id = as.character(county_id))]
merged_cm <- merge(m, acs_cy, by = c("YEAR","county_id"), all.x = TRUE)

fwrite(merged_cm, out_path)
cat("✅ 合并完成：", normalizePath(out_path),
    "\nRows:", nrow(merged_cm), " Cols:", ncol(merged_cm), "\n")


library(data.table)
z <- fread("~/Desktop/snap_laus_with_acs_county_month_mi.csv.gz")

# 1) 行数是否与月表一致
nrow(z)  # 7056 说明对齐

# 2) 看新增的 ACS 列是否存在
intersect(c("emp_rate","lfpr","le_130fpl_share","hrs_year_avg","income_avg",
            "snap_hh_share","pop_weighted","households_weighted"), names(z))

# 3) 比例变量是否在 0~1
summary(z[, .(emp_rate, lfpr, le_130fpl_share, snap_hh_share)])

# 4) 某年抽查
z[YEAR==2020][order(county_id)][1:5]

