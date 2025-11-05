#FAP Ouput

library(dplyr)
library(stringr)
library(lubridate)
library(readr)

# 已加载数据
d17  <- FAP_2017_ALL_COUNTIES
d18p <- FAP_ALL_YEARS_COMBINED

# === 1) 清理 ===
clean_names <- function(df){
  df <- df %>%
    rename_with(~ str_replace_all(.x, "\\.", " ")) %>%  # 把点改成空格
    rename_with(~ str_trim(.x)) %>%
    rename(
      county = county,
      year = year,
      Cases = Cases,
      Recipients = Recipients,
      `Adult Recipients` = `Adult Recipients`,
      `Child Recipients` = `Child Recipients`,
      Payments = Payments
    )
  
  # 县名规范化
  if ("county" %in% names(df)) {
    df <- df %>%
      mutate(county = str_to_title(county),
             county = str_remove(county, "\\s*County\\b"))
  }
  
  # 数值列去逗号并转 numeric
  num_cols <- intersect(c("Cases","Recipients","Adult Recipients","Child Recipients","Payments"), names(df))
  df <- df %>% mutate(across(all_of(num_cols), ~ as.numeric(gsub(",", "", .x))))
  
  # 删除 page_no, table_index
  df %>% select(-any_of(c("page_no","table_index")))
}

d17  <- clean_names(d17)
d18p <- clean_names(d18p)

# === 2) 合并 ===
snap_raw <- bind_rows(
  d17  %>% mutate(source = "2017"),
  d18p %>% mutate(source = "combined")
) %>%
  mutate(FY = as.integer(year))

# === 3) 每个 county×FY 取 12 个月（去掉多余的汇总行） ===
snap_all <- snap_raw %>%
  mutate(.row = row_number()) %>%
  arrange(county, FY, factor(source, levels = c("2017","combined")), .row) %>%
  group_by(county, FY) %>%
  mutate(row_in_grp = row_number()) %>%
  slice_head(n = 12) %>%
  mutate(
    start_date = ymd(sprintf("%d-10-01", FY - 1)),
    date  = start_date + months(row_in_grp - 1),
    year  = year(date),
    month = month(date)
  ) %>%
  ungroup() %>%
  select(county, year, month, FY,
         Cases, Recipients, `Adult Recipients`, `Child Recipients`, Payments) %>%
  arrange(county, year, month)

# === 4) 检查是否每个县×FY 都有 12 行 ===
qc <- count(snap_all, county, FY, name = "n_rows")
if (any(qc$n_rows != 12)) {
  warning("⚠️ 部分 county×FY 不是 12 行。")
  print(qc %>% filter(n_rows != 12))
}

# === 5) 导出 ===
write_csv(snap_all, "snap_all_counties_monthly_2016_2025.csv")

# 预览前几行
head(snap_all)


library(dplyr)

# 覆盖面
snap_all %>% summarise(n_rows = n(), n_counties = n_distinct(county),
                       fy_min = min(FY), fy_max = max(FY))

# 每个 county×FY 是否满 12 个月
qc <- count(snap_all, county, FY, name = "n_rows")
qc %>% filter(n_rows != 12)   # 空表就说明都齐了


out_path <- "~/Desktop/snap_all_counties_monthly_2016_2025.csv"
write_csv(snap_all, out_path)

cat("✅ 已成功导出到桌面：", out_path, "\n")



#Unemployment

library(dplyr)
library(stringr)
library(tidyr)
library(readr)

## --- 0) 标准化列名 ---
std_names <- function(df) { names(df) <- tolower(str_squish(names(df))); df }
la_data    <- std_names(la_data_raw)
la_area    <- std_names(la_area_raw)
la_measure <- std_names(la_measure_raw)

## --- 1) 清理 la_data：去年度行、提取 month/area_code/measure_code ---
la_data <- la_data %>%
  filter(!grepl("^m13$", tolower(period))) %>%
  mutate(
    month        = as.integer(sub("^m", "", tolower(period))),
    area_code5   = substr(series_id, 6, 10),        # 5位FIPS (州2+县3)
    measure_code3 = substr(series_id, 18, 20)       # 三位，例如 "003"
  )

## --- 2) 清理 la_measure：使其与三位 measure_code 对齐 ---
# 原表是 "03"/"04"/"05"/"06" 等 → 左侧补零到三位
if (!"measure_code" %in% names(la_measure)) {
  cand <- names(la_measure)[grepl("^measure.?code$", names(la_measure))]
  if (length(cand) == 0) stop("找不到 measure_code 列"); 
  la_measure <- la_measure %>% rename(measure_code = all_of(cand[1]))
}
if (!"measure_text" %in% names(la_measure)) {
  cand <- names(la_measure)[grepl("^measure.?text$|^measure.?name$", names(la_measure))]
  if (length(cand) == 0) stop("找不到 measure_text/name 列"); 
  la_measure <- la_measure %>% rename(measure_text = all_of(cand[1]))
}
la_measure_clean <- la_measure %>%
  mutate(
    measure_code3 = str_pad(str_replace_all(measure_code, "\\D", ""), width = 3, side = "left", pad = "0"),
    measure_text  = str_squish(measure_text)
  ) %>%
  select(measure_code3, measure_text)

## --- 3) 强力清洗 la.area：找出县级（后三位≠000），并提取县名 ---
# 自适应 area_code 列名
if (!"area_code" %in% names(la_area)) {
  acand <- names(la_area)[grepl("area.?code|^code$", names(la_area))]
  if (length(acand) == 0) stop("在 la.area 中找不到 area_code/areacode/code 等列。")
  la_area <- la_area %>% rename(area_code = all_of(acand[1]))
}
# 自适应“名称”列名
name_candidates <- c("area_text","area name","area_name","name","label","area","areatitle","area_title")
existing_name_col <- intersect(name_candidates, names(la_area))
if (length(existing_name_col) == 0) {
  stop(paste0("在 la.area 中未找到县名列。当前列名：", paste(names(la_area), collapse = ", ")))
}

la_area_clean <- la_area %>%
  mutate(
    area_code = str_squish(as.character(area_code)),
    # 提取**首个**5位数字串（可能是州FIPS+县FIPS）
    area_code5 = str_extract(area_code, "\\d{5}"),
    county_raw = .data[[existing_name_col[1]]]
  ) %>%
  filter(!is.na(area_code5)) %>%
  # 仅保留县级（后三位≠000）
  filter(!grepl("000$", area_code5)) %>%
  # 选最像县的记录
  mutate(is_county_like = grepl("county", county_raw, ignore.case = TRUE) |
           grepl(",\\s*[A-Z]{2}$", county_raw) |
           grepl(",\\s*Michigan$", county_raw, ignore.case = TRUE)) %>%
  arrange(area_code5, desc(is_county_like)) %>%
  group_by(area_code5) %>%
  slice(1) %>% ungroup() %>%
  transmute(area_code5, county_raw = str_squish(as.character(county_raw)))

## --- 4) 合并，先把 measure 连接修好 ---
laus_full2 <- la_data %>%
  left_join(la_measure_clean, by = "measure_code3") %>%
  left_join(la_area_clean,    by = "area_code5")

# 诊断：指标和县名匹配情况
cat(sprintf("指标(measure_text) 非缺失比例：%.1f%%\n", 100*mean(!is.na(laus_full2$measure_text))))
cat(sprintf("县名(county_raw)  非缺失比例：%.1f%%\n", 100*mean(!is.na(laus_full2$county_raw))))

## --- 5) 选四类指标 → 规范化 → 宽表；若县名缺失，回退用 area_code5 作为 ID ---
laus_clean <- laus_full2 %>%
  filter(!is.na(measure_text)) %>%
  filter(
    str_detect(measure_text, regex("unemployment\\s*rate",  ignore_case = TRUE)) |
      str_detect(measure_text, regex("^employ(ed|ment)\\b",   ignore_case = TRUE)) |
      str_detect(measure_text, regex("^unemploy(ed|ment)\\b", ignore_case = TRUE)) |
      str_detect(measure_text, regex("(civilian\\s*)?labor\\s*force", ignore_case = TRUE))
  ) %>%
  mutate(
    measure_std = case_when(
      str_detect(measure_text, regex("unemployment\\s*rate",  ignore_case = TRUE)) ~ "unemployment_rate",
      str_detect(measure_text, regex("^employ(ed|ment)\\b",   ignore_case = TRUE)) ~ "employment",
      str_detect(measure_text, regex("^unemploy(ed|ment)\\b", ignore_case = TRUE)) ~ "unemployed",
      str_detect(measure_text, regex("(civilian\\s*)?labor\\s*force", ignore_case = TRUE)) ~ "labor_force",
      TRUE ~ NA_character_
    ),
    value = suppressWarnings(as.numeric(value)),
    county_id = area_code5,
    county    = county_raw
  ) %>%
  filter(!is.na(measure_std)) %>%
  transmute(
    county_id,                               # 兜底留FIPS
    county = if_else(is.na(county) | county == "", NA_character_, county),
    year, month, measure_std, value
  ) %>%
  # 清洗县名（如果有）
  mutate(
    county = str_to_title(county),
    county = str_remove(county, "\\s*County\\b"),
    county = str_remove(county, ",\\s*[A-Z]{2}$"),
    county = str_remove(county, ",\\s*Michigan$")
  ) %>%
  group_by(county_id, county, year, month, measure_std) %>%
  summarise(value = dplyr::first(value), .groups = "drop") %>%
  pivot_wider(names_from = measure_std, values_from = value) %>%
  arrange(coalesce(county, county_id), year, month)

# 快速看下多少行、有没有县名
cat(sprintf("📊 输出维度：%s 行 × %s 列\n", nrow(laus_clean), ncol(laus_clean)))
cat(sprintf("✅ 有县名行比例：%.1f%%（其余用county_id=FIPS代替）\n",
            100*mean(!is.na(laus_clean$county))))

# 导出（包含 county_id 与 county）
out_path <- "~/Desktop/laus_county_monthly.csv"
write_csv(laus_clean, out_path)
cat("✅ 已导出：", out_path, "\n")

# 预览
print(head(laus_clean, 10), n = 10)


library(dplyr)
library(stringr)
library(readr)

# 1) 读你桌面的 LAUS 结果
laus_clean <- read_csv("~/Desktop/laus_county_monthly.csv", show_col_types = FALSE)

# 2) 只保留密歇根（州FIPS=26），并把县名清洗到与 SNAP 对齐的格式
laus_mi <- laus_clean %>%
  filter(substr(county_id, 1, 2) == "26") %>%             # MI only
  mutate(
    county = str_remove(county, regex("\\s*County\\b", ignore_case = TRUE)),
    county = str_remove(county, regex(",\\s*[A-Z]{2}$",  ignore_case = TRUE)),
    county = str_squish(str_to_title(county))
  ) %>%
  select(county_id, county, year, month,
         employment, labor_force, unemployed, unemployment_rate)

# 3) 如果你的 SNAP 数据还在当前会话里（对象名 snap_all），就直接用；
#    如果你已经导出过，也可以读回来：
# snap_all <- read_csv("~/Desktop/snap_all_counties_monthly_2016_2025.csv", show_col_types = FALSE)

# 统一 SNAP 县名格式，避免大小写/后缀差异
snap_mi <- snap_all %>%
  mutate(
    county = str_remove(county, regex("\\s*County\\b", ignore_case = TRUE)),
    county = str_squish(str_to_title(county))
  )

# 4) 合并（按 county + year + month）
snap_laus <- snap_mi %>%
  left_join(laus_mi, by = c("county", "year", "month")) %>%
  mutate(
    participation_rate = if_else(is.finite(Recipients / labor_force),
                                 Recipients / labor_force, NA_real_)
  )

# 5) 快速质检
qc_na <- snap_laus %>%
  summarise(
    rows             = n(),
    matched_rate_pct = 100 * mean(!is.na(unemployment_rate)),
    na_labor_force   = sum(is.na(labor_force)),
    na_unemp_rate    = sum(is.na(unemployment_rate))
  )
print(qc_na)

# 6) 导出到桌面
out_path <- "~/Desktop/snap_laus_MI_monthly_2016_2025.csv"
write_csv(snap_laus, out_path)
cat("✅ 已导出合并面板：", out_path, "\n")





#检查合并
library(tidyverse)
snap_laus <- read_csv("~/Desktop/data clean/snap_laus_MI_monthly_2016_2025.csv",
                      show_col_types = FALSE)
cat("✅ 文件已成功读取！\n")



# 关键键
keys <- c("county","year","month")

# 1a. LAUS 关键变量是否都带上来
snap_laus %>%
  summarise(
    rows = n(),
    matched_unemp_rate = mean(!is.na(unemployment_rate)),
    matched_labor_force = mean(!is.na(labor_force)),
    matched_all = mean(!is.na(unemployment_rate) & !is.na(labor_force))
  )


snap_laus %>%
  filter(is.na(unemployment_rate) | is.na(labor_force)) %>%
  distinct(county) %>%
  arrange(county)


snap_laus <- snap_laus %>%
  filter(county != "X-Unassigned")

cat("✅ 已删除虚拟县 X-Unassigned；现在数据完全匹配。\n")

snap_laus %>%
  summarise(
    rows = n(),
    matched_all = mean(!is.na(unemployment_rate) & !is.na(labor_force))
  )



# 加上waive 信息
library(dplyr)
library(lubridate)
library(readr)

data <- read_csv("/Users/jiamingzhang/Desktop/data clean/snap_laus_MI_monthly_2016_2025.csv")

nonwaived_2017 <- c("Kent","Oakland","Ottawa","Washtenaw")
nonwaived_2018_2019 <- c("Allegan","Barry","Berrien","Clinton","Eaton","Grand Traverse",
                         "Ingham","Ionia","Kalamazoo","Kent","Livingston","Oakland",
                         "Ottawa","Washtenaw")
nonwaived_2020 <- c("Allegan","Kent","Livingston","Oakland","Ottawa","Washtenaw")

data <- data %>%
  mutate(
    ym = make_date(year, month, 1),
    waived = case_when(
      # 2016 全州豁免
      ym >= ymd("2016-01-01") & ym <= ymd("2016-12-01") ~ 1L,
      
      # 2017 部分豁免
      ym >= ymd("2017-01-01") & ym <= ymd("2017-12-01") &
        !(county %in% nonwaived_2017) ~ 1L,
      
      # 2018 上半年：14县执行，其他豁免
      ym >= ymd("2018-01-01") & ym <= ymd("2018-06-01") &
        !(county %in% nonwaived_2018_2019) ~ 1L,
      
      # 2018-07 至 2018-09 全州豁免
      ym >= ymd("2018-07-01") & ym <= ymd("2018-09-01") ~ 1L,
      
      # 2019：14县执行，其余豁免
      ym >= ymd("2019-01-01") & ym <= ymd("2019-12-01") &
        !(county %in% nonwaived_2018_2019) ~ 1L,
      
      # 2020-02 至 2020-03：6县执行，其余豁免
      ym >= ymd("2020-02-01") & ym <= ymd("2020-03-01") &
        !(county %in% nonwaived_2020) ~ 1L,
      
      # 2020-08 起 statewide 豁免
      ym >= ymd("2020-08-01") ~ 1L,
      
      TRUE ~ 0L
    )
  ) %>%
  mutate(enforced = 1L - waived)

write_csv(data, "snap_laus_with_policy.csv")
