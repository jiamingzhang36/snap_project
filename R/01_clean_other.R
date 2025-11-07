## ===== LAUS county-month pipeline =====
## Reads from: /Users/jiamingzhang/Desktop/snap_project/data_raw
## Writes to : /Users/jiamingzhang/Desktop/snap_project/data_clean

suppressPackageStartupMessages({
  library(readr); library(readr); library(dplyr); library(stringr)
  library(tidyr); library(lubridate)
})

in_dir  <- "/Users/jiamingzhang/Desktop/snap_project/data_raw"
out_dir <- "/Users/jiamingzhang/Desktop/snap_project/data_raw"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# --- 0) Locate files by pattern (robust to missing extensions) ---
lf <- list.files(in_dir, full.names = TRUE)
path_data    <- lf[grepl("^.*/la\\.data.*County$", lf)]
path_area    <- lf[grepl("^.*/la\\.area$", lf)]
path_measure <- lf[grepl("^.*/la\\.meas", lf, ignore.case = TRUE)]

if (length(path_data) != 1)    stop("Cannot uniquely find la.data.64.County in data_raw.")
if (length(path_area) != 1)    stop("Cannot uniquely find la.area in data_raw.")
if (length(path_measure) != 1) stop("Cannot uniquely find la.measure in data_raw.")

message("Found:\n- ", basename(path_data), "\n- ", basename(path_area), "\n- ", basename(path_measure))

# --- Helper to read tab-separated or auto-delimited text ---
read_auto <- function(p){
  # Try tab-delimited first (BLS files are typically TSV)
  df <- trySuppressWarnings(read_delim(p, delim = "\t", trim_ws = TRUE, show_col_types = FALSE))
  if (inherits(df, "try-error") || ncol(df) == 1) {
    df <- read_table(p, show_col_types = FALSE)
  }
  df
}
trySuppressWarnings <- function(expr) {
  suppressWarnings(try(expr, silent = TRUE))
}

# --- 1) Read raw files ---
la_data_raw    <- read_auto(path_data)
la_area_raw    <- read_auto(path_area)
la_measure_raw <- read_auto(path_measure)

# --- 2) Standardize column names ---
std_names <- function(df) { names(df) <- tolower(str_squish(names(df))); df }
la_data    <- std_names(la_data_raw)
la_area    <- std_names(la_area_raw)
la_measure <- std_names(la_measure_raw)

# Quick sanity on year range (if present)
if ("year" %in% names(la_data)) {
  message(sprintf("la_data year range: [%s, %s]",
                  suppressWarnings(min(as.integer(la_data$year), na.rm = TRUE)),
                  suppressWarnings(max(as.integer(la_data$year), na.rm = TRUE))))
}

# --- 3) Parse month and codes from la_data ---
la_data <- la_data %>%
  filter(!grepl("^m13$", tolower(period))) %>%             # drop annual summary
  mutate(
    month          = as.integer(sub("^m", "", tolower(period))),
    series_id      = as.character(series_id),
    area_code5     = substr(series_id, 6, 10),             # 5-digit FIPS (state2 + county3)
    measure_code3  = substr(series_id, 18, 20)             # 3-digit measure
  )

# --- 4) Clean measure lookup ---
if (!"measure_code" %in% names(la_measure)) {
  cand <- names(la_measure)[grepl("^measure.?code$", names(la_measure))]
  if (length(cand) == 0) stop("Cannot find measure_code column in la.measure.")
  la_measure <- la_measure %>% rename(measure_code = all_of(cand[1]))
}
if (!"measure_text" %in% names(la_measure)) {
  cand <- names(la_measure)[grepl("^measure.?text$|^measure.?name$", names(la_measure))]
  if (length(cand) == 0) stop("Cannot find measure_text/name column in la.measure.")
  la_measure <- la_measure %>% rename(measure_text = all_of(cand[1]))
}
la_measure_clean <- la_measure %>%
  mutate(
    measure_code3 = str_pad(str_replace_all(as.character(measure_code), "\\D", ""), width = 3, side = "left", pad = "0"),
    measure_text  = str_squish(as.character(measure_text))
  ) %>%
  select(measure_code3, measure_text)

# --- 5) Clean area lookup to county level ---
if (!"area_code" %in% names(la_area)) {
  acand <- names(la_area)[grepl("area.?code|^code$", names(la_area))]
  if (length(acand) == 0) stop("Cannot find area_code in la.area.")
  la_area <- la_area %>% rename(area_code = all_of(acand[1]))
}
name_candidates <- c("area_text","area name","area_name","name","label","area","areatitle","area_title")
existing_name_col <- intersect(name_candidates, names(la_area))
if (length(existing_name_col) == 0) {
  stop(paste0("Cannot find area/county name column in la.area. Columns: ", paste(names(la_area), collapse = ", ")))
}

la_area_clean <- la_area %>%
  mutate(
    area_code = str_squish(as.character(area_code)),
    area_code5 = str_extract(area_code, "\\d{5}"),
    county_raw = .data[[existing_name_col[1]]]
  ) %>%
  filter(!is.na(area_code5)) %>%
  filter(!grepl("000$", area_code5)) %>%                    # keep counties only
  mutate(is_county_like = grepl("county", county_raw, ignore.case = TRUE) |
           grepl(",\\s*[A-Z]{2}$", county_raw) |
           grepl(",\\s*Michigan$", county_raw, ignore.case = TRUE)) %>%
  arrange(area_code5, desc(is_county_like)) %>%
  group_by(area_code5) %>% slice(1) %>% ungroup() %>%
  transmute(area_code5, county_raw = str_squish(as.character(county_raw)))

# --- 6) Merge and diagnose coverage ---
laus_full2 <- la_data %>%
  left_join(la_measure_clean, by = "measure_code3") %>%
  left_join(la_area_clean,    by = "area_code5")

cat(sprintf("Non-missing measure_text: %.1f%%\n", 100*mean(!is.na(laus_full2$measure_text))))
cat(sprintf("Non-missing county_raw : %.1f%%\n", 100*mean(!is.na(laus_full2$county_raw))))

# --- 7) Keep four core metrics and pivot wider ---
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
    county_id,
    county = if_else(is.na(county) | county == "", NA_character_, county),
    year, month, measure_std, value
  ) %>%
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

cat(sprintf("Output shape: %s rows × %s cols\n", nrow(laus_clean), ncol(laus_clean)))
cat(sprintf("Rows with county name: %.1f%%\n", 100*mean(!is.na(laus_clean$county))))

# --- 8) Save main wide table ---
out_main <- file.path(out_dir, "laus_county_monthly.csv")
write_csv(laus_clean, out_main)
cat("✅ Saved: ", out_main, "\n")



############################################################
# Filter LAUS to Michigan (FIPS 26), 2014-10 ~ 2022-12
############################################################

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(stringr)
})

# Paths (adjust if needed)
root    <- "~/Desktop/snap_project"
in_dir  <- file.path(root, "data_raw")
out_dir <- file.path(root, "data_raw")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

in_path  <- file.path(in_dir,  "laus_county_monthly.csv")
out_path <- file.path(out_dir, "laus_MI_2014_2022.csv")

stopifnot(file.exists(in_path))

laus <- read_csv(in_path, show_col_types = FALSE) %>%
  mutate(
    year  = as.integer(year),
    month = as.integer(month),
    # county_id is 5-digit state+county FIPS; Michigan = "26***"
    county_id = as.character(county_id)
  ) %>%
  filter(!is.na(year), !is.na(month)) %>%
  filter(str_starts(county_id, "26")) %>%                  # keep Michigan only
  mutate(ym = year*100L + month) %>%
  filter(ym >= 201410L, ym <= 202212L) %>%                 # 2014-10 .. 2022-12
  select(county_id, county, year, month,
         unemployment_rate, employment, unemployed, labor_force) %>%
  arrange(county, year, month)

message(sprintf("Rows kept: %s; Counties: %s; Years: %s-%s",
                nrow(laus), dplyr::n_distinct(laus$county),
                min(laus$year), max(laus$year)))

write_csv(laus, out_path)
message("✅ Saved: ", out_path)






############################################################
# FAP × LAUS merge (FAP left; timeline follows FAP)
# - Join key: (county_key, year, month)
# - county_key is a heavily-normalized version of county names
# - FAP year/month are derived from date_cal to avoid fiscal/calendar mixups
############################################################

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(stringr)
  library(lubridate); library(tidyr)
})

# -------------------- Paths --------------------
root     <- "~/Desktop/snap_project"
in_raw   <- file.path(root, "data_raw")
in_clean <- file.path(root, "data_clean")
dir.create(in_clean, showWarnings = FALSE, recursive = TRUE)

# Adjust if your filenames differ
fap_path  <- file.path(in_raw,  "FAP_ALL_YEARS_COMBINED_MERGED.csv")
# Prefer LAUS from data_clean; fallback to data_raw if needed
laus_clean_path <- file.path(in_clean, "laus_MI_2014_2022.csv")
laus_raw_path   <- file.path(in_raw,   "laus_MI_2014_2022.csv")
laus_path <- if (file.exists(laus_clean_path)) laus_clean_path else laus_raw_path
cat("Using LAUS file:", laus_path, "\n")

stopifnot(file.exists(fap_path), file.exists(laus_path))

# -------------------- Normalizers --------------------
# Strong normalizer to create a stable county_key used for joining
norm_county_key <- function(x){
  x <- str_squish(as.character(x))
  x <- str_replace_all(x, "\\.", "")                    # drop dots
  x <- str_replace_all(x, ",\\s*[A-Za-z]{2}$", "")      # drop ", MI"
  x <- str_replace_all(x, ",\\s*Michigan$", "")         # drop ", Michigan"
  x <- str_replace_all(x, "\\bCounty\\b", "")           # drop "County"
  x <- str_replace_all(x, "\\bSt\\b\\s", "Saint ")      # St -> Saint
  x <- str_replace_all(x, "\\bSt\\.?\\s", "Saint ")     # St. -> Saint
  x <- str_to_lower(x)
  x <- str_replace_all(x, "[^a-z0-9]", "")              # only a-z0-9
  x
}
# Pretty county string for display
pretty_county <- function(x){
  x %>%
    str_squish() %>% str_to_title() %>%
    str_remove("\\s*County\\b") %>%
    str_remove(",\\s*[A-Z]{2}$") %>%
    str_remove(",\\s*Michigan$")
}

# -------------------- Read LAUS --------------------
# Expect: county_id, county, year, month, unemployment_rate, employment, unemployed, labor_force
laus <- read_csv(laus_path, show_col_types = FALSE) %>%
  mutate(
    county       = pretty_county(county),
    county_key   = norm_county_key(county),
    county_id    = as.character(county_id),
    year         = as.integer(year),
    month        = as.integer(month),
    unemployment_rate = as.numeric(unemployment_rate),
    employment        = as.numeric(employment),
    unemployed        = as.numeric(unemployed),
    labor_force       = as.numeric(labor_force)
  ) %>%
  select(county_id, county, county_key, year, month,
         unemployment_rate, employment, unemployed, labor_force) %>%
  arrange(county_key, year, month) %>%
  distinct(county_key, year, month, .keep_all = TRUE)

# -------------------- Read FAP (derive year/month from date_cal) --------------------
# Expect: county, date_cal, cases, recipients, payments, adult recipients, child recipients, ...
fap <- read_csv(fap_path, show_col_types = FALSE)

needed <- c("county", "date_cal", "cases", "recipients", "payments",
            "adult recipients", "child recipients")
miss <- setdiff(needed, names(fap))
if (length(miss)) stop(paste("FAP missing columns:", paste(miss, collapse = ", ")))

fap_clean <- fap %>%
  mutate(
    county     = as.character(county),
    county_key = norm_county_key(county),
    date_cal   = suppressWarnings(ymd(date_cal)),
    year       = year(date_cal),
    month      = month(date_cal),
    is_avg     = is.na(date_cal) | month == 13           # drop M13/averages or bad dates
  ) %>%
  filter(!is_avg) %>%
  transmute(
    county_fap = pretty_county(county),
    county_key, year, month,
    fap_cases        = as.numeric(cases),
    fap_recipients   = as.numeric(recipients),
    fap_payments     = as.numeric(payments),
    adult_recipients = as.numeric(`adult recipients`),
    child_recipients = as.numeric(`child recipients`)
  ) %>%
  group_by(county_key, year, month) %>%
  summarise(
    across(where(is.numeric), ~sum(.x, na.rm = TRUE)),
    county_fap = first(county_fap),
    .groups = "drop"
  ) %>%
  arrange(county_key, year, month)

# -------------------- Merge (FAP left) --------------------
merged <- fap_clean %>%
  left_join(
    laus,
    by = c("county_key", "year", "month"),
    suffix = c("_fap", "_laus")
  ) %>%
  mutate(county = dplyr::coalesce(county_fap, county)) %>%  # prefer FAP county
  relocate(county_id, county, .after = county_key) %>%
  select(-county_fap) %>%
  arrange(county_key, year, month)

# -------------------- Diagnostics --------------------
match_rate <- mean(!is.na(merged$unemployment_rate))
message(sprintf("Match rate: %.1f%% (LAUS variables present)", 100 * match_rate))

if (match_rate < 100) {
  message("Unmatched FAP keys (first 20):")
  print(
    merged %>%
      filter(is.na(unemployment_rate)) %>%
      select(county, county_key, year, month) %>%
      distinct() %>% head(20)
  )
  message("\nDo these county_key exist in LAUS?")
  print(
    laus %>%
      filter(county_key %in% (merged %>% filter(is.na(unemployment_rate)) %>% pull(county_key) %>% unique())) %>%
      select(county, county_key) %>%
      distinct() %>% head(20)
  )
}

# -------------------- Save --------------------
out_merge <- file.path(in_clean, "fap_laus_merged_MI.csv")
out_diag1 <- file.path(in_clean, "diag_fap_rows_missing_laus.csv")
out_diag2 <- file.path(in_clean, "diag_laus_rows_not_in_fap.csv")

write_csv(merged, out_merge)

diag_fap_missing_laus <- merged %>%
  filter(is.na(unemployment_rate)) %>%
  count(county, name = "n_months_missing_laus") %>%
  arrange(desc(n_months_missing_laus))
write_csv(diag_fap_missing_laus, out_diag1)

diag_laus_only <- anti_join(
  laus %>% select(county_key, year, month),
  fap_clean %>% select(county_key, year, month),
  by = c("county_key","year","month")
) %>%
  count(county_key, name = "n_months_only_in_laus") %>%
  arrange(desc(n_months_only_in_laus))
write_csv(diag_laus_only, out_diag2)

message("✅ Saved merged file: ", out_merge)
message("ℹ️ Diagnostics written: ")
message("   - ", out_diag1)
message("   - ", out_diag2)


# 1) Sanity: how many MI counties per source and in the overlap?
cat("FAP counties:", n_distinct(fap_clean$county_key), "\n")
cat("LAUS counties:", n_distinct(laus$county_key), "\n")
cat("Overlap counties:", length(intersect(unique(fap_clean$county_key),
                                          unique(laus$county_key))), "\n")

# 2) Are year & month inside the expected ranges?
range(fap_clean$year); range(laus$year)
range(fap_clean$month); range(laus$month)

# 3) How many rows match on the key? (should be close to FAP rows)
fap_rows <- nrow(fap_clean)
laus_rows <- nrow(laus)
merge_rows <- nrow(merged)
match_rate <- mean(!is.na(merged$unemployment_rate))
cat(sprintf("FAP rows: %d | LAUS rows: %d | merged rows: %d | match rate: %.1f%%\n",
            fap_rows, laus_rows, merge_rows, 100*match_rate))

# 4) Show a few FAP keys that failed to match (to diagnose)
merged %>%
  filter(is.na(unemployment_rate)) %>%
  select(county, county_key, year, month) %>%
  distinct() %>%
  head(15)

# 5) Check if those county_keys actually exist in LAUS
bad_keys <- merged %>% filter(is.na(unemployment_rate)) %>% pull(county_key) %>% unique()
laus %>% filter(county_key %in% bad_keys) %>% distinct(county, county_key) %>% head(15)



# 1) 每县月份是否齐全（2014-10~2022-12 对 FAP 来说应该是起于 2014-10）
table_rows <- merged %>%
  count(county, year, month) %>%
  count(county, name = "n_months")
summary(table_rows$n_months)  # 应该接近 96（2014-10 到 2022-12）或按你的 FAP窗口

# 2) 关键变量是否全为数值、是否有明显缺失
sapply(merged[, c("unemployment_rate","employment","unemployed","labor_force",
                  "fap_recipients","fap_cases","fap_payments")],
       function(x) c(class=class(x)[1], na=sum(is.na(x))))

# 3) 建一个 ym，后续建模更方便
merged <- merged %>% mutate(ym = year*100L + month)

write_csv(merged %>% distinct(county_key, county_id, county),
          file.path(in_clean, "county_crosswalk_MI.csv"))






############################################################
# Add RUCC (2023) + SVI (RPL_THEMES) to monthly panel
# Inputs:
#   - Panel:  ~/Desktop/snap_project/data_clean/fap_laus_merged_MI.csv
#   - RUCC:   ~/Desktop/snap_project/data_raw/Ruralurbancontinuumcodes2023.xlsx
#   - SVI:    ~/Desktop/snap_project/data_raw/{2014,2016,2018,2020,2022}_SVI_Michigan_county.csv
# Output:
#   -        ~/Desktop/snap_project/data_clean/fap_laus_rucc_svi.csv
############################################################

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(stringr)
  library(readxl); library(tidyr)
})

# -------------------- Paths --------------------
root     <- "~/Desktop/snap_project"
in_raw   <- file.path(root, "data_raw")
in_clean <- file.path(root, "data_clean")
dir.create(in_clean, showWarnings = FALSE, recursive = TRUE)

panel_path <- file.path(in_clean, "fap_laus_merged_MI.csv")
rucc_path  <- file.path(in_raw,  "Ruralurbancontinuumcodes2023.xlsx")

stopifnot(file.exists(panel_path), file.exists(rucc_path))

# -------------------- Load panel --------------------
panel <- read_csv(panel_path, show_col_types = FALSE)
if (!all(c("county_id","year","month") %in% names(panel))) {
  stop("Panel must contain columns: county_id, year, month")
}
panel <- panel %>%
  mutate(
    county_id = sprintf("%05d", as.integer(county_id)),
    year  = as.integer(year),
    month = as.integer(month)
  )

# -------------------- Read RUCC 2023 --------------------
rucc_raw <- read_excel(rucc_path, sheet = 1)

# Fuzzy-detect important columns
col_fips <- names(rucc_raw)[grepl("fips", names(rucc_raw), ignore.case = TRUE)][1]
col_rucc <- names(rucc_raw)[grepl("rucc.*2023|rucc[_ ]?code", names(rucc_raw), ignore.case = TRUE)][1]
col_desc <- names(rucc_raw)[grepl("desc", names(rucc_raw), ignore.case = TRUE)][1]

if (is.na(col_fips) || is.na(col_rucc)) {
  stop("RUCC file: could not find FIPS or RUCC code column.")
}

rucc <- rucc_raw %>%
  transmute(
    county_id = sprintf("%05d", suppressWarnings(as.integer(.data[[col_fips]]))),
    rucc_code = suppressWarnings(as.integer(.data[[col_rucc]])),
    rucc_desc = if (!is.na(col_desc)) as.character(.data[[col_desc]]) else NA_character_
  ) %>%
  filter(nchar(county_id) == 5, !is.na(rucc_code)) %>%
  filter(startsWith(county_id, "26")) %>%                  # Michigan only
  distinct(county_id, .keep_all = TRUE) %>%
  mutate(urban_dummy = as.integer(rucc_code <= 3))

message(sprintf("RUCC: %d MI counties (unique).", n_distinct(rucc$county_id)))

# -------------------- Read SVI batches (RPL_THEMES only) --------------------
# Expect files like: 2014_SVI_Michigan_county.csv, ..., 2022_*.csv
svi_files <- list.files(in_raw, pattern = "^[0-9]{4}_SVI_.*_county\\.csv$", full.names = TRUE)
svi_keep_years <- c(2014L, 2016L, 2018L, 2020L, 2022L)

if (!length(svi_files)) stop("No SVI *_Michigan_county.csv files found in data_raw.")

parse_svi_one <- function(path){
  # infer year from filename
  y <- as.integer(str_extract(basename(path), "^[0-9]{4}"))
  if (is.na(y) || !(y %in% svi_keep_years)) return(NULL)
  
  df <- read_csv(path, show_col_types = FALSE)
  
  # Find FIPS and RPL_THEMES columns (names vary slightly across vintages)
  col_fips <- names(df)[grepl("^fips$", names(df), ignore.case = TRUE)][1]
  col_themes <- names(df)[grepl("^rpl_themes$", names(df), ignore.case = TRUE)][1]
  if (is.na(col_fips) || is.na(col_themes)) {
    warning("SVI file missing FIPS or RPL_THEMES: ", basename(path))
    return(NULL)
  }
  
  out <- df %>%
    transmute(
      county_id  = sprintf("%05d", suppressWarnings(as.integer(.data[[col_fips]]))),
      svi_total  = suppressWarnings(as.numeric(.data[[col_themes]]))
    ) %>%
    filter(nchar(county_id) == 5, startsWith(county_id, "26")) %>%  # Michigan only
    distinct(county_id, .keep_all = TRUE) %>%
    mutate(svi_year = y)
  
  out
}

# -------------------- Combine SVI list and print summary --------------------
svi_list <- lapply(svi_files, parse_svi_one)
svi_all  <- bind_rows(svi_list)

if (!nrow(svi_all)) stop("No valid SVI rows parsed. Check files/columns.")

# ✅ Use sprintf directly for messages
message(sprintf("SVI batches loaded: %s",
                paste(sort(unique(svi_all$svi_year)), collapse = ", ")))

# Sanity check that all expected years are present
stopifnot(all(svi_keep_years %in% unique(svi_all$svi_year)))


# -------------------- Map SVI batches to panel years (carry-forward by period) --------------------
# 2014–2015 -> 2014 SVI; 2016–2017 -> 2016; 2018–2019 -> 2018; 2020–2021 -> 2020; 2022 -> 2022
map_tbl <- tibble(
  year = 2014:2022,
  svi_year = case_when(
    year %in% 2014:2015 ~ 2014L,
    year %in% 2016:2017 ~ 2016L,
    year %in% 2018:2019 ~ 2018L,
    year %in% 2020:2021 ~ 2020L,
    year == 2022        ~ 2022L,
    TRUE ~ NA_integer_
  )
)

# Build a wide table county_id x svi_year -> svi_total
svi_wide <- svi_all %>%
  select(county_id, svi_year, svi_total) %>%
  distinct()

# For each (county_id, year) in panel, attach correct svi_total via svi_year mapping
panel_svi <- panel %>%
  select(county_id, year, month, dplyr::everything()) %>%
  left_join(map_tbl, by = "year") %>%
  left_join(svi_wide, by = c("county_id","svi_year"))

# -------------------- Merge RUCC (static) --------------------
panel_rs <- panel_svi %>%
  left_join(rucc, by = "county_id")

# -------------------- Diagnostics --------------------
miss_rucc <- panel_rs %>% filter(is.na(rucc_code)) %>% distinct(county_id) %>% nrow()
miss_svi  <- panel_rs %>% filter(is.na(svi_total)) %>% distinct(county_id, year) %>% nrow()

message(sprintF("Diagnostics — missing RUCC counties: %d", miss_rucc))
message(sprintF("Diagnostics — missing SVI county-years: %d", miss_svi))

# Spot-check
message("Preview with new columns:")
print(panel_rs %>% select(county_id, county, year, month, rucc_code, urban_dummy, svi_total) %>% head(12))

# -------------------- Save --------------------
out_path <- file.path(in_clean, "fap_laus_rucc_svi.csv")
write_csv(panel_rs, out_path)
message("✅ Saved: ", out_path)







############################################################
# Pull ACS county-year controls for Michigan (ACS 5-year)
# Outputs: ~/Desktop/snap_project/data_clean/acs_county_year_MI.csv
# Variables:
#   - population_total   (B01003_001E)
#   - population_18_49   (sum of B01001 age buckets 18–49, M+F)
#   - poverty_rate       (S1701_C03_001E, percent)
#   - bachelor_share     (S1501_C02_015E, percent)
############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(tidycensus)
})

# -------------------- Config --------------------
root       <- "~/Desktop/snap_project"
out_dir    <- file.path(root, "data_clean")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

out_path   <- file.path(out_dir, "acs_county_year_MI.csv")
state_abbr <- "MI"
years      <- 2013:2022   # include 2013 so you can build 2014 lag(12 months)

# Set your Census API key (either here or via Sys.setenv(CENSUS_API_KEY="..."))
# If you already set it in your R profile/environment, you can comment this line.
census_api_key("4c061ed5f2a325b66bd05bad0dcfbfc109866252", install = FALSE, overwrite = TRUE)

# Stable B01001 variable IDs for ages 18–49 (male & female), ACS 5-year
age_vars_18_49 <- c(
  # Male 18–49
  "B01001_007E","B01001_008E","B01001_009E","B01001_010E","B01001_011E",
  "B01001_012E","B01001_013E","B01001_014E","B01001_015E",
  # Female 18–49
  "B01001_031E","B01001_032E","B01001_033E","B01001_034E","B01001_035E",
  "B01001_036E","B01001_037E","B01001_038E","B01001_039E"
)

# -------------------- Fetch per year --------------------
fetch_for_year <- function(y) {
  message(sprintf("Fetching ACS (%d)...", y))
  
  # 1) Total population
  pop_total <- get_acs(
    geography = "county",
    state     = state_abbr,
    variables = c(population_total = "B01003_001E"),
    year      = y, survey = "acs5", geometry = FALSE
  ) %>% select(GEOID, population_total = estimate)
  
  # 2) Population age 18–49 (sum across selected B01001 buckets)
  pop_18_49_long <- get_acs(
    geography = "county",
    state     = state_abbr,
    variables = age_vars_18_49,
    year      = y, survey = "acs5", geometry = FALSE
  ) %>% select(GEOID, variable, estimate)
  
  pop_18_49 <- pop_18_49_long %>%
    group_by(GEOID) %>%
    summarise(population_18_49 = sum(estimate, na.rm = TRUE), .groups = "drop")
  
  # 3) Poverty rate (percent)
  pov <- get_acs(
    geography = "county",
    state     = state_abbr,
    variables = c(poverty_rate = "S1701_C03_001E"),
    year      = y, survey = "acs5", geometry = FALSE
  ) %>% select(GEOID, poverty_rate = estimate)
  
  # 4) Bachelor’s share (percent)
  bach <- get_acs(
    geography = "county",
    state     = state_abbr,
    variables = c(bachelor_share = "S1501_C02_015E"),
    year      = y, survey = "acs5", geometry = FALSE
  ) %>% select(GEOID, bachelor_share = estimate)
  
  # Combine to county-year
  out <- pop_total %>%
    left_join(pop_18_49, by = "GEOID") %>%
    left_join(pov,       by = "GEOID") %>%
    left_join(bach,      by = "GEOID") %>%
    transmute(
      county_id        = sprintf("%05d", as.integer(GEOID)),
      year             = y,
      population_total,
      population_18_49,
      poverty_rate,
      bachelor_share
    )
  
  # Keep Michigan only (FIPS prefix 26)
  out %>% filter(str_starts(county_id, "26"))
}

acs_all <- map_dfr(years, fetch_for_year) %>%
  arrange(county_id, year)

# -------------------- Save --------------------
write_csv(acs_all, out_path)
message("✅ Saved: ", out_path)

# -------------------- Quick sanity --------------------
print(
  acs_all %>%
    summarise(
      n_counties  = n_distinct(county_id),
      min_year    = min(year), max_year = max(year),
      na_pop18_49 = sum(is.na(population_18_49)),
      na_poverty  = sum(is.na(poverty_rate)),
      na_bachelor = sum(is.na(bachelor_share))
    )
)

# Preview
head(acs_all, 10)



############################################################
# Merge ACS county-year controls into monthly panel
# - Compute annual lags (lag by 1 year) before merging
# - Keep full pre-trend (2014–onward)
# Inputs:
#   - Panel (monthly): ~/Desktop/snap_project/data_clean/fap_laus_rucc_svi.csv
#   - ACS (county-year): ~/Desktop/snap_project/data_clean/acs_county_year_MI.csv
# Output:
#   - ~/Desktop/snap_project/data_clean/panel_with_acs.csv
############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

# -------------------- Paths --------------------
root      <- "~/Desktop/snap_project"
in_clean  <- file.path(root, "data_clean")
panel_in  <- file.path(in_clean, "fap_laus_rucc_svi.csv")
acs_in    <- file.path(in_clean, "acs_county_year_MI.csv")
out_path  <- file.path(in_clean, "panel_with_acs.csv")

stopifnot(file.exists(panel_in), file.exists(acs_in))

# -------------------- Read --------------------
panel <- read_csv(panel_in, show_col_types = FALSE)
acs   <- read_csv(acs_in,   show_col_types = FALSE)

# Basic checks
need_cols_panel <- c("county_id","year","month","fap_recipients")
if (!all(need_cols_panel %in% names(panel))) {
  stop(sprintf("Panel is missing columns: %s",
               paste(setdiff(need_cols_panel, names(panel)), collapse=", ")))
}
need_cols_acs <- c("county_id","year","population_total","population_18_49",
                   "poverty_rate","bachelor_share")
if (!all(need_cols_acs %in% names(acs))) {
  stop(sprintf("ACS is missing columns: %s",
               paste(setdiff(need_cols_acs, names(acs)), collapse=", ")))
}

# -------------------- Type coercion --------------------
panel <- panel %>%
  mutate(
    county_id = sprintf("%05d", as.integer(county_id)),
    year  = as.integer(year),
    month = as.integer(month)
  )
acs <- acs %>%
  mutate(
    county_id = sprintf("%05d", as.integer(county_id)),
    year = as.integer(year)
  )

# -------------------- Compute annual lags on ACS --------------------
acs_lag <- acs %>%
  arrange(county_id, year) %>%
  group_by(county_id) %>%
  mutate(
    poverty_rate_lag12     = dplyr::lag(poverty_rate, 1),
    bachelor_share_lag12   = dplyr::lag(bachelor_share, 1),
    population_total_lag12 = dplyr::lag(population_total, 1),
    population_18_49_lag12 = dplyr::lag(population_18_49, 1)
  ) %>%
  ungroup()

# -------------------- Merge into monthly panel --------------------
panel2 <- panel %>%
  left_join(acs_lag, by = c("county_id","year")) %>%
  arrange(county_id, year, month) %>%
  mutate(
    recipients_per_1k_total  = 1000 * fap_recipients / population_total,
    recipients_per_1k_18_49  = 1000 * fap_recipients / population_18_49,
    w_pop_total  = population_total,
    w_pop_18_49  = population_18_49
  )

# -------------------- Diagnostics --------------------
diag <- panel2 %>%
  summarise(
    n_rows        = n(),
    n_counties    = dplyr::n_distinct(county_id),
    ym_min        = sprintf("%d-%02d", min(year), min(month)),
    ym_max        = sprintf("%d-%02d", max(year), max(month)),
    na_pop_total  = sum(is.na(population_total)),
    na_pop_18_49  = sum(is.na(population_18_49)),
    na_pov_lag12  = sum(is.na(poverty_rate_lag12)),
    na_bach_lag12 = sum(is.na(bachelor_share_lag12))
  )
print(diag)

# Spot check one county
print(
  panel2 %>%
    filter(county_id == dplyr::first(panel2$county_id)) %>%
    select(county_id, year, month,
           fap_recipients,
           population_total, population_18_49,
           recipients_per_1k_total, recipients_per_1k_18_49,
           poverty_rate, poverty_rate_lag12,
           bachelor_share, bachelor_share_lag12) %>%
    head(15)
)

# -------------------- Save --------------------
write_csv(panel2, out_path)
message("✅ Saved merged monthly panel with ACS (annual lags): ", out_path)






# --- Michigan ABAWD waiver coding (final, consistent with your county lists) ---

library(dplyr)
library(lubridate)
library(readr)
library(stringr)

data <- read_csv("/Users/jiamingzhang/Desktop/snap_project/data_clean/snap_laus_MI_monthly_2016_2025.csv",
                 show_col_types = FALSE) %>%
  mutate(county = str_squish(str_to_title(str_remove(county, "\\s*County\\b"))))

# 清理无效县名，确保只有官方83个县
data <- data %>%
  mutate(county = str_squish(str_to_title(str_remove(county, "\\s*County\\b")))) %>%
  filter(!county %in% c("X-Unassigned", "Unknown", "Unassigned", "Statewide", "Total")) %>%
  distinct(county, year, month, .keep_all = TRUE)

## 2017: 79 waived counties（你提供的 Appendix 1）
waived_2017 <- c(
  "Alcona","Alger","Allegan","Alpena","Antrim","Arenac","Baraga","Barry","Bay","Benzie","Berrien","Branch",
  "Calhoun","Cass","Charlevoix","Cheboygan","Chippewa","Clare","Clinton","Crawford","Delta","Dickinson","Eaton",
  "Emmet","Genesee","Gladwin","Gogebic","Grand Traverse","Gratiot","Hillsdale","Houghton","Huron","Ingham",
  "Ionia","Iosco","Iron","Isabella","Jackson","Kalamazoo","Kalkaska","Keweenaw","Lake","Lapeer","Leelanau",
  "Lenawee","Livingston","Luce","Mackinac","Macomb","Manistee","Marquette","Mason","Mecosta","Menominee",
  "Midland","Missaukee","Monroe","Montcalm","Montmorency","Muskegon","Newaygo","Oceana","Ogemaw","Ontonagon",
  "Osceola","Oscoda","Otsego","Presque Isle","Roscommon","Saginaw","Sanilac","Schoolcraft","Shiawassee",
  "St. Clair","St. Joseph","Tuscola","Van Buren","Wayne","Wexford"
)
# 非豁免(执行)的 4 县（2017）
nonwaived_2017 <- setdiff(unique(data$county), waived_2017)

## 2018 & 2019: 69 waived counties（你提供的 Appendix 2；与 2019 相同）
waived_2018_2019 <- c(
  "Alcona","Alger","Alpena","Antrim","Arenac","Baraga","Bay","Benzie","Branch","Calhoun","Cass","Charlevoix",
  "Cheboygan","Chippewa","Clare","Crawford","Delta","Dickinson","Emmet","Genesee","Gladwin","Gogebic","Gratiot",
  "Hillsdale","Houghton","Huron","Iosco","Iron","Isabella","Jackson","Kalkaska","Keweenaw","Lake","Lapeer",
  "Leelanau","Lenawee","Luce","Mackinac","Macomb","Manistee","Marquette","Mason","Mecosta","Menominee","Midland",
  "Missaukee","Monroe","Montcalm","Montmorency","Muskegon","Newaygo","Oceana","Ogemaw","Ontonagon","Osceola",
  "Oscoda","Otsego","Presque Isle","Roscommon","Saginaw","Sanilac","Schoolcraft","Shiawassee","St. Clair",
  "St. Joseph","Tuscola","Van Buren","Wayne","Wexford"
)
# 2018/2019 非豁免(执行)的 14 县
nonwaived_2018_2019 <- setdiff(unique(data$county), waived_2018_2019)

## 2020 Feb–Mar: 77 waived counties（你提供的 Appendix 3）
waived_2020_feb_mar <- c(
  "Alcona","Alger","Alpena","Antrim","Arenac","Baraga","Barry","Bay","Benzie","Berrien","Branch","Calhoun","Cass",
  "Charlevoix","Cheboygan","Chippewa","Clare","Clinton","Crawford","Delta","Dickinson","Eaton","Emmet","Genesee",
  "Gladwin","Gogebic","Grand Traverse","Gratiot","Hillsdale","Houghton","Huron","Ingham","Ionia","Iosco","Iron",
  "Isabella","Jackson","Kalamazoo","Kalkaska","Keweenaw","Lake","Lapeer","Leelanau","Lenawee","Luce","Mackinac",
  "Macomb","Manistee","Marquette","Mason","Mecosta","Menominee","Midland","Missaukee","Monroe","Montcalm",
  "Montmorency","Muskegon","Newaygo","Oceana","Ogemaw","Ontonagon","Osceola","Oscoda","Otsego","Presque Isle",
  "Roscommon","Saginaw","Sanilac","Schoolcraft","Shiawassee","St. Clair","St. Joseph","Tuscola","Van Buren",
  "Wayne","Wexford"
)
# 2020-02/03 执行（非豁免）的 6 县 = 全 83 - 这 77
nonwaived_2020_feb_mar <- setdiff(unique(data$county), waived_2020_feb_mar)

# ---------- 编码 ----------
out <- data %>%
  mutate(
    ym = make_date(year, month, 1),
    waived = case_when(
      # 2016: 全州豁免
      ym >= ymd("2016-01-01") & ym <= ymd("2016-12-01") ~ 1L,
      
      # 2017: 79 县豁免（→其余 4 县执行）
      ym >= ymd("2017-01-01") & ym <= ymd("2017-12-01") &
        county %in% waived_2017 ~ 1L,
      
      # 2018: 69 县豁免（全年度按 69 县口径，不是 7–9 月 statewide）
      ym >= ymd("2018-01-01") & ym <= ymd("2018-12-01") &
        county %in% waived_2018_2019 ~ 1L,
      
      # 2019: 同 2018，69 县豁免
      ym >= ymd("2019-01-01") & ym <= ymd("2019-12-01") &
        county %in% waived_2018_2019 ~ 1L,
      
      # 2020-02 ~ 2020-03：77 县豁免（→其余 6 县执行）
      ym >= ymd("2020-02-01") & ym <= ymd("2020-03-01") &
        county %in% waived_2020_feb_mar ~ 1L,
      
      # 2020-08 起：全州豁免
      ym >= ymd("2020-08-01") ~ 1L,
      
      TRUE ~ 0L
    ),
    enforced = 1L - waived
  )

# ---------- 审计：每月豁免县数是否等于预期 ----------
check <- out %>%
  filter(year %in% 2016:2020) %>%
  group_by(year, month) %>%
  summarise(n_waived = sum(waived, na.rm = TRUE), .groups = "drop") %>%
  mutate(ym = make_date(year, month, 1)) %>%
  mutate(expected = case_when(
    ym >= ymd("2016-01-01") & ym <= ymd("2016-12-01") ~ 83L,           # statewide
    ym >= ymd("2017-01-01") & ym <= ymd("2017-12-01") ~ 79L,
    ym >= ymd("2018-01-01") & ym <= ymd("2018-12-01") ~ 69L,
    ym >= ymd("2019-01-01") & ym <= ymd("2019-12-01") ~ 69L,
    ym >= ymd("2020-02-01") & ym <= ymd("2020-03-01") ~ 77L,
    ym >= ymd("2020-08-01") ~ 83L,
    TRUE ~ NA_integer_
  )) %>%
  mutate(ok = (is.na(expected) | n_waived == expected))

print(head(check, 15))
cat(sprintf("审计不匹配的月份：%d 个\n",
            sum(!check$ok, na.rm = TRUE)))

# 输出
write_csv(out, "snap_laus_with_policy.csv")
cat("✅ 写出：snap_laus_with_policy.csv\n")




############################################################
# Michigan ABAWD Waiver Coding (with 2018 monthly phase-in)
# + Build panel with G (first full enforcement month)
# + Create multiple Y definitions (incl. per 18–49 population)
#
# Inputs :
#   data_clean/panel_with_acs.csv
#     (Must include: county, year, month; ideally recipients + population)
#
# Outputs:
#   data_clean/snap_laus_with_policy.csv         # monthly policy flags (keep all cols)
#   data_clean/panel_with_G.csv                  # full panel (all covariates + G + all Y)
#   data_clean/panel_input.csv                   # lean DID input (id/date/G/outcome_final)
#   data_clean/cohort_sizes_from_G.csv           # sizes by G cohort
#   data_clean/support_snapshot.csv              # treated/control counts over time
#   data_clean/waiver_audit_mismatches.csv       # only if mismatches
#   data_clean/panel_outcomes_wide.csv           # id/date + all Y columns (helper)
#
# Notes:
#   - IDs kept as CHARACTER to avoid join issues
#   - Robust to Recipients/recipients column names
#   - No covariate is dropped; we only ADD columns
############################################################

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr); library(lubridate); library(tidyr); library(rlang)
})

# ----------------------- Paths -----------------------
root     <- "/Users/jiamingzhang/Desktop/snap_project"
in_path  <- file.path(root, "data_clean", "panel_with_acs.csv")
out_dir  <- file.path(root, "data_clean")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

out_policy       <- file.path(out_dir, "snap_laus_with_policy.csv")
out_panelG       <- file.path(out_dir, "panel_with_G.csv")
out_panel_input  <- file.path(out_dir, "panel_input.csv")
out_cohort       <- file.path(out_dir, "cohort_sizes_from_G.csv")
out_support      <- file.path(out_dir, "support_snapshot.csv")
out_audit_bad    <- file.path(out_dir, "waiver_audit_mismatches.csv")
out_outcomeswide <- file.path(out_dir, "panel_outcomes_wide.csv")

stopifnot(file.exists(in_path))

# ---------------------- Outcome control ----------------------
# Choose which Y will be exported as 'outcome' in panel_input.csv.
# All Y columns will still be computed and saved in panel_with_G.csv.
# Options below must match the names we compute later.
Y_MODE <- "log1p_per1k_total"   # e.g. "raw", "per1k_total", "per1k_18_49",
# "log1p_raw", "log1p_per1k_total", "log1p_per1k_18_49",
# "per100_lf", "log1p_per100_lf",
# "exit_rate_proxy", "net_change_rate"

# Column name candidates (first match will be used if present)
RECIP_CANDIDATES      <- c("fap_recipients", "Recipients", "recipients")
POP_TOTAL_CANDIDATES  <- c("population", "pop", "total_population", "acs_total_pop", "acs_pop", "pop_total")
POP_18_49_CANDIDATES  <- c("pop_18_49", "population_18_49", "pop18_49", "acs_pop_18_49", "pop_18to49")
LABOR_FORCE_CANDS     <- c("labor_force", "laborforce", "lf", "LAUS_labor_force")

# Optional exits/entries columns (if present we’ll use; otherwise we proxy using lags)
EXITS_CANDIDATES      <- c("exits", "exits_total", "exit_count")
ENTRIES_CANDIDATES    <- c("entries", "entry_count")

# ---------------- Canonical MI 83 counties -------------------
mi_83 <- c(
  "Alcona","Alger","Allegan","Alpena","Antrim","Arenac","Baraga","Barry","Bay","Benzie","Berrien","Branch",
  "Calhoun","Cass","Charlevoix","Cheboygan","Chippewa","Clare","Clinton","Crawford","Delta","Dickinson","Eaton",
  "Emmet","Genesee","Gladwin","Gogebic","Grand Traverse","Gratiot","Hillsdale","Houghton","Huron","Ingham",
  "Ionia","Iosco","Iron","Isabella","Jackson","Kalamazoo","Kalkaska","Kent","Keweenaw","Lake","Lapeer","Leelanau",
  "Lenawee","Livingston","Luce","Mackinac","Macomb","Manistee","Marquette","Mason","Mecosta","Menominee","Midland",
  "Missaukee","Monroe","Montcalm","Montmorency","Muskegon","Newaygo","Oakland","Oceana","Ogemaw","Ontonagon",
  "Osceola","Oscoda","Otsego","Ottawa","Presque Isle","Roscommon","Saginaw","St. Clair","St. Joseph","Sanilac",
  "Schoolcraft","Shiawassee","Tuscola","Van Buren","Washtenaw","Wayne","Wexford"
)

# ---------------- Read & clean base data ---------------------
data0 <- read_csv(in_path, show_col_types = FALSE) %>%
  mutate(
    county = str_squish(str_to_title(str_remove(county, "\\s*County\\b"))),
    ym     = make_date(year, month, 1)
  ) %>%
  filter(!county %in% c("X-Unassigned", "Unknown", "Unassigned", "Statewide", "Total")) %>%
  distinct(county, year, month, .keep_all = TRUE)

# ---- Audit county names vs canonical list ----
present <- sort(unique(data0$county))
extra_in_data      <- setdiff(present, mi_83)
missing_from_data  <- setdiff(mi_83, present)
message(sprintf("Counties in data: %d | Expected MI counties: 83", length(present)))
if (length(extra_in_data))      message("⚠️ Extra county names in data: ", paste(extra_in_data, collapse = ", "))
if (length(missing_from_data))  message("⚠️ Missing MI counties in data: ", paste(missing_from_data, collapse = ", "))

# Keep only canonical 83 to avoid statewide/unknown noise
data <- data0 %>% filter(county %in% mi_83)

# ---------------- Waiver sets (stable years) -------------------
# 2017 = 79, 2018/2019 = 69, 2020 Feb–Mar = 77
waived_2017 <- c(
  "Alcona","Alger","Allegan","Alpena","Antrim","Arenac","Baraga","Barry","Bay","Benzie","Berrien","Branch",
  "Calhoun","Cass","Charlevoix","Cheboygan","Chippewa","Clare","Clinton","Crawford","Delta","Dickinson","Eaton",
  "Emmet","Genesee","Gladwin","Gogebic","Grand Traverse","Gratiot","Hillsdale","Houghton","Huron","Ingham",
  "Ionia","Iosco","Iron","Isabella","Jackson","Kalamazoo","Kalkaska","Keweenaw","Lake","Lapeer","Leelanau",
  "Lenawee","Livingston","Luce","Mackinac","Macomb","Manistee","Marquette","Mason","Mecosta","Menominee",
  "Midland","Missaukee","Monroe","Montcalm","Montmorency","Muskegon","Newaygo","Oceana","Ogemaw","Ontonagon",
  "Osceola","Oscoda","Otsego","Presque Isle","Roscommon","Saginaw","Sanilac","Schoolcraft","Shiawassee",
  "St. Clair","St. Joseph","Tuscola","Van Buren","Wayne","Wexford"
)

waived_2018_2019 <- c(
  "Alcona","Alger","Alpena","Antrim","Arenac","Baraga","Bay","Benzie","Branch","Calhoun","Cass","Charlevoix",
  "Cheboygan","Chippewa","Clare","Crawford","Delta","Dickinson","Emmet","Genesee","Gladwin","Gogebic","Gratiot",
  "Hillsdale","Houghton","Huron","Iosco","Iron","Isabella","Jackson","Kalkaska","Keweenaw","Lake","Lapeer",
  "Leelanau","Lenawee","Luce","Mackinac","Macomb","Manistee","Marquette","Mason","Mecosta","Menominee","Midland",
  "Missaukee","Monroe","Montcalm","Montmorency","Muskegon","Newaygo","Oceana","Ogemaw","Ontonagon","Osceola",
  "Oscoda","Otsego","Presque Isle","Roscommon","Saginaw","Sanilac","Schoolcraft","Shiawassee","St. Clair",
  "St. Joseph","Tuscola","Van Buren","Wayne","Wexford"
)

waived_2020_feb_mar <- c(
  "Alcona","Alger","Alpena","Antrim","Arenac","Baraga","Barry","Bay","Benzie","Berrien","Branch","Calhoun","Cass",
  "Charlevoix","Cheboygan","Chippewa","Clare","Clinton","Crawford","Delta","Dickinson","Eaton","Emmet","Genesee",
  "Gladwin","Gogebic","Grand Traverse","Gratiot","Hillsdale","Houghton","Huron","Ingham","Ionia","Iosco","Iron",
  "Isabella","Jackson","Kalamazoo","Kalkaska","Keweenaw","Lake","Lapeer","Leelanau","Lenawee","Luce","Mackinac",
  "Macomb","Manistee","Marquette","Mason","Mecosta","Menominee","Midland","Missaukee","Monroe","Montcalm",
  "Montmorency","Muskegon","Newaygo","Oceana","Ogemaw","Ontonagon","Osceola","Oscoda","Otsego","Presque Isle",
  "Roscommon","Saginaw","Sanilac","Schoolcraft","Shiawassee","St. Clair","St. Joseph","Tuscola","Van Buren",
  "Wayne","Wexford"
)

# -------- 2018 monthly phase-in (requested assumption) --------
waived_2018_jan_jun <- setdiff(waived_2018_2019, "Wayne")  # 68 counties
waived_2018_jul_dec <- waived_2018_2019                     # 69 counties

# Optional monthly override file: year,month,county (waived list rows)
override_path <- file.path(root, "data_raw", "waiver_overrides_2018.csv")
has_override  <- file.exists(override_path)
if (has_override) {
  ov <- read_csv(override_path, show_col_types = FALSE) %>%
    mutate(county = str_squish(str_to_title(str_remove(county, "\\s*County\\b"))))
  message("Using monthly overrides from: ", override_path)
}

# ---------------- Flag waived/enforced (monthly) -------------------
out <- data %>%
  mutate(
    waived = case_when(
      ym >= ymd("2016-01-01") & ym <= ymd("2016-12-01") ~ 1L,  # 2016 statewide waived
      ym >= ymd("2017-01-01") & ym <= ymd("2017-12-01") & county %in% waived_2017 ~ 1L,
      ym >= ymd("2018-01-01") & ym <= ymd("2018-06-01") & county %in% waived_2018_jan_jun ~ 1L,
      ym >= ymd("2018-07-01") & ym <= ymd("2018-12-01") & county %in% waived_2018_jul_dec ~ 1L,
      ym >= ymd("2019-01-01") & ym <= ymd("2019-12-01") & county %in% waived_2018_2019 ~ 1L,
      ym >= ymd("2020-02-01") & ym <= ymd("2020-03-01") & county %in% waived_2020_feb_mar ~ 1L,
      ym >= ymd("2020-08-01") ~ 1L,  # 2020-08+ statewide waived
      TRUE ~ 0L
    )
  )

# Apply optional overrides (authoritative)
if (has_override) {
  out <- out %>%
    left_join(ov %>% mutate(flag_ov = 1L), by = c("year","month","county")) %>%
    mutate(waived = ifelse(!is.na(flag_ov), 1L, waived)) %>%
    select(-flag_ov)
}
out <- out %>% mutate(enforced = 1L - waived)

# ---------------- Monthly audit (expectations) ----------------
audit <- out %>%
  filter(year %in% 2016:2020) %>%
  group_by(year, month) %>%
  summarise(n_waived = sum(waived, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    ym = make_date(year, month, 1),
    expected = case_when(
      ym >= ymd("2016-01-01") & ym <= ymd("2016-12-01") ~ 83L,
      ym >= ymd("2017-01-01") & ym <= ymd("2017-12-01") ~ 79L,
      ym >= ymd("2018-01-01") & ym <= ymd("2018-06-01") ~ 68L,
      ym >= ymd("2018-07-01") & ym <= ymd("2018-12-01") ~ 69L,
      ym >= ymd("2019-01-01") & ym <= ymd("2019-12-01") ~ 69L,
      ym >= ymd("2020-02-01") & ym <= ymd("2020-03-01") ~ 77L,
      ym >= ymd("2020-08-01") ~ 83L,
      TRUE ~ NA_integer_
    ),
    ok = is.na(expected) | (n_waived == expected)
  )

bad_months <- audit %>% filter(!ok)
if (nrow(bad_months)) {
  warning(sprintf("Monthly waiver counts mismatch for %d months; wrote: %s",
                  nrow(bad_months), out_audit_bad))
  write_csv(bad_months, out_audit_bad)
}
print(audit %>% arrange(year, month) %>% filter(year == 2018))
message(sprintf("Audit mismatches: %d", sum(!audit$ok, na.rm = TRUE)))

# ---------------- Save policy-coded monthly (no column drop) ------
write_csv(out, out_policy)
message("✅ Wrote policy-coded monthly: ", out_policy)

# ==========================================================
# Build panel with G (first month with enforced==1) + all Y
# ==========================================================
panel0 <- out %>%
  mutate(
    county    = str_squish(str_to_title(str_remove(county, "\\s*County\\b"))),
    ym_date   = make_date(year, month, 1),
    county_id = if ("county_id" %in% names(.)) sprintf("%05d", as.integer(county_id)) else NA_character_
  ) %>%
  distinct(county, year, month, .keep_all = TRUE)

has_fips <- "county_id" %in% names(panel0)

# Resolve core columns for outcomes
recip_col <- RECIP_CANDIDATES[RECIP_CANDIDATES %in% names(panel0)][1]
if (is.na(recip_col)) {
  stop(sprintf("No recipients column found. Tried: %s", paste(RECIP_CANDIDATES, collapse = ", ")))
}
pop_total_col <- POP_TOTAL_CANDIDATES[POP_TOTAL_CANDIDATES %in% names(panel0)][1]
pop_18_49_col <- POP_18_49_CANDIDATES[POP_18_49_CANDIDATES %in% names(panel0)][1]
labor_force_col <- LABOR_FORCE_CANDS[LABOR_FORCE_CANDS %in% names(panel0)][1]

exits_col   <- EXITS_CANDIDATES[EXITS_CANDIDATES %in% names(panel0)][1]
entries_col <- ENTRIES_CANDIDATES[ENTRIES_CANDIDATES %in% names(panel0)][1]

# Build G table
G_table <- panel0 %>%
  mutate(id_key = if (has_fips) sprintf("%05d", as.integer(county_id)) else county) %>%
  group_by(id_key) %>%
  summarise(
    G_date = suppressWarnings({
      g_first <- min(ym_date[enforced == 1], na.rm = TRUE)
      if (!is.finite(g_first)) NA_Date_ else g_first
    }),
    .groups = "drop"
  ) %>%
  mutate(G = ifelse(is.na(G_date), "0", format(G_date, "%Y-%m"))) %>%
  select(id_key, G)

# Build full panel (keep all covariates)
panel <- panel0 %>%
  mutate(
    id   = if (has_fips) sprintf("%05d", as.integer(county_id)) else county,
    date = ym_date,
    
    # Base measures
    y_raw = .data[[recip_col]],
    
    # Per-capita intensities (guard against zero/NA denominators)
    y_per1k_total  = if (!is.na(pop_total_col))  ifelse(.data[[pop_total_col]]  > 0, 1000 * (.data[[recip_col]] / .data[[pop_total_col]]),  NA_real_) else NA_real_,
    y_per1k_18_49  = if (!is.na(pop_18_49_col))  ifelse(.data[[pop_18_49_col]]  > 0, 1000 * (.data[[recip_col]] / .data[[pop_18_49_col]]),  NA_real_) else NA_real_,
    y_per100_lf    = if (!is.na(labor_force_col)) ifelse(.data[[labor_force_col]]> 0, 100 * (.data[[recip_col]] / .data[[labor_force_col]]), NA_real_) else NA_real_,
    
    # Log transforms (use log1p to avoid issues at zero)
    y_log1p_raw           = log1p(.data[["y_raw"]]),
    y_log1p_per1k_total   = log1p(.data[["y_per1k_total"]]),
    y_log1p_per1k_18_49   = log1p(.data[["y_per1k_18_49"]]),
    y_log1p_per100_lf     = log1p(.data[["y_per100_lf"]])
  ) %>%
  arrange(id, date) %>%
  group_by(id) %>%
  # Dynamics using lags; safe proxies if exits/entries not present
  mutate(
    recip_lag         = dplyr::lag(y_raw, 1),
    net_change        = y_raw - recip_lag,
    net_change_rate   = ifelse(!is.na(recip_lag) & recip_lag > 0, net_change / recip_lag, NA_real_),
    
    # Exit rate proxy: if no 'exits' column, use negative part of net change / lagged recipients
    exits_used        = if (!is.na(exits_col)) .data[[exits_col]] else pmax(0, -net_change),
    exit_rate_proxy   = ifelse(!is.na(recip_lag) & recip_lag > 0, exits_used / recip_lag, NA_real_),
    
    # Entry rate proxy (optional; may help decomposition)
    entries_used      = if (!is.na(entries_col)) .data[[entries_col]] else pmax(0, net_change),
    entry_rate_proxy  = ifelse(!is.na(recip_lag) & recip_lag > 0, entries_used / recip_lag, NA_real_)
  ) %>%
  ungroup() %>%
  # Attach G
  left_join(G_table %>% mutate(id_key = as.character(id_key)), by = c("id" = "id_key")) %>%
  mutate(G = ifelse(is.na(G), "0", G))

# ---------------- Choose outcome_final for panel_input ----------
# Map Y_MODE to a computed column name
y_map <- c(
  raw               = "y_raw",
  per1k_total       = "y_per1k_total",
  per1k_18_49       = "y_per1k_18_49",
  per100_lf         = "y_per100_lf",
  log1p_raw         = "y_log1p_raw",
  log1p_per1k_total = "y_log1p_per1k_total",
  log1p_per1k_18_49 = "y_log1p_per1k_18_49",
  log1p_per100_lf   = "y_log1p_per100_lf",
  exit_rate_proxy   = "exit_rate_proxy",
  net_change_rate   = "net_change_rate"
)
if (!(Y_MODE %in% names(y_map))) stop("Y_MODE not recognized. Choose one of: ", paste(names(y_map), collapse = ", "))
y_col <- y_map[[Y_MODE]]

panel <- panel %>%
  mutate(outcome_final = .data[[y_col]])

# ---------------- Quick audits -----------------------------------
dup_chk <- panel %>% count(id, year, month) %>% filter(n > 1)
if (nrow(dup_chk) > 0) {
  write_csv(dup_chk, file.path(out_dir, "dup_rows_check.csv"))
  warning("Found duplicated id-year-month rows. See dup_rows_check.csv")
}

coh <- panel %>%
  filter(G != "0") %>%
  count(G, name = "n_ids") %>%
  arrange(G)

support_snapshot <- panel %>%
  mutate(
    t_ym = lubridate::year(date) * 12L + lubridate::month(date),
    G_ym = ifelse(G == "0", NA_integer_,
                  as.integer(substr(G,1,4)) * 12L + as.integer(substr(G,6,7)))
  ) %>%
  group_by(date) %>%
  summarise(
    n_treated = n_distinct(id[!is.na(G_ym) & t_ym >= G_ym]),
    n_control = n_distinct(id[is.na(G_ym) | t_ym < G_ym]),
    .groups = "drop"
  ) %>%
  arrange(date)

# ---------------- Save panel artifacts ---------------------------
# Full panel with ALL covariates + G + all computed Y
write_csv(panel, out_panelG)
write_csv(coh,   out_cohort)
write_csv(support_snapshot, out_support)

# Lean DID input
panel_input <- panel %>%
  transmute(
    id,
    date    = format(date, "%Y-%m-%d"),
    outcome = outcome_final,
    G
  )
write_csv(panel_input, out_panel_input)

# Outcomes-only helper: id/date + all Y columns (for quick switching)
outcomes_wide <- panel %>%
  select(
    id, county, year, month, date, G,
    y_raw,
    y_per1k_total, y_per1k_18_49, y_per100_lf,
    y_log1p_raw, y_log1p_per1k_total, y_log1p_per1k_18_49, y_log1p_per100_lf,
    exit_rate_proxy, entry_rate_proxy, net_change_rate
  )
write_csv(outcomes_wide, out_outcomeswide)

cat("=== BUILD PANEL WITH G + MULTI-Y: DONE ===\n")
cat("Main outputs:\n")
cat(" - ", out_panelG, "\n")
cat(" - ", out_panel_input, " [Y_MODE = ", Y_MODE, "]\n", sep = "")
cat(" - ", out_outcomeswide, " (helper)\n", sep = "")
cat("QC artifacts:\n")
cat(" - ", out_cohort, "\n")
cat(" - ", out_support, "\n")
if (nrow(dup_chk) > 0) {
  cat(" - ", file.path(out_dir, "dup_rows_check.csv"), " (duplicates found)\n")
}
