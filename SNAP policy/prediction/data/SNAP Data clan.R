# install.packages(c("readr","data.table","dplyr"))  # run once

library(readr); library(data.table); library(dplyr)

base_dir <- "/Users/jiamingzhang/Desktop/SNAP policy/prediction/data/SNAP COVID Policy and Enrollment Data/DS1 SNAP COVID Policy Data/DS0001"

# find a data file: prefer .tsv/.csv; fall back to .txt
files <- list.files(base_dir, pattern = "(?i)Data\\.(tsv|csv|txt)$", full.names = TRUE)
if (length(files) == 0) stop("No data file (*.tsv/csv/txt) found in DS0001.")
data_path <- files[1]
cat("Reading file:", data_path, "\n")

# read preview (first ~2k rows)
if (grepl("\\.tsv$", data_path, ignore.case = TRUE)) {
  df <- read_tsv(data_path, n_max = 2000, progress = FALSE)
} else if (grepl("\\.csv$", data_path, ignore.case = TRUE)) {
  df <- read_csv(data_path, n_max = 2000, progress = FALSE)
} else {
  df <- fread(data_path, sep = "\t", nrows = 2000, data.table = FALSE, showProgress = FALSE)
}

cat("Preview dim:", nrow(df), "x", ncol(df), "\n")
print(head(df, 5))
cat("\nFirst 30 columns:\n"); print(names(df)[1:min(30, ncol(df))])
cat("\nKey column candidates:",
    paste(intersect(names(df), c("year","month","date","state","state_fips")), collapse=", "), "\n")
