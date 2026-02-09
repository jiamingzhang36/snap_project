# config/paths.R — Central paths and global params
# Source at top of any script to get ROOT and dirs

if (exists("ROOT") && nzchar(ROOT)) {
  # Already set (e.g. for tests)
} else {
  ROOT <- normalizePath(
    if (file.exists("config/paths.R")) "." else "..",
    winslash = "/",
    mustWork = TRUE
  )
}

# Data dirs (aligned with recommended structure; compatible with existing data_clean)
DIR_RAW      <- file.path(ROOT, "data", "raw")
DIR_EXTERNAL <- file.path(ROOT, "data", "external")
DIR_DERIVED  <- file.path(ROOT, "data", "derived")
# Existing cleaned outputs (used during migration)
DIR_DATA_CLEAN <- file.path(ROOT, "data_clean")

# Output dirs
DIR_OUT_TABLES <- file.path(ROOT, "outputs", "tables")
DIR_OUT_FIGURES <- file.path(ROOT, "outputs", "figures")
DIR_OUT_MAPS   <- file.path(ROOT, "outputs", "maps")
DIR_OUT_LOGS   <- file.path(ROOT, "outputs", "logs")

# Create if missing
for (d in c(DIR_RAW, DIR_EXTERNAL, DIR_DERIVED, DIR_OUT_TABLES, DIR_OUT_FIGURES, DIR_OUT_MAPS, DIR_OUT_LOGS)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# Analysis window (adjust for paper)
WINDOW_START <- as.Date("2014-01-01")
WINDOW_END   <- as.Date("2019-12-01")
REF_YEAR     <- 2019L
