# R/02_abawd/01_construct_event_time.R
# Input:  data/derived/panel_analysis.rds, data/raw/abawd_waiver_timing.csv (optional)
# Output: data/derived/panel_abawd_event.rds — panel with event_time, G_int, etc.

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)

panel <- readRDS(file.path(DIR_DERIVED, "panel_analysis.rds"))
if (!"G" %in% names(panel)) stop("panel_analysis must contain G (cohort month). Run 01_build/03_merge_panel.R.")

panel <- panel %>%
  mutate(
    date = as.Date(date),
    t = lubridate::year(date) * 12L + lubridate::month(date),
    id_num = as.integer(factor(id)),
    G_int = case_when(
      is.na(G) ~ 0L,
      G == "0" ~ 0L,
      TRUE ~ {
        g_chr <- as.character(G)
        yy <- as.integer(substr(g_chr, 1, 4))
        mm <- as.integer(substr(g_chr, 6, 7))
        ifelse(is.na(yy) | is.na(mm), 0L, yy * 12L + mm)
      }
    ),
    event_time = t - G_int
  )

saveRDS(panel, file.path(DIR_DERIVED, "panel_abawd_event.rds"))
message("Wrote data/derived/panel_abawd_event.rds  nrow = ", nrow(panel))
