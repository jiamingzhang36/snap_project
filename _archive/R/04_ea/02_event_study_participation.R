# R/04_ea/02_event_study_participation.R
# Input:  data_clean/fap_panel_derived.csv, EA end date (config/globals or 2023-03-01)
# Output: outputs/figures/ea_es_participation.png, data/derived/ea_participation_es.rds
# Main: intensity DID with date FE — Y ~ i(event_time, ref=-1) * high_intensity | id_num + date.
#   Plots high-minus-low path (differential response by pre-EA benefit intensity). Appendix: descriptive ES in ea_es_participation_descriptive.png.

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)
if (!exists("PATH_EA_POLICY")) source("config/globals.R", local = TRUE)

ea_end_date <- as.Date("2023-03-01")
if (exists("ea_policy_dates") && nrow(ea_policy_dates) > 0) {
  idx <- which(ea_policy_dates$event == "EA_end")
  if (length(idx) > 0) ea_end_date <- as.Date(ea_policy_dates$date[idx[1]])
}
ea_ym <- as.integer(lubridate::year(ea_end_date)) * 12L + as.integer(lubridate::month(ea_end_date))

PATH_FAP <- file.path(DIR_DATA_CLEAN, "fap_panel_derived.csv")
stopifnot(file.exists(PATH_FAP))
if (!dir.exists(DIR_OUT_FIGURES)) dir.create(DIR_OUT_FIGURES, recursive = TRUE)
if (!dir.exists(DIR_DERIVED)) dir.create(DIR_DERIVED, recursive = TRUE)

fap <- readr::read_csv(PATH_FAP, show_col_types = FALSE)
fap <- fap %>%
  mutate(
    date = as.Date(paste(year, month, "01", sep = "-")),
    id = as.character(county),
    id_num = as.integer(factor(county)),
    event_time_raw = as.integer(year) * 12L + as.integer(month) - ea_ym
  )
if ("ln_recipients" %in% names(fap)) {
  fap <- fap %>% mutate(Y = as.numeric(ln_recipients))
} else {
  fap <- fap %>% mutate(Y = log1p(as.numeric(recipients)))
}

# Pre-EA avg benefit per county = intensity (months -12 to -1)
avgpp_col <- NULL
if ("average per person" %in% names(fap)) avgpp_col <- "average per person"
if (is.null(avgpp_col) && "average_per_person" %in% names(fap)) avgpp_col <- "average_per_person"
if (is.null(avgpp_col) && "average per case" %in% names(fap)) avgpp_col <- "average per case"
stopifnot(!is.null(avgpp_col))
pre <- fap %>%
  filter(event_time_raw >= -12L, event_time_raw <= -1L) %>%
  mutate(avg_pp = as.numeric(.data[[avgpp_col]]))
county_intensity <- pre %>%
  filter(is.finite(avg_pp)) %>%
  group_by(id) %>%
  summarise(avg_benefit_pre = mean(avg_pp, na.rm = TRUE), .groups = "drop")
med_int <- median(county_intensity$avg_benefit_pre, na.rm = TRUE)
county_intensity <- county_intensity %>%
  mutate(high_intensity = as.integer(avg_benefit_pre > med_int))

MIN_EV <- -12L
MAX_EV <- 12L
panel <- fap %>%
  filter(event_time_raw >= MIN_EV, event_time_raw <= MAX_EV, is.finite(Y)) %>%
  mutate(event_time = pmin(pmax(event_time_raw, MIN_EV), MAX_EV)) %>%
  left_join(county_intensity %>% select(id, high_intensity), by = "id") %>%
  filter(!is.na(high_intensity))

# Main: differential path (high − low) with date FE. Only interaction (high_intensity absorbed by county FE).
mod <- fixest::feols(
  Y ~ i(event_time, ref = -1) : high_intensity | id_num + date,
  data = panel,
  cluster = ~ id_num
)
b <- coef(mod)
# Interaction terms: event_time::k:high_intensity => high minus low at k
nms_int <- names(b)[grepl("^event_time::.*:high_intensity", names(b))]
ev_vals <- as.integer(gsub("^event_time::(-?[0-9]+):high_intensity$", "\\1", nms_int))
es_df <- data.frame(
  event_time = ev_vals,
  estimate   = as.numeric(b[nms_int]),
  se         = as.numeric(summary(mod)$coeftable[nms_int, "Std. Error"]),
  stringsAsFactors = FALSE
) %>%
  mutate(ci_lower = estimate - 1.96 * se, ci_upper = estimate + 1.96 * se) %>%
  arrange(event_time)

ea_part_es <- list(
  es_df = es_df, ea_end_date = ea_end_date,
  outcome = "log(recipients)", model = mod,
  note = "Intensity DID: high minus low at each event_time; county + date FE"
)
saveRDS(ea_part_es, file.path(DIR_DERIVED, "ea_participation_es.rds"))
message("Wrote data/derived/ea_participation_es.rds (intensity DID + date FE)")

p <- ggplot2::ggplot(es_df, ggplot2::aes(x = event_time, y = estimate)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lower, ymax = ci_upper), fill = "gray85", alpha = 0.7) +
  ggplot2::geom_line(linewidth = 1.2, color = "#0072B2") +
  ggplot2::geom_point(size = 2.6, color = "#0072B2") +
  ggplot2::geom_vline(xintercept = 0, color = "black", linewidth = 0.9) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  ggplot2::labs(
    title = "EA end: Participation (log recipients) — high vs low pre-EA benefit intensity",
    subtitle = paste0("DID (high − low) by event month. County + month FE. Ref = -1. EA end = ", ea_end_date),
    x = "Months relative to EA end",
    y = "Coefficient (high − low)"
  ) +
  ggplot2::theme_bw(base_size = 14) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), plot.title = ggplot2::element_text(face = "bold"))
ggplot2::ggsave(file.path(DIR_OUT_FIGURES, "ea_es_participation.png"), p, width = 8, height = 5, dpi = 300)
message("Wrote outputs/figures/ea_es_participation.png")
