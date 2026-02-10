# R/04_ea/01_event_study_avg_benefit.R
# Input:  data_clean/fap_panel_derived.csv (to 2025), config/globals.R or data/raw/ea_policy_dates.csv (EA end date)
# Output: outputs/figures/ea_es_avgpp.png, data/derived/ea_avgpp_es.rds
# Event-study: average benefit per person (or per case) around EA end (national date).

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)
if (!exists("PATH_EA_POLICY")) source("config/globals.R", local = TRUE)

# EA end date (national)
ea_end_date <- as.Date("2023-03-01")
if (exists("ea_policy_dates") && nrow(ea_policy_dates) > 0) {
  idx <- which(ea_policy_dates$event == "EA_end")
  if (length(idx) > 0) ea_end_date <- as.Date(ea_policy_dates$date[idx[1]])
}

PATH_FAP <- file.path(DIR_DATA_CLEAN, "fap_panel_derived.csv")
stopifnot(file.exists(PATH_FAP))
if (!dir.exists(DIR_OUT_FIGURES)) dir.create(DIR_OUT_FIGURES, recursive = TRUE)

fap <- readr::read_csv(PATH_FAP, show_col_types = FALSE)
fap <- fap %>%
  mutate(
    date = as.Date(paste(year, month, "01", sep = "-")),
    id = as.character(county),
    id_num = as.integer(factor(county)),
    event_time_raw = as.integer(round((as.numeric(date) - as.numeric(ea_end_date)) / 30.44))
  )
# Outcome: average per person (dollars). Column name may have space.
avgpp_col <- NULL
if ("average per person" %in% names(fap)) avgpp_col <- "average per person"
if (is.null(avgpp_col) && "average_per_person" %in% names(fap)) avgpp_col <- "average_per_person"
if (is.null(avgpp_col)) {
  if ("average per case" %in% names(fap)) avgpp_col <- "average per case"
}
stopifnot(!is.null(avgpp_col))
fap <- fap %>% mutate(Y = as.numeric(.data[[avgpp_col]]))

# Restrict to event window -12 to +12 months
MIN_EV <- -12L
MAX_EV <- 12L
panel <- fap %>%
  filter(event_time_raw >= MIN_EV, event_time_raw <= MAX_EV, is.finite(Y)) %>%
  mutate(event_time = pmin(pmax(event_time_raw, MIN_EV), MAX_EV))

# Event-study: Y ~ i(event_time, ref=-1) | county
mod <- fixest::feols(Y ~ i(event_time, ref = -1) | id_num, data = panel, cluster = ~ id_num)
b <- coef(mod)
nms <- names(b)[grepl("^event_time::", names(b))]
ev_vals <- as.integer(gsub("event_time::", "", nms))
ev_vals <- ev_vals[!is.na(ev_vals)]
ct <- summary(mod)$coeftable
se <- setNames(as.numeric(ct[nms, "Std. Error"]), nms)

es_df <- data.frame(
  event_time = ev_vals,
  estimate   = as.numeric(b[nms]),
  se         = as.numeric(ct[nms, "Std. Error"]),
  stringsAsFactors = FALSE
) %>%
  mutate(ci_lower = estimate - 1.96 * se, ci_upper = estimate + 1.96 * se) %>%
  arrange(event_time)

# Save
ea_avgpp_es <- list(es_df = es_df, ea_end_date = ea_end_date, outcome = avgpp_col, model = mod)
saveRDS(ea_avgpp_es, file.path(DIR_DERIVED, "ea_avgpp_es.rds"))
message("Wrote data/derived/ea_avgpp_es.rds")

# Plot
p <- ggplot2::ggplot(es_df, ggplot2::aes(x = event_time, y = estimate)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lower, ymax = ci_upper), fill = "gray85", alpha = 0.7) +
  ggplot2::geom_line(linewidth = 1.2, color = "#0072B2") +
  ggplot2::geom_point(size = 2.6, color = "#0072B2") +
  ggplot2::geom_vline(xintercept = 0, color = "black", linewidth = 0.9) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  ggplot2::labs(
    title = "Event-study: Average benefit per person around EA end",
    subtitle = paste0("EA end = ", ea_end_date, ". Reference = month -1. County FE."),
    x = "Months relative to EA end",
    y = "Coefficient (avg $ per person)"
  ) +
  ggplot2::theme_bw(base_size = 14) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), plot.title = ggplot2::element_text(face = "bold"))
ggplot2::ggsave(file.path(DIR_OUT_FIGURES, "ea_es_avgpp.png"), p, width = 8, height = 5, dpi = 300)
message("Wrote outputs/figures/ea_es_avgpp.png")
