# R/04_ea/02_event_study_participation.R
# Input:  data_clean/fap_panel_derived.csv, EA end date (config/globals or 2023-03-01)
# Output: outputs/figures/ea_es_participation.png, data/derived/ea_participation_es.rds
# Event-study: participation (log recipients or cases) around EA end.

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)
if (!exists("PATH_EA_POLICY")) source("config/globals.R", local = TRUE)

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
    id_num = as.integer(factor(county)),
    event_time_raw = as.integer(round((as.numeric(date) - as.numeric(ea_end_date)) / 30.44))
  )
# Outcome: log(1+recipients) or ln_recipients
if ("ln_recipients" %in% names(fap)) {
  fap <- fap %>% mutate(Y = as.numeric(ln_recipients))
} else {
  fap <- fap %>% mutate(Y = log1p(as.numeric(recipients)))
}

MIN_EV <- -12L
MAX_EV <- 12L
panel <- fap %>%
  filter(event_time_raw >= MIN_EV, event_time_raw <= MAX_EV, is.finite(Y)) %>%
  mutate(event_time = pmin(pmax(event_time_raw, MIN_EV), MAX_EV))

mod <- fixest::feols(Y ~ i(event_time, ref = -1) | id_num, data = panel, cluster = ~ id_num)
b <- coef(mod)
nms <- names(b)[grepl("^event_time::", names(b))]
ct <- summary(mod)$coeftable
ev_vals <- as.integer(sub("event_time::", "", nms))

es_df <- data.frame(
  event_time = ev_vals,
  estimate   = as.numeric(b[nms]),
  se         = as.numeric(ct[nms, "Std. Error"]),
  stringsAsFactors = FALSE
) %>%
  mutate(ci_lower = estimate - 1.96 * se, ci_upper = estimate + 1.96 * se) %>%
  arrange(event_time)

ea_part_es <- list(es_df = es_df, ea_end_date = ea_end_date, outcome = "log(recipients)", model = mod)
saveRDS(ea_part_es, file.path(DIR_DERIVED, "ea_participation_es.rds"))
message("Wrote data/derived/ea_participation_es.rds")

p <- ggplot2::ggplot(es_df, ggplot2::aes(x = event_time, y = estimate)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lower, ymax = ci_upper), fill = "gray85", alpha = 0.7) +
  ggplot2::geom_line(linewidth = 1.2, color = "#0072B2") +
  ggplot2::geom_point(size = 2.6, color = "#0072B2") +
  ggplot2::geom_vline(xintercept = 0, color = "black", linewidth = 0.9) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  ggplot2::labs(
    title = "Event-study: Participation (log recipients) around EA end",
    subtitle = paste0("EA end = ", ea_end_date, ". Reference = month -1. County FE."),
    x = "Months relative to EA end",
    y = "Coefficient"
  ) +
  ggplot2::theme_bw(base_size = 14) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), plot.title = ggplot2::element_text(face = "bold"))
ggplot2::ggsave(file.path(DIR_OUT_FIGURES, "ea_es_participation.png"), p, width = 8, height = 5, dpi = 300)
message("Wrote outputs/figures/ea_es_participation.png")
