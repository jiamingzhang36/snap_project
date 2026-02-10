# R/03_income/03_figures_tables.R
# Input:  data/derived/unemp_dl_model.rds
# Output: outputs/figures/income_dl_recipients.png (DL dynamic response)

source("config/paths.R", local = TRUE)
source("R/00_utils/packages.R", local = TRUE)

dl_model <- readRDS(file.path(DIR_DERIVED, "unemp_dl_model.rds"))
if (!dir.exists(DIR_OUT_FIGURES)) dir.create(DIR_OUT_FIGURES, recursive = TRUE)

df <- dl_model$coefs

p <- ggplot2::ggplot(df, ggplot2::aes(x = lag, y = estimate)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lower, ymax = ci_upper), fill = "gray85", alpha = 0.7) +
  ggplot2::geom_line(linewidth = 1.2, color = "#0072B2") +
  ggplot2::geom_point(size = 2.6, color = "#0072B2") +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  ggplot2::labs(
    title = "Distributed-lag: Unemployment → SNAP (log 1+ recipients per 1k, 18–49)",
    subtitle = paste0("Lags 0–", dl_model$lag_max, " of unemployment rate; county + month FE. N = ", dl_model$n_obs),
    x = "Lag (months)",
    y = "Coefficient"
  ) +
  ggplot2::theme_bw(base_size = 14) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(face = "bold")
  )

path_fig <- file.path(DIR_OUT_FIGURES, "income_dl_recipients.png")
ggplot2::ggsave(path_fig, p, width = 8, height = 5, dpi = 300)
message("Wrote ", path_fig)
