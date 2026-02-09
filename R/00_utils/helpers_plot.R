# R/00_utils/helpers_plot.R — Shared plot theme (extensible)
# Input:  none
# Output: none

theme_paper <- function(base_size = 12) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "gray95")
    )
}
