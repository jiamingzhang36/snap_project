# R/00_utils/helpers_forecast.R — 2026 OBBA scenario helpers
# Parametric bootstrap for uncertainty intervals on county-level projections.

#' Bootstrap ATT draws and propagate to county-level scenarios
#'
#' @param county_baseline data.frame with county-level baseline outcomes
#' @param did_att  numeric, point estimate of ABAWD ATT (log scale)
#' @param did_se   numeric, standard error of ATT
#' @param share_55_64 numeric vector (length = nrow(county_baseline)) or scalar
#' @param n_boot   number of bootstrap draws
#' @param seed     random seed
#' @return list with: point_estimates, boot_matrix (n_county x n_boot of delta_log),
#'         county_ci (data.frame with lo/hi), rank_ci (data.frame with median rank + interval),
#'         aggregate_ci (state-level total lo/hi)
bootstrap_scenarios <- function(county_baseline, did_att, did_se, share_55_64,
                                n_boot = 2000, seed = 42) {
  set.seed(seed)

  n_county <- nrow(county_baseline)
  if (length(share_55_64) == 1) share_55_64 <- rep(share_55_64, n_county)

  # Draw ATT from N(att, se^2)
  att_draws <- rnorm(n_boot, mean = did_att, sd = did_se)

  # County x boot matrix of delta_log = ATT_b * share_55_64_c
  boot_delta <- outer(share_55_64, att_draws)  # n_county x n_boot

  # Point estimates
  delta_point <- did_att * share_55_64

  # County-level CI (percentile method)
  county_ci <- data.frame(
    id       = county_baseline$id,
    delta_lo = apply(boot_delta, 1, quantile, probs = 0.05, na.rm = TRUE),
    delta_hi = apply(boot_delta, 1, quantile, probs = 0.95, na.rm = TRUE)
  )

  # Rank with uncertainty
  rank_ci <- rank_with_uncertainty(boot_delta, county_baseline$id)

  # State-level aggregate: sum of level changes
  if ("pop_18_49" %in% names(county_baseline) && any(!is.na(county_baseline$pop_18_49))) {
    pop <- county_baseline$pop_18_49
    pop[is.na(pop)] <- 0

    # For each bootstrap draw, compute state-level total loss
    boot_loss <- sapply(seq_len(n_boot), function(b) {
      obba_log <- county_baseline$baseline_outcome_log + boot_delta[, b]
      obba_level <- expm1(obba_log)
      base_level <- expm1(county_baseline$baseline_outcome_log)
      sum((obba_level - base_level) * pop / 1000, na.rm = TRUE)
    })

    aggregate_ci <- data.frame(
      metric   = c("point", "lo_90", "hi_90"),
      value    = c(
        mean(boot_loss, na.rm = TRUE),
        quantile(boot_loss, 0.05, na.rm = TRUE),
        quantile(boot_loss, 0.95, na.rm = TRUE)
      )
    )
  } else {
    aggregate_ci <- data.frame(metric = "point", value = NA_real_)
  }

  list(
    delta_point  = delta_point,
    boot_delta   = boot_delta,
    county_ci    = county_ci,
    rank_ci      = rank_ci,
    aggregate_ci = aggregate_ci
  )
}


#' Compute rank with uncertainty from bootstrap matrix
#'
#' @param boot_matrix n_county x n_boot matrix of delta_log values
#' @param county_ids  character vector of county IDs
#' @return data.frame with id, rank_median, rank_lo, rank_hi (90% interval)
rank_with_uncertainty <- function(boot_matrix, county_ids) {
  n_boot <- ncol(boot_matrix)

  # For each draw, rank counties by delta_log (most negative = rank 1 = most impacted)
  rank_matrix <- apply(boot_matrix, 2, function(col) rank(col, ties.method = "average"))

  data.frame(
    id          = county_ids,
    rank_median = apply(rank_matrix, 1, median, na.rm = TRUE),
    rank_lo     = apply(rank_matrix, 1, quantile, probs = 0.05, na.rm = TRUE),
    rank_hi     = apply(rank_matrix, 1, quantile, probs = 0.95, na.rm = TRUE)
  )
}
