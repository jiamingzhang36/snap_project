# R/00_utils/helpers_forecast.R — 2026 OBBA scenario helpers
# Parametric bootstrap for uncertainty intervals on county-level projections.

#' Bootstrap ATT draws and propagate to county-level scenarios
#'
#' Fixed version: draws both overall ATT uncertainty AND gamma (heterogeneity)
#' uncertainty jointly, so county-level ranks vary across bootstrap draws.
#'
#' @param county_baseline data.frame with county-level baseline outcomes
#' @param did_att  numeric, point estimate of overall ABAWD ATT (log scale)
#' @param did_se   numeric, standard error of overall ATT
#' @param share_55_64 numeric vector (length = nrow(county_baseline)) or scalar
#' @param att_county  numeric vector of county-specific ATT (optional; defaults to uniform did_att)
#' @param het_boot_info list from 04_heterogeneity.R with gamma_hat, gamma_vcov, z_matrix, county_ids
#' @param n_boot   number of bootstrap draws
#' @param seed     random seed
#' @return list with: point_estimates, boot_matrix (n_county x n_boot of delta_log),
#'         county_ci (data.frame with lo/hi), rank_ci (data.frame with median rank + interval),
#'         aggregate_ci (state-level total lo/hi)
bootstrap_scenarios <- function(county_baseline, did_att, did_se, share_55_64,
                                att_county = NULL, het_boot_info = NULL,
                                n_boot = 2000, seed = 42) {
  set.seed(seed)

  n_county <- nrow(county_baseline)
  if (length(share_55_64) == 1) share_55_64 <- rep(share_55_64, n_county)

  # County-specific ATT: if not provided, use uniform
  if (is.null(att_county)) {
    att_county <- rep(did_att, n_county)
  }

  # ---- Draw bootstrap ATT for each county ----
  att_draws_overall <- rnorm(n_boot, mean = did_att, sd = did_se)

  if (!is.null(het_boot_info) &&
      !is.null(het_boot_info$gamma_hat) &&
      length(het_boot_info$gamma_hat) > 0) {
    # --- Full bootstrap: overall ATT + gamma heterogeneity ---
    gamma_hat  <- het_boot_info$gamma_hat
    gamma_vcov <- het_boot_info$gamma_vcov
    z_matrix   <- het_boot_info$z_matrix   # n_county x n_gamma
    het_ids    <- het_boot_info$county_ids

    # Draw gamma from multivariate normal
    n_gamma <- length(gamma_hat)
    gamma_draws <- MASS::mvrnorm(n_boot, mu = gamma_hat, Sigma = gamma_vcov)
    if (n_gamma == 1) gamma_draws <- matrix(gamma_draws, ncol = 1)

    # For each draw b, compute county-specific ATT:
    #   variation_c_b = Z_c %*% gamma_b
    #   variation_centered_c_b = variation_c_b - mean(variation_c_b)
    #   att_c_b = att_overall_b + variation_centered_c_b
    #   capped at 0

    # Align county order: het_boot_info may have different order than county_baseline
    bl_ids <- as.character(county_baseline$id)
    het_idx <- match(bl_ids, as.character(het_ids))

    boot_att <- matrix(NA_real_, nrow = n_county, ncol = n_boot)
    for (b in seq_len(n_boot)) {
      variation <- as.numeric(z_matrix %*% gamma_draws[b, ])
      variation_centered <- variation - mean(variation)
      att_b <- att_draws_overall[b] + variation_centered
      att_b <- pmin(att_b, did_att * 0.10)  # soft floor: at least 10% of main ATT

      # Map to county_baseline order
      att_mapped <- rep(att_draws_overall[b], n_county)  # fallback for unmatched
      matched <- !is.na(het_idx)
      att_mapped[matched] <- att_b[het_idx[matched]]
      boot_att[, b] <- att_mapped
    }
    message("  Bootstrap: drawing from joint (overall ATT + gamma) distribution")
  } else {
    # --- Fallback: proportional scaling only (original behavior) ---
    ratio_draws <- att_draws_overall / did_att
    boot_att <- outer(att_county, ratio_draws)
    message("  Bootstrap: proportional scaling only (no het_boot_info)")
  }

  # County x boot matrix of delta_log = att_c_b * share_55_64_c
  boot_delta <- boot_att * share_55_64  # element-wise: n_county x n_boot

  # Point estimates
  delta_point <- att_county * share_55_64

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
