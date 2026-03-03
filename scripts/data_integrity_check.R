suppressPackageStartupMessages(library(dplyr))

cat("============================================\n")
cat("  POST-FIX DATA INTEGRITY CHECK\n")
cat("============================================\n\n")

# 1) County ATT hat
cat("== 1. County ATT hat ==\n")
att_hat <- readr::read_csv("outputs/tables/abawd_county_att_hat.csv", show_col_types = FALSE)
cat("  Rows:", nrow(att_hat), "\n")
cat("  NA in att_hat:", sum(is.na(att_hat$att_hat)), "\n")
cat("  Summary:\n")
print(summary(att_hat$att_hat))
cat("  att_base:", unique(round(att_hat$att_base, 6)), "\n")
pos_att <- att_hat %>% filter(att_hat > 0)
cat("  Counties with POSITIVE att_hat:", nrow(pos_att), "\n")
if (nrow(pos_att) > 0) {
  cat("  WARNING: Positive ATT = ABAWD increased SNAP (interaction pushes above zero).\n")
  cat("  Top 5 positive:\n")
  print(head(pos_att %>% arrange(desc(att_hat)) %>% select(id, att_hat, att_base), 5))
}

# 2) Vulnerability ranking direction
cat("\n== 2. Vulnerability Ranking Direction ==\n")
vuln <- readr::read_csv("outputs/tables/county_vulnerability_ranking.csv", show_col_types = FALSE)
cat("  Rows:", nrow(vuln), "\n")
cat("  delta_log > 0:", sum(vuln$delta_log > 0, na.rm = TRUE), "counties\n")
cat("  delta_log < 0:", sum(vuln$delta_log < 0, na.rm = TRUE), "counties\n")
cat("  rank_median unique:", length(unique(vuln$rank_median)), "\n")
cat("  Rank 1:", vuln$county_name[1], "delta_log =", round(vuln$delta_log[1], 6), "\n")
cat("  Rank 83:", vuln$county_name[83], "delta_log =", round(vuln$delta_log[83], 6), "\n")
if (vuln$delta_log[1] > vuln$delta_log[83]) {
  cat("  *** ISSUE: Ranking INVERTED — rank 1 has LESS negative delta ***\n")
} else {
  cat("  OK: rank 1 has most negative delta_log\n")
}

# 3) Forecast scenarios
cat("\n== 3. Forecast Scenarios ==\n")
scen <- readr::read_csv("outputs/tables/forecast_2026_scenarios.csv", show_col_types = FALSE)
cat("  Total rows:", nrow(scen), "\n")
print(table(scen$scenario))
bl <- scen %>% filter(scenario == "baseline")
cat("  share_55_64 unique:", length(unique(bl$share_55_64)), "\n")
cat("  share_55_64 range:", round(range(bl$share_55_64), 4), "\n")
if ("att_used" %in% names(bl)) {
  cat("  att_used unique:", length(unique(bl$att_used)), "\n")
  cat("  att_used range:", round(range(bl$att_used), 4), "\n")
}
cat("  delta_log unique:", length(unique(bl$delta_log)), "\n")
cat("  delta_log range:", round(range(bl$delta_log), 6), "\n")

# 4) Interaction model
cat("\n== 4. Interaction Coefficients ==\n")
inter <- readr::read_csv("outputs/tables/abawd_heterogeneity_interactions.csv", show_col_types = FALSE)
for (i in seq_len(nrow(inter))) {
  dir <- ifelse(inter$gamma[i] > 0, "attenuates", "amplifies")
  sig <- ifelse(inter$gamma_p[i] < 0.01, "***", ifelse(inter$gamma_p[i] < 0.05, "**", "*"))
  cat(sprintf("  %-15s gamma=%.4f %s -> %s\n", inter$moderator[i], inter$gamma[i], sig, dir))
}

# 5) Joint model
cat("\n== 5. Joint Model ==\n")
joint <- readr::read_csv("outputs/tables/abawd_heterogeneity_joint.csv", show_col_types = FALSE)
print(as.data.frame(joint))

# 6) Subgroup DID
cat("\n== 6. Subgroup DID ==\n")
het <- readr::read_csv("outputs/tables/abawd_heterogeneity.csv", show_col_types = FALSE)
cat("  Succeeded:", sum(!is.na(het$att)), "/", nrow(het), "\n")
het_na <- het %>% filter(is.na(att))
if (nrow(het_na) > 0) { cat("  Failed:\n"); print(as.data.frame(het_na %>% select(subgroup, group, n_obs, reason))) }
het_ok <- het %>% filter(!is.na(att)) %>% mutate(sig = ifelse(abs(att/se) > 1.96, "**", ""))
cat("  Succeeded:\n"); print(as.data.frame(het_ok %>% select(subgroup, group, att, se, sig, n_obs)))

# 7) File inventory
cat("\n== 7. Output Files ==\n")
tables <- list.files("outputs/tables", pattern = "\\.csv$", full.names = TRUE)
figures <- list.files("outputs/figures", pattern = "\\.(png|pdf)$", full.names = TRUE)
cat("  Tables:", length(tables), "\n")
for (f in tables) cat(sprintf("    %-50s %s\n", basename(f), format(file.info(f)$size, big.mark=",")))
cat("  Figures:", length(figures), "\n")
for (f in figures) cat(sprintf("    %-50s %s\n", basename(f), format(file.info(f)$size, big.mark=",")))

# 8) Stabilizer
cat("\n== 8. Stabilizer ==\n")
stab <- readr::read_csv("outputs/tables/abawd_stabilizer_correlation.csv", show_col_types = FALSE)
print(as.data.frame(stab))

cat("\n============================================\n")
cat("  END CHECK\n")
cat("============================================\n")
