# 05_forecast_2026

2026 OBBA scenario forecast: county-level vulnerability under expanded ABAWD work requirements.

## Purpose

Estimate county-level SNAP participation losses under OBBA (age expansion to 55-64, reduced exemptions), with uncertainty intervals and alternative labor-market scenarios. Produces county vulnerability ranking.

## Scripts

1. `01_baseline_model.R` — County baselines and ATT extraction from DID
2. `02_scenarios.R` — OBBA scenarios with county-specific age shares, 3 labor-market scenarios, bootstrap uncertainty
3. `03_outputs_maps.R` — Figures, tables, summary
4. `04_vulnerability_index.R` — Composite county risk ranking with choropleth map

## Outputs

- `outputs/tables/forecast_2026_scenarios.csv` — All scenarios (baseline + recession + recovery)
- `outputs/tables/forecast_2026_summary.csv` — State-level summary with aggregate CI
- `outputs/tables/county_vulnerability_ranking.csv` — 83 counties ranked by composite score
- `outputs/figures/forecast_2026_scenarios.png` — Bar + histogram
- `outputs/figures/vulnerability_ranking_dotplot.png` — Top 30 counties
- `outputs/figures/vulnerability_map.png` — Michigan choropleth (requires sf + tigris)

## Config/data hooks

- `config/globals.R` — `OBBA_EFFECTIVE_DATE`, `OBBA_AGE_EXPANSION`, `PATH_OBBA_EXEMPTIONS`
- `data/derived/panel_analysis.rds`, `data/derived/forecast_baseline.rds`
- (Optional) `data/external/acs_age_shares_MI.csv` — County-level 55-64 age shares

## Run

```bash
Rscript R/05_forecast_2026/01_baseline_model.R
Rscript R/05_forecast_2026/02_scenarios.R
Rscript R/05_forecast_2026/03_outputs_maps.R
Rscript R/05_forecast_2026/04_vulnerability_index.R
```
