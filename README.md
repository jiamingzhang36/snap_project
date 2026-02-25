# Michigan SNAP Policy Analysis

Single entry README for this repo. Module-level READMEs only contain local run notes.

## What this repo does
- ABAWD policy evaluation (DID/event-study)
- Income-unemployment dynamics (distributed lag)
- EA-related analyses
- 2026 OBBA forecast scenario (module scaffold)
- Food behavior outcomes from Dewey mobility data

## Canonical pipeline
- Main runner: `R/99_run/run_all.R`
- Typical order:
  1. `R/01_build/` (build analysis panel)
  2. `R/02_abawd/` (ABAWD DID/event-study)
  3. `R/03_income/` (unemployment DL)
  4. `R/04_ea/` (EA analyses)
  5. `R/05_forecast_2026/` (OBBA scenarios)
  6. `R/06_food_behavior/` (optional; only runs if Dewey parquet exists)

Run all:
```bash
Rscript R/99_run/run_all.R
```

## Data paths
- Core paths defined in: `config/paths.R`
- Main derived panel: `data/derived/panel_analysis.rds`
- Food behavior input parquet expected at:
  - `dewey-downloads/snap/michigan_county_weekly_behavior_panel.parquet`

## Module readmes
- Forecast module: `R/05_forecast_2026/README.md`
- Food behavior module: `R/06_food_behavior/README.md`
- Paper build: `paper/README.md`

## Food behavior quick outputs
- `outputs/tables/food_behavior_event_study_abawd.csv`
- `outputs/tables/food_behavior_robustness_abawd.csv`
- `outputs/tables/food_behavior_heterogeneity_urban_rural_summary.csv`
- `outputs/tables/food_behavior_ea_its_results.csv`
- `outputs/tables/food_behavior_unemp_dl_cumulative.csv`

Current note: in the current Michigan-only unemployment DL (lags 0..6), cumulative `fast_food_visits_log1p` effect is not statistically significant.
