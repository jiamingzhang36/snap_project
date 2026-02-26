# 06_food_behavior

Local README for food behavior analyses built from Dewey county-week mobility data.

## Required inputs
- `dewey-downloads/snap/michigan_county_weekly_behavior_panel.parquet`
- `data/derived/panel_analysis.rds`

## Scripts
1. `01_build_behavior_panel.R` — build merged weekly panel.
2. `02_event_study_abawd_behavior.R` — ABAWD Sun-Abraham event-study.
3. `03_robustness_abawd_behavior.R` — robustness table.
4. `04_heterogeneity_urban_rural.R` — urban/rural heterogeneity.
5. `05_ea_event_study_behavior.R` — EA ITS (Michigan statewide timing).
6. `06_unemployment_dl_behavior.R` — unemployment distributed-lag.
7. `run_food_behavior.R` — run 01→06.

## Run
```bash
Rscript R/06_food_behavior/run_food_behavior.R
```

## Key outputs
- `outputs/food_behavior/tables/food_behavior_event_study_abawd.csv`
- `outputs/food_behavior/tables/food_behavior_robustness_abawd.csv`
- `outputs/food_behavior/tables/food_behavior_heterogeneity_urban_rural_summary.csv`
- `outputs/food_behavior/tables/food_behavior_ea_its_results.csv`
- `outputs/food_behavior/tables/food_behavior_unemp_dl_cumulative.csv`

Notes:
- Current customized extract has no `452311` supercenter rows.
- Current Michigan-only unemployment DL (lags 0..6): cumulative `fast_food_visits_log1p` not statistically significant.

For project-level context, use root `README.md`.
