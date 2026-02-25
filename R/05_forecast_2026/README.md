# 05_forecast_2026

Local README for the 2026 OBBA forecast module.

## Purpose
Estimate policy scenarios after OBBA effective date (2026-01-01), including age expansion and tighter exemptions.

## Scripts
1. `01_baseline_model.R`
2. `02_scenarios.R`
3. `03_outputs_maps.R`

## Run
```bash
Rscript R/05_forecast_2026/01_baseline_model.R
Rscript R/05_forecast_2026/02_scenarios.R
Rscript R/05_forecast_2026/03_outputs_maps.R
```

## Config/data hooks
- `config/globals.R` (`OBBA_EFFECTIVE_DATE`, `OBBA_AGE_EXPANSION`, `PATH_OBBA_EXEMPTIONS`)
- `data/derived/panel_analysis.rds`
- `outputs/step1_did/` and optional `outputs/step2_dml/`

For project-level context, use root `README.md`.
