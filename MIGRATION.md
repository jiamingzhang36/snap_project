# Paper-level project structure — Migration guide

## Current vs recommended layout

| Recommended path | Current counterpart | Notes |
|------------------|---------------------|--------|
| **config/paths.R** | (new) | Central ROOT, data/derived, outputs |
| **config/globals.R** | (new) | Policy dates, variable names; reads data/raw/*.csv |
| **data/raw/** | data_clean/ + FAP/ | Raw data read-only; policy timing in abawd_waiver_timing.csv, ea_policy_dates.csv |
| **data/derived/** | data_clean/*.csv as RDS | panel_base.rds, panel_with_laus.rds, **panel_analysis.rds** = single entry for downstream |
| **data/external/** | (new) | RUCC, industry, etc. |
| **R/01_build/** | — | 01_clean_snap → panel_base; 02_clean_laus → panel_with_laus; 03_merge_panel → panel_analysis (reads data_clean/panel_with_G.csv) |
| **R/02_abawd/** | R/01_did_cs.R | 01 event_time; 02 estimation = 01_did_cs logic; 03 copy figures to outputs/figures/abawd_*.png |
| **R/03_income/** | (todo) | Unemployment shock DL, heterogeneity |
| **R/04_ea/** | (todo) | EA end event-study, intensity heterogeneity |
| **R/05_forecast_2026/** | — | 2026 scenario forecast |
| **outputs/tables/** | outputs/step1_did/*.csv etc | Naming: abawd_*, income_*, ea_*, forecast_* |
| **outputs/figures/** | outputs/step1_did/*.png, fap_overview/*.png | Same |
| **R/99_run/run_all.R** | (new) | Sources 01_build → 02_abawd → … → 05_forecast_2026 in order |

## Three rules (per script)

1. **Read only from upstream**: data/raw or data/derived/*.rds (during migration, data_clean/ is OK).
2. **Write one clear product**: one .rds or one set of figures/tables to a fixed path.
3. **Header comment**: one line for Input, one for Output.

## Key RDS (downstream reads only these three)

1. **panel_base.rds** — SNAP-cleaned county-month (cases/recipients/adult/child/payments/avgpp/avgpc).
2. **panel_with_laus.rds** — + unemp_rate, du_yoy, etc.
3. **panel_analysis.rds** — + log_*, adult_share, G, y_per1k_18_49, etc.; **ABAWD/DDD, income, EA, forecast all read from here**.

During migration **03_merge_panel.R** reads data_clean/panel_with_G.csv and writes panel_analysis.rds.

## Next steps to implement

1. **R/01_build/02_clean_laus.R** — Build yoy unemployment shock and write panel_with_laus.rds.
2. **R/03_income/01_estimate_dl_unemp.R** — Main DL regression + dynamic response plot.
3. **R/04_ea/01_event_study_avg_benefit.R** — EA “benefit jump” event-study.

## One-command run

From project root (directory that contains config/ and R/):

```r
setwd("/Users/jiamingzhang/Desktop/snap_project")  # or your path
source("R/99_run/run_all.R")
```

Paper figures only:

```r
source("R/99_run/run_paper_outputs.R")
```

Full ABAWD (CS + DDD) still uses the existing script:

```r
source("R/01_did_cs.R")
```

Once 02_abawd/02 reads panel_analysis.rds and calls did/fixest, run_all can fully replace it.
