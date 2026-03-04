# Michigan SNAP ABAWD County Vulnerability Analysis

County-level vulnerability assessment under expanded ABAWD work requirements (2026 OBBBA — One Big Beautiful Bill Act).

## Research question

Under expanded ABAWD work requirements, which Michigan counties are most vulnerable to SNAP participation losses, and what county characteristics predict larger projected losses?

## Pipeline

Three-module pipeline: data construction → ABAWD event-study → 2026 forecast/vulnerability.

```
01_build: FAP raw data → county-month panel (panel_analysis.rds)
02_abawd: Staggered DID event-study + heterogeneity + stabilizer analysis
05_forecast_2026: OBBBA scenarios + uncertainty + county vulnerability ranking
```

### Run

```bash
Rscript R/99_run/run_all.R              # full pipeline
Rscript R/99_run/run_paper_outputs.R    # paper figures/tables only (fast)
bash scripts/run_pipeline_strict.sh     # pipeline + quality gates
```

## Key outputs

| File | Description |
|------|-------------|
| `outputs/tables/abawd_heterogeneity.csv` | ATT by county subgroup (6 dimensions) |
| `outputs/tables/abawd_heterogeneity_interactions.csv` | Continuous interaction coefficients |
| `outputs/tables/abawd_stabilizer_correlation.csv` | SNAP-unemployment elasticity vs ABAWD effect |
| `outputs/tables/forecast_2026_scenarios.csv` | 3 OBBBA scenarios (baseline/recession/recovery) |
| `outputs/tables/county_vulnerability_ranking.csv` | 83 counties ranked with uncertainty |
| `outputs/figures/abawd_es_main.png` | Main event-study plot |
| `outputs/figures/abawd_heterogeneity_forest.png` | Subgroup ATT forest plot |
| `outputs/figures/vulnerability_ranking_dotplot.png` | Top 30 vulnerable counties |
| `outputs/figures/vulnerability_map.png` | Michigan choropleth map |

## Data paths

- Central config: `config/paths.R`, `config/globals.R`
- Main derived panel: `data/derived/panel_analysis.rds`
- Raw FAP data: `FAP/` (957 county-year CSVs)
- Intermediate: `data_clean/` (read by 01_build)
- (Optional) County age shares: `data/external/acs_age_shares_MI.csv`

## Module READMEs

- Forecast: `R/05_forecast_2026/README.md`
- Paper build: `paper/README.md`

## Quality gates

```bash
python3 scripts/quality_gates.py --root .
```

## Archived modules

Non-ABAWD analyses (income DL, EA event-study, food behavior) are preserved in `_archive/`.
See `_archive/README.md` for restoration instructions.
