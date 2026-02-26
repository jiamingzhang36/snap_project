# Project Rules (Minimal Workflow)

Purpose: keep this repo reproducible and avoid scattered outputs.

## 1) Plan-first
- Before major changes, define: objective, files touched, output paths, and checks.

## 2) Canonical outputs
- Tables: `outputs/tables/`
- Figures: `outputs/figures/`
- Maps: `outputs/figures/` (vulnerability_map.png)
- Legacy/historical outputs must stay under `outputs/archive_legacy_*/`.

## 3) Data separation
- Raw FAP inputs: `FAP/`
- Intermediate data: `data_clean/` (read by 01_build)
- Pipeline-derived: `data/derived/*.rds`
- External (ACS age shares, etc.): `data/external/`

## 4) Quality gates (must pass)
- Required canonical files exist (config, run_all, abawd scripts, forecast scripts).
- No stray archived module outputs (`income_*`, `ea_*`, `food_behavior_*`) in `outputs/tables` or `outputs/figures`.
- Key result CSVs are non-empty.
- No junk files (`.DS_Store`, `.Rapp.history`) in repo.

## 5) Run commands
- Full pipeline: `Rscript R/99_run/run_all.R`
- Paper outputs only: `Rscript R/99_run/run_paper_outputs.R`
- Strict run: `bash scripts/run_pipeline_strict.sh`
- Gates only: `python3 scripts/quality_gates.py --root .`

## 6) Report policy
- If any gate fails, fix paths/files and rerun gates before presenting results.
