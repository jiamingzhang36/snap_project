# Project Rules (Minimal Workflow)

Purpose: keep this repo reproducible and avoid scattered outputs.

## 1) Plan-first
- Before major changes, define: objective, files touched, output paths, and checks.

## 2) Canonical outputs
- General outputs:
  - tables: `outputs/tables/`
  - figures: `outputs/figures/`
- Food behavior outputs only:
  - tables: `outputs/food_behavior/tables/`
  - figures: `outputs/food_behavior/figures/`
- Legacy/historical outputs must stay under `outputs/archive_legacy_*/`.

## 3) Data separation
- Active Dewey inputs remain in `dewey-downloads/snap/`.
- Intermediate/old Dewey files must stay in `dewey-downloads/snap/archive_intermediate/`.

## 4) Quality gates (must pass)
- Required canonical files exist.
- No stray `food_behavior_*` files in `outputs/tables` or `outputs/figures`.
- Key result CSVs are non-empty.
- No junk files (`.DS_Store`, `.Rapp.history`) in repo.

## 5) Run commands
- Full pipeline: `Rscript R/99_run/run_all.R`
- Strict run: `bash scripts/run_pipeline_strict.sh`
- Gates only: `python3 scripts/quality_gates.py --root .`

## 6) Report policy
- If any gate fails, fix paths/files and rerun gates before presenting results.
