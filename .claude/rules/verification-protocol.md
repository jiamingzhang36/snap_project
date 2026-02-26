# Verification Protocol

**Triggered by:** `.R`, `.tex` files in `R/`, `paper/`, `scripts/`.

At the end of EVERY task, verify the output works correctly. Do not trust intent — trust output.

## R Scripts
- [ ] `Rscript [script.R]` exits with code 0
- [ ] Expected output files exist at correct paths
- [ ] Output files have non-zero size
- [ ] No warning messages that indicate data issues
- [ ] Panel dimensions: `nrow(df)` matches expected N_counties × T_months
- [ ] Key estimates are within plausible range

## LaTeX (Paper/Slides)
- [ ] `cd paper && xelatex [target].tex` exits cleanly (or `make [target]`)
- [ ] No undefined references or citations
- [ ] Overfull hbox count < 5
- [ ] PDF generated at expected location

## Data Pipeline
- [ ] `data/derived/panel_analysis.rds` is loadable
- [ ] Required columns present
- [ ] No duplicate observations
- [ ] Date range within expected window

## Quality Gates
- [ ] `python3 scripts/quality_gates.py --root .` exits with code 0
- [ ] All required files present
- [ ] Key CSVs non-empty

## Major Pitfalls
- Assuming the script ran because you wrote it — always execute
- Not checking output file sizes (empty files pass existence checks)
- Forgetting that `saveRDS()` succeeded but the data is wrong
- LaTeX "compiling" but with missing figures/tables
