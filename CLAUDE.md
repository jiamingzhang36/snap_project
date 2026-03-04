# CLAUDE.md — Project Context for Claude Code

## What is this project?

Michigan SNAP ABAWD county-vulnerability analysis. A single-paper academic project studying how expanded Able-Bodied Adults Without Dependents (ABAWD) work requirements affect SNAP participation across Michigan's 83 counties, and projecting impacts of the 2026 One Big Beautiful Bill Act (OBBBA) age expansion (55-64).

**Author**: Jiaming Zhang, PhD student, Agricultural Economics, Michigan State University

## Research question

Under expanded ABAWD work requirements, which Michigan counties are most vulnerable to SNAP participation losses, and what county characteristics predict larger projected losses?

## Pipeline (3 modules, sequential)

```
01_build       → FAP raw data → county-month panel (panel_analysis.rds)
02_abawd       → Staggered DiD (Callaway-Sant'Anna 2021) + heterogeneity + robustness
05_forecast_2026 → OBBBA scenarios + bootstrap uncertainty + county vulnerability ranking
```

Run: `Rscript R/99_run/run_all.R`

## Key design decisions (do not change without discussion)

- **Estimator**: Callaway-Sant'Anna (2021) via `did` package, not-yet-treated control, anticipation=2
- **Outcome**: `log(cases)` — natural log, NOT log(1+x). Min county-month value ~52, no zeros.
- **Treatment cohorts**: G=2017-01 (4 counties), G=2018-01 (10), G=2018-07 (68), G=2018-10 (1 Wayne)
- **Overall ATT**: ~-0.060 (≈5.8% reduction)
- **Variable naming**: R pipeline uses `OBBA_*` prefix in config/globals.R; paper/slides use `OBBBA`. Don't rename R variables.
- **paper/ directory**: gitignored, not tracked. Compile locally with xelatex.
- **_archive/**: Contains non-ABAWD modules (income DL, EA, food behavior). Don't delete.

## File layout

```
config/          paths.R, globals.R (constants, policy parameters)
R/00_utils/      packages.R, helpers_did.R, helpers_forecast.R
R/01_build/      01_clean_snap.R, 02_clean_laus.R, 03_merge_panel.R
R/02_abawd/      01-08 (event study, heterogeneity, stabilizer, robustness suite)
R/05_forecast_2026/ 01-05 (baseline model, scenarios, summary, vulnerability, sensitivity)
R/99_run/        run_all.R, run_paper_outputs.R
outputs/         tables/*.csv, figures/*.png, maps/
paper/           paper.tex, slides.tex, refs.bib (gitignored)
scripts/         quality_gates.py, run_pipeline_strict.sh
_archive/        Shelved non-ABAWD modules
```

## Common tasks

| Task | Command |
|------|---------|
| Run full pipeline | `Rscript R/99_run/run_all.R` |
| Paper outputs only | `Rscript R/99_run/run_paper_outputs.R` |
| Quality gates | `python3 scripts/quality_gates.py --root .` |
| Compile paper | `cd paper && xelatex paper.tex && bibtex paper && xelatex paper.tex && xelatex paper.tex` |
| Compile slides | `cd paper && xelatex slides.tex` |

## Style notes

- R code uses `fixest`, `did`, `data.table` heavily
- Figures: ggplot2 with minimal theme, MSU green (#18453B) for slides
- Tables output as CSV (read by LaTeX via csvsimple or manual input)
- Paper language: English. Comments/communication: Chinese OK.
