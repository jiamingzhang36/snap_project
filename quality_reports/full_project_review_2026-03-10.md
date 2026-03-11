# Full Project Review — Michigan SNAP Policy Analysis

**Date**: 2026-03-10  
**Reviewer**: Automated audit  
**Branch**: `cursor/all-project-issues-b9c1`  
**Scope**: All active R scripts, config, paper, data, pipeline  

---

## Executive Summary

| Severity | Count |
|----------|-------|
| **CRITICAL** (pipeline-breaking) | 2 |
| **HIGH** (correctness / convention violation) | 8 |
| **MEDIUM** (quality / consistency) | 11 |
| **LOW** (style / minor) | 5 |
| **Total** | **26** |

**Overall Score**: ~62/100 (Blocked — must fix critical and high issues before commit)

---

## CRITICAL Issues (Pipeline-Breaking)

### C-1. Typo in `R/99_run/run_all.R` — pipeline will crash

**File**: `R/99_run/run_all.R`, line 29  
**Issue**: Path reads `R/05_foreground_2026/02_scenarios.R` — should be `R/05_forecast_2026/02_scenarios.R`.  
**Impact**: `run_all.R` will error at the forecast stage; the full pipeline cannot complete.  
**Fix**: Change `foreground` to `forecast`.

```r
# WRONG (line 29):
source("R/05_foreground_2026/02_scenarios.R", local = new.env())

# CORRECT:
source("R/05_forecast_2026/02_scenarios.R", local = new.env())
```

### C-2. `data/raw/abawd_waiver_timing.csv` is empty

**File**: `data/raw/abawd_waiver_timing.csv`  
**Issue**: Contains only the header row (`county,treat_month,source_note`) with zero data rows. This is the canonical data source for ABAWD treatment timing referenced in `config/globals.R`.  
**Impact**: `abawd_timing` in `globals.R` will be a 0-row data frame. Treatment timing falls back entirely to hardcoded vectors in `helpers_policy.R`. While the fallback works, the CSV should either contain the actual data or be removed to avoid confusion.  
**Fix**: Populate the CSV with the actual county treatment months, or add an explicit comment in `globals.R` explaining that `helpers_policy.R` is the canonical source.

---

## HIGH Severity Issues

### H-1. Zero `ggsave()` calls use `bg = "transparent"`

**Convention**: R code conventions require `ggsave(bg = "transparent")` for slide compatibility.  
**Issue**: Out of 8 `ggsave()` calls across active R scripts, **none** include `bg = "transparent"`.  
**Affected files**:
- `R/01_did_cs.R` (4 calls: lines 155, 415, 798, 947)
- `R/02_abawd/04_heterogeneity.R` (1 call: line 207)
- `R/02_abawd/05_stabilizer.R` (1 call: line 155)
- `R/05_forecast_2026/04_vulnerability_index.R` (2 calls: lines 158, 180)

**Fix**: Add `bg = "transparent"` to every `ggsave()` call.

### H-2. Non-standard `set.seed()` values

**Convention**: `set.seed(YYYYMMDD)` called once at top.  
**Issue**: Two files use `set.seed(123)` instead of the YYYYMMDD format.  
**Affected files**:
- `R/01_did_cs.R`, line 20: `set.seed(123)`
- `R/02_abawd/04_heterogeneity.R`, line 16: `set.seed(123)`

**Fix**: Change to `set.seed(20260310)` or similar YYYYMMDD format.

### H-3. Mixed pipe operators within files

**Convention**: "Pick one pipe and be consistent within file."  
**Issue**: Almost every active R file uses **both** `%>%` (magrittr) and `|>` (base R). For example, `R/01_did_cs.R` has 32 uses of `%>%` and 164 of `|>`.  
**Fix**: Standardize on `|>` (preferred) or `%>%` within each file.

### H-4. Paper `paper.tex` is a skeleton — all body sections empty

**File**: `paper/paper.tex`  
**Issue**: Every section after the abstract (Introduction, Background, Data, Methods, Results, Discussion, Conclusion) contains only LaTeX comments as placeholders — no actual prose.  
**Impact**: Paper is not compilable into a readable manuscript.

### H-5. Bibliography disabled in paper

**File**: `paper/paper.tex`, lines 136–137  
**Issue**: Both `\bibliographystyle{plain}` and `\bibliography{refs}` are commented out.  
**Impact**: No references will appear in the compiled paper.

### H-6. `refs.bib` contains only 1 entry

**File**: `paper/refs.bib`  
**Issue**: Only Callaway & Sant'Anna (2021) is listed. The knowledge base identifies 6 key papers.  
**Missing citations**: Sun & Abraham (2021), Gray et al. (2023), Ganong & Liebman (2018), Stacy et al. (2018), de Chaisemartin & D'Haultfoeuille (2020), Goodman-Bacon (2021).

### H-7. Slides reference incorrect figure path

**File**: `paper/slides.tex`, lines 40, 45  
**Issue**: Slides reference `../outputs/step1_did/fig_es_main` and `../outputs/step1_did/fig_es_ddd_adult_child`, while the paper references `../outputs/figures/abawd_es_main` and `../outputs/figures/abawd_es_ddd_adult_child`.  
**Impact**: If only the canonical `outputs/figures/` path is populated, slides will have missing figures.

### H-8. `vulnerability_map.png` referenced in paper but does not exist

**File**: `paper/paper.tex`, line 108  
**Issue**: `\includegraphics{../outputs/figures/vulnerability_map}` — this file is not in `outputs/figures/`. It requires `sf` + `tigris` R packages which are not installed.  
**Impact**: Paper compilation will fail or produce a warning about missing figure.

---

## MEDIUM Severity Issues

### M-1. `R/02_abawd/03_figures_tables.R` is a stub

**Issue**: The script only copies files from `outputs/step1_did/` to `outputs/figures/`. It does not generate publication-ready figures or tables as its name implies.  
**Fix**: Either expand to produce actual publication figures/tables, or rename to clarify its purpose (e.g., `03_copy_step1_outputs.R`).

### M-2. `R/01_did_cs.R` does not use shared `theme_paper()`

**File**: `R/01_did_cs.R`  
**Convention**: "Consistent theme across project (define once in `R/00_utils/helpers_plot.R`)."  
**Issue**: Uses `theme_bw(base_size = 14)` directly instead of `theme_paper()`.  
**Fix**: Replace `theme_bw()` calls with `theme_paper()`.

### M-3. `broom` package used but not declared

**File**: `R/02_abawd/04_heterogeneity.R`, line 168  
**Issue**: `broom::tidy(m)` is called, but `broom` is not in `R/00_utils/packages.R`.  
**Fix**: Add `"broom"` to the `need_pkg()` list in `packages.R`, or use `fixest::coeftable()` instead.

### M-4. `patchwork` and `gridExtra` not in `packages.R`

**File**: `R/05_forecast_2026/03_outputs_maps.R`, lines 80–83  
**Issue**: Both packages are checked via `requireNamespace()` but not declared in `packages.R`.  
**Fix**: Add to `packages.R` or document as optional dependencies.

### M-5. `grDevices::png()` used with low resolution (150 dpi)

**File**: `R/05_forecast_2026/03_outputs_maps.R`, line 79  
**Convention**: Figures should be 300 dpi.  
**Issue**: `grDevices::png(path_fig, width = 10, height = 8, units = "in", res = 150)` — uses 150 dpi.  
**Fix**: Change to `res = 300`, or better yet, use `ggsave()` with 300 dpi.

### M-6. Deprecated `dplyr::do()` used

**File**: `R/02_abawd/05_stabilizer.R`, line 42  
**Issue**: `dplyr::do()` is deprecated since dplyr 1.0.0.  
**Fix**: Refactor to `group_modify()`, `group_map()`, or `nest() + purrr::map()`.

### M-7. Event-study plot x-axis breaks too sparse

**File**: `R/01_did_cs.R`, line 141  
**Issue**: `scale_x_continuous(breaks = seq(min_pre, max_post, by = 6))` with `min_pre = -6`, `max_post = 6` produces only 3 tick marks (-6, 0, 6).  
**Fix**: Use `by = 2` or `by = 3` for better readability.

### M-8. Makefile uses `pdflatex` but CLAUDE.md mentions `xelatex`

**File**: `paper/Makefile` vs. `.claude/rules/verification-protocol.md`  
**Issue**: Makefile defines `LATEX = pdflatex`, but the verification protocol says `cd paper && xelatex [target].tex`.  
**Fix**: Align on one LaTeX engine. If UTF-8 support is needed, switch to `xelatex`.

### M-9. `R/01_did_cs.R` loads packages directly instead of via `packages.R`

**File**: `R/01_did_cs.R`, lines 8–17  
**Issue**: Uses `library()` calls directly instead of sourcing `R/00_utils/packages.R`.  
**Fix**: Replace with `source("R/00_utils/packages.R", local = TRUE)`.

### M-10. No PDF output for figures — only PNG

**Convention**: "Figures ? PNG (300 dpi) + PDF to `DIR_OUT_FIGURES`."  
**Issue**: All `ggsave()` calls save only PNG files. No PDF copies are generated.  
**Fix**: Add a second `ggsave()` call with `.pdf` extension, or create a helper function.

### M-11. Approximate date arithmetic in forecast baseline

**File**: `R/05_forecast_2026/01_baseline_model.R`, line 39  
**Issue**: `cutoff <- last_date - 12 * 30` uses 360 days as proxy for 12 months.  
**Fix**: Use `cutoff <- last_date %m-% months(12)` (from `lubridate`).

---

## LOW Severity Issues

### L-1. Verbose diagnostic output in `R/01_did_cs.R`

**Issue**: ~100+ lines of `cat()` diagnostic messaging. While useful for development, this is noisy for a pipeline script.  
**Suggestion**: Gate behind a `verbose` flag or use `message()` which can be suppressed.

### L-2. Some script headers incomplete

**Convention**: "Script header: title, purpose, inputs, outputs."  
**Issue**: `R/01_did_cs.R`, `R/02_abawd/01_construct_event_time.R`, and several others have partial headers.

### L-3. `data_clean/` contains 19 CSV files from multiple pipeline stages

**Issue**: Multiple versions of similar data (e.g., `abawd_tdense_G_treated.csv`, `abawd_tdense_G_treated_corrected.csv`, `abawd_tdense_G_treated_rebuilt.csv`). Unclear which are canonical.  
**Suggestion**: Document which files are current or archive the rest.

### L-4. Missing `AGENTS.md` for cloud agent environment

**Issue**: No `AGENTS.md` file exists in the workspace root. This would help cloud agents understand the development environment.

### L-5. `ea_policy_dates.csv` has only 1 row

**File**: `data/raw/ea_policy_dates.csv`  
**Issue**: Contains only `event,date\nEA_end,2023-03-01`. The EA analysis module is archived, so this may be intentional, but the file is sparse.

---

## Quality Gate Results

```
? python3 scripts/quality_gates.py --root .
   PASS: 4, WARN: 0, FAIL: 0
```

Note: The quality gates script checks for file existence and non-empty CSVs but does **not** check for the issues listed above (code conventions, figure quality, LaTeX compilation, etc.).

---

## Recommendations (Priority Order)

1. **Fix C-1 immediately** — the `foreground` ? `forecast` typo breaks the pipeline.
2. **Fix H-1 through H-3** — these are direct violations of project conventions.
3. **Populate paper content (H-4, H-5, H-6)** or mark paper as WIP.
4. **Standardize figure paths** between paper and slides (H-7).
5. **Add missing packages** to `packages.R` (M-3, M-4).
6. **Adopt `theme_paper()`** across all plotting code (M-2).
7. **Upgrade deprecated `dplyr::do()`** (M-6).
8. **Add PDF figure outputs** alongside PNG (M-10).

---

## Scoring Breakdown

| Category | Deduction | Items |
|----------|-----------|-------|
| Pipeline crash (C-1) | -100 (capped at -30 for single bug) | 1 |
| Empty canonical data file (C-2) | -10 | 1 |
| Missing `bg = "transparent"` (H-1) | -5 × 8 = -40 (capped) | 8 calls |
| Non-standard seed (H-2) | -10 | 2 files |
| Mixed pipes (H-3) | -2 × 12 = -24 (capped) | 12 files |
| Paper skeleton (H-4–H-6) | -15 | 3 issues |
| Missing figure (H-8) | -10 | 1 |
| Medium issues | -5 each × 11 | 11 |
| Low issues | -1 each × 5 | 5 |

**Raw deduction**: exceeds 100 ? **Score: < 80 (Blocked)**

Per quality gates rules: "Scores below 80: list blocking issues, do not commit."
