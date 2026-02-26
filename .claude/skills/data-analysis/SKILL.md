# /data-analysis

End-to-end R data analysis workflow: exploration through regression to publication-ready outputs.

## Invocation
```
/data-analysis [dataset or analysis goal]
```
Examples:
- `/data-analysis estimate ABAWD event study with heterogeneity by urban/rural`
- `/data-analysis summarize panel_analysis.rds demographics`
- `/data-analysis regress ln_cases on treatment with county and month FEs`

## Workflow

### Phase 1: Setup
1. Source `config/paths.R` and `config/globals.R`
2. Load packages via `R/00_utils/packages.R`
3. Read and inspect input data (dimensions, types, missingness)

### Phase 2: Exploratory Analysis
1. Summary statistics table (N, mean, sd, min, p25, median, p75, max)
2. Balance table (treatment vs control pre-treatment means)
3. Diagnostic visualizations: outcome time series, treatment timing plot

### Phase 3: Main Analysis
1. Specify regression with appropriate:
   - Fixed effects (county + month for panel)
   - Clustering (county level)
   - Standard errors (HC1 or cluster-robust)
2. Run main specification and robustness checks
3. For DID/event-study: use `did` or `fixest` packages; reference `R/00_utils/helpers_did.R`

### Phase 4: Publication Output
1. Tables via `modelsummary` or `fixest::etable` → save to `DIR_OUT_TABLES` as CSV
2. Figures via `ggplot2` → save to `DIR_OUT_FIGURES` as PNG (300 dpi) + PDF
3. Use `ggsave(bg = "transparent")` for slide compatibility
4. Explicit `width` and `height` in all `ggsave()` calls

### Phase 5: Review & Export
1. Save script to appropriate `R/` subdirectory
2. Save RDS objects to `DIR_DERIVED`
3. **Run the r-reviewer agent** on the script before presenting results
4. Present summary of findings with key coefficients, SEs, and significance

## Conventions
- Follow `R/00_utils/` helper patterns
- Use `VAR_*` constants from `config/globals.R` for variable names
- 2-space indent, `|>` pipe, snake_case functions
- Every computed object gets `saveRDS()` before it's used in figures/tables

## Output Location
- Scripts: `R/` (in appropriate numbered subdirectory)
- Tables: `outputs/tables/`
- Figures: `outputs/figures/`
- Food behavior: `outputs/food_behavior/tables/` and `outputs/food_behavior/figures/`
