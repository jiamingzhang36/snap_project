# Replication-First Protocol

**Triggered by:** working in `R/` on estimation or analysis scripts.

When modifying or extending existing analysis, ALWAYS replicate the current results first before making changes.

## Four Phases

### Phase 1: Inventory & Baseline
- Run the existing script as-is
- Record "gold standard" numbers:

| Target | Source | Value | CI/SE |
|--------|--------|-------|-------|
| Main DID estimate | Table 2, col 1 | -0.XXX | (0.XXX) |
| ...    | ...    | ...   | ...   |

### Phase 2: Execute Existing Code
- Run the script: `Rscript R/[module]/[script].R`
- Verify outputs match the gold standard
- If outputs don't exist yet, the current run IS the baseline

### Phase 3: Verify Match
Tolerance thresholds:

| Result Type | Tolerance |
|-------------|-----------|
| Integers (N, counts) | Exact match |
| Point estimates | < 0.01 absolute difference |
| Standard errors | < 0.005 absolute difference |
| p-values | Same significance level |
| R-squared | < 0.001 |

**DO NOT proceed to extension if replication fails.** Document the discrepancy first.

### Phase 4: Only Then Extend
- Commit the replication baseline (if new)
- Make your modifications
- Compare new results against the replicated baseline
- Document what changed and why

## Common Pitfalls (Stata → R translation)

| Issue | Stata default | R equivalent |
|-------|--------------|--------------|
| Clustering | `vce(cluster x)` | `vcov = ~county` in `fixest` |
| Absorbing FE | `areg` / `reghdfe` | `feols()` in `fixest` |
| Weights | `[pw=wt]` | `weights = ~wt` |
| Missing values | Listwise by default | Check `na.action` |

## Output
Save replication report to `quality_reports/replication_[script_name].md`
