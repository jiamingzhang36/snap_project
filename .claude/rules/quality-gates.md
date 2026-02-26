# Quality Gates & Scoring

**Triggered by:** `.R`, `.tex`, `.md` files in `R/`, `paper/`, `scripts/`.

## Score Thresholds

| Score | Meaning | Action |
|-------|---------|--------|
| **95+** | Excellence | Aspirational target |
| **90-94** | PR-ready | Can present / submit |
| **80-89** | Commit-worthy | OK to commit with warnings |
| **< 80** | Blocked | Must fix before committing |

## Scoring Deductions — R Scripts

| Issue | Deduction | Severity |
|-------|-----------|----------|
| Script won't run (syntax error) | -100 | Critical |
| Domain bug (wrong estimator, wrong clustering) | -30 | Critical |
| Hardcoded absolute path | -20 | High |
| Missing `set.seed()` for stochastic ops | -10 | High |
| No `saveRDS()` for key computed objects | -10 | High |
| Figure without explicit dimensions | -5 | Medium |
| Missing variable documentation | -5 | Medium |
| Style violations (indent, naming) | -2 each | Low |
| Long lines (> 100 chars, non-formula) | -1 each | Low |

## Scoring Deductions — LaTeX

| Issue | Deduction | Severity |
|-------|-----------|----------|
| Won't compile | -100 | Critical |
| Undefined citation | -15 | High |
| Undefined reference | -10 | High |
| Overfull hbox > 10pt | -10 | Medium |
| Overfull hbox 1-10pt | -5 | Low |

## Scoring Deductions — Analysis Quality

| Issue | Deduction | Severity |
|-------|-----------|----------|
| Wrong identification strategy | -50 | Critical |
| Missing robustness check | -15 | High |
| No pre-trend test for DID | -15 | High |
| Causal language without identification | -10 | Medium |
| Effect size not benchmarked | -5 | Medium |

## Tolerance Table (Research-Specific)

| Measurement | Acceptable tolerance |
|-------------|---------------------|
| Point estimates (replication) | < 0.01 |
| Standard errors | < 0.005 |
| Monte Carlo simulations | 95% CI overlap |
| Sample sizes | Exact match |

## Enforcement
- Scores below 80: list blocking issues, do not commit
- Scores 80-89: commit with warnings documented
- User may override with documented justification

## Quality Reports
Generated at merge time only. Save to `quality_reports/merges/YYYY-MM-DD_[branch-name].md`
