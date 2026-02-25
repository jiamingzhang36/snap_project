# Data Layout

## `data/raw/`
Canonical raw inputs actively used by pipeline configs (policy timing files, etc.).

## `data/derived/`
Pipeline-generated analysis-ready datasets (`*.rds`).

## `data/external/`
External and legacy raw inputs not directly consumed by the current main pipeline.
- `raw_legacy/` (migrated from former top-level `data_raw/`)

## Compatibility
Top-level `data_raw` is kept as a symlink to `data/external/raw_legacy` for old notebooks/scripts.
