# Dewey Snap Data Layout

## Active files (used by current scripts)
- `michigan_county_weekly_behavior_panel.parquet`  
  Main county-week behavior input for `R/06_food_behavior/*`.
- `michigan_only_with_county.parquet`  
  Row-level Michigan detail (POI-week) with `county_fips`.
- `michigan_only.parquet`  
  Michigan-only raw subset (without added `county_fips`).

## Archived intermediate files
- `archive_intermediate/michigan_county_weekly.parquet`
- `archive_intermediate/michigan_county_weekly_by_type.parquet`
- `archive_intermediate/michigan_county_weekly_core.parquet`

These are kept for reproducibility but are not required for the current pipeline.
