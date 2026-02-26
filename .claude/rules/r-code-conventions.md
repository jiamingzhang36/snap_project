# R Code Conventions

**Triggered by:** `*.R` files in `R/`, `scripts/`, `explorations/`.

## Reproducibility
- `set.seed(YYYYMMDD)` called ONCE at top for stochastic operations
- All packages loaded via `library()` at top (or via `R/00_utils/packages.R`)
- All paths via `config/paths.R` variables (`ROOT`, `DIR_*`) — never hardcoded

## Naming
- Functions: `snake_case` with `verb_noun` pattern (e.g., `estimate_event_study()`)
- Variables: `snake_case` (e.g., `panel_analysis`, `abawd_timing`)
- Constants: `UPPER_SNAKE_CASE` (e.g., `WINDOW_START`, `EVENT_REF_PERIOD`)
- Files: `NN_descriptive_name.R` (e.g., `01_clean_snap.R`)

## Style
- 2-space indentation
- `<-` for assignment (not `=`)
- `TRUE`/`FALSE` (not `T`/`F`)
- Pipe: `|>` preferred; `%>%` acceptable if consistent within file
- Line length: < 100 characters
- **Exception**: long formulas may exceed 100 chars with an explanatory comment

## Data Pattern
- Heavy computations → `saveRDS()` to `DIR_DERIVED`
- Tables → CSV to `DIR_OUT_TABLES`
- Figures → PNG (300 dpi) + PDF to `DIR_OUT_FIGURES`
- Always `ggsave(bg = "transparent")` for slide compatibility

## Figure Standards
- Consistent theme across project (define once in `R/00_utils/helpers_plot.R`)
- Explicit `width` and `height` in `ggsave()`
- Readable fonts (min 10pt effective)
- Colorblind-friendly palettes
- Transparent backgrounds

## Documentation
- Script header: title, purpose, inputs, outputs
- WHY-comments for non-obvious logic
- No dead/commented-out code blocks
- Roxygen for exported/shared functions

## Common Pitfalls

| Risk | Wrong | Right |
|------|-------|-------|
| Absolute paths | `"/Users/jiaming/data/..."` | `file.path(DIR_DERIVED, "panel.rds")` |
| Non-transparent figs | `ggsave("fig.png")` | `ggsave("fig.png", bg = "transparent")` |
| No seed | `sample(...)` | `set.seed(20260226); sample(...)` |
| Mixed pipes | `df %>% ... |> ...` | Pick one and be consistent |
| T/F shortcuts | `if (T)` | `if (TRUE)` |

## Quality Checklist
- [ ] Packages loaded at top
- [ ] `set.seed()` present (if needed)
- [ ] All paths relative via `config/paths.R`
- [ ] Functions documented
- [ ] Figures have explicit dimensions + transparent bg
- [ ] Key objects saved as RDS
- [ ] Comments explain WHY, not WHAT
- [ ] No dead code
