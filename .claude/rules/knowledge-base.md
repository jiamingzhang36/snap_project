# Knowledge Base — SNAP Policy Analysis

**Triggered by:** all project files. This is the domain-specific reference for maintaining consistency.

## Notation Registry

| Symbol | Meaning | First used |
|--------|---------|------------|
| $Y_{ct}$ | Outcome for county $c$ in month $t$ | 02_abawd |
| $D_{ct}$ | Treatment indicator (ABAWD work requirement active) | 02_abawd |
| $\tau$ | ATT (average treatment effect on the treated) | 02_abawd |
| $\hat{\tau}^{ES}_k$ | Event-study coefficient at relative time $k$ | 02_abawd |
| $G_c$ | Treatment cohort (first treatment month for county $c$) | 02_abawd |
| $\beta$ | Generic regression coefficient | throughout |
| $X_{ct}$ | Control variables (unemployment rate, demographics) | 02_abawd |
| $\alpha_c$ | County fixed effect | 02_abawd |
| $\gamma_t$ | Time (month) fixed effect | 02_abawd |

## Key Acronyms

| Acronym | Full name |
|---------|-----------|
| SNAP | Supplemental Nutrition Assistance Program |
| ABAWD | Able-Bodied Adults Without Dependents |
| FNS | Food and Nutrition Service (USDA) |
| EA | Emergency Allotment (COVID-era SNAP supplement) |
| OBBA | Consolidated Appropriations Act 2024 (age expansion to 55-64) |
| DID | Difference-in-Differences |
| CS | Callaway & Sant'Anna (2021) estimator |
| SA | Sun & Abraham (2021) estimator |
| TWFE | Two-Way Fixed Effects |
| RD | Regression Discontinuity |

## Policy Timeline

| Date | Event |
|------|-------|
| 2014-01 | Analysis window start |
| 2016 (varies) | Michigan counties begin losing ABAWD waivers (staggered) |
| 2020-03 | COVID: nationwide ABAWD waiver + Emergency Allotments begin |
| 2019-12 | Analysis window end (pre-COVID) |
| 2023-03 | Emergency Allotments end |
| 2026-01 | OBBA: ABAWD requirements expand to age 55-64 |

## Outcome Variables

| Code variable | Description | Transform |
|---------------|-------------|-----------|
| `ln_cases` | Log SNAP caseload (households) | log(cases) |
| `ln_recipients` | Log SNAP recipients (individuals) | log(recipients) |
| `y_per1k_18_49` | SNAP recipients per 1000 pop age 18-49 | rate |
| `adult_recipients` | Adult SNAP recipients | level |
| `child_recipients` | Child SNAP recipients | level |
| `unemployment_rate` | County unemployment rate | percentage |
| `fast_food_visits_log1p` | Log(1+fast food visits) from Dewey | log1p |

## Key Literature

| Paper | Relevance |
|-------|-----------|
| Callaway & Sant'Anna (2021) | Staggered DID estimator used in main analysis |
| Sun & Abraham (2021) | Alternative staggered DID (robustness) |
| Gray et al. (2023) | SNAP and food security with ABAWD |
| Ganong & Liebman (2018) | SNAP benefit formula and consumption |
| Stacy et al. (2018) | ABAWD waivers and employment |
| de Chaisemartin & D'Haultfoeuille (2020) | TWFE bias in staggered settings |

## Data Sources

| Source | Contains | Path |
|--------|----------|------|
| USDA-FNS | County-level SNAP participation | `data/raw/` |
| BLS LAUS | County unemployment rates | `data/raw/` |
| Census ACS | County demographics | `data/external/` |
| Dewey | County food purchase behavior | `dewey-downloads/snap/` |
| FNS waivers | ABAWD waiver timing by county | `data/raw/abawd_waiver_timing.csv` |

## Anti-Patterns

| Mistake | Why it's wrong | Correct approach |
|---------|---------------|-----------------|
| TWFE with staggered treatment | Biased under heterogeneous effects | Use CS or SA estimator |
| Clustering at state level (Michigan only) | Only 1 state = no clustering at state | Cluster at county level |
| Including post-2020 data | COVID confounds everything | Window ends 2019-12 |
| "ABAWD effect" without specifying margin | Extensive vs intensive margin | State: participation rate vs benefit amount |
| County FE + state trends (1 state) | State trends = collinear with time FE | County FE + time FE sufficient |
