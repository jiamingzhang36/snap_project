# CLAUDE.md — Michigan SNAP Policy Analysis

## Project Identity
- **Author**: Jiaming Zhang (PhD student, Agricultural Economics, MSU)
- **Topic**: County-level heterogeneity in SNAP participation losses under ABAWD work-requirement expansion in Michigan
- **Methods**: Staggered DID (Callaway & Sant'Anna), event study, distributed lag, county risk projection
- **Tooling**: R (fixest, did, ggplot2, modelsummary), LaTeX/Beamer, Python (quality gates)

## Canonical Pipeline
```
R/99_run/run_all.R
  → 01_build   (county-month panel)
  → 02_abawd   (main staggered DID/event-study)
  → 03_income   (unemployment distributed lag)
  → 04_ea       (Emergency Allotment context)
  → 05_forecast_2026  (OBBA county risk projection)
  → 06_food_behavior  (optional; Dewey mobility data)
```

Run: `Rscript R/99_run/run_all.R`
Strict: `bash scripts/run_pipeline_strict.sh`
Gates: `python3 scripts/quality_gates.py --root .`

## Key Paths
| What | Where |
|------|-------|
| Config | `config/paths.R`, `config/globals.R` |
| Helpers | `R/00_utils/` |
| Derived data | `data/derived/` |
| Tables | `outputs/tables/` |
| Figures | `outputs/figures/` |
| Paper | `paper/paper.tex`, `paper/slides.tex` |
| Quality reports | `quality_reports/` |

## Workflow System

This project uses an adapted version of [Sant'Anna's Claude Code workflow](https://github.com/pedrohcgs/claude-code-my-workflow).

### Agents (`.claude/agents/`)
| Agent | Purpose |
|-------|---------|
| `r-reviewer` | R code quality + reproducibility review |
| `domain-reviewer` | SNAP/applied-micro substantive correctness |
| `verifier` | End-to-end task completion verification |
| `proofreader` | Academic writing quality |

### Skills (`.claude/skills/`)
| Skill | Purpose |
|-------|---------|
| `/data-analysis` | End-to-end R analysis workflow |
| `/review-paper` | Referee-style manuscript review |
| `/lit-review` | Literature search + synthesis |
| `/compile-latex` | 3-pass XeLaTeX compilation |
| `/devils-advocate` | Pre-commit critical challenge |
| `/commit` | Stage → commit → PR → merge |
| `/research-ideation` | Generate research questions |
| `/interview-me` | Interactive research spec development |

### Rules (`.claude/rules/`)
| Rule | Scope | Purpose |
|------|-------|---------|
| `plan-first-workflow` | Always | Plan before implementing |
| `orchestrator-protocol` | Always | Implement → verify → review → fix → score |
| `session-logging` | Always | Document as you go |
| `replication-protocol` | `R/` | Replicate before extending |
| `quality-gates` | `R/`, `paper/` | 80/90/95 scoring thresholds |
| `r-code-conventions` | `*.R` | Code style + reproducibility |
| `knowledge-base` | All | Domain notation + acronyms + anti-patterns |
| `verification-protocol` | `R/`, `paper/` | Verify outputs actually work |

### Templates (`templates/`)
- `requirements-spec.md` — MUST/SHOULD/MAY framework
- `session-log.md` — Structured session documentation
- `quality-report.md` — Merge-time quality audit

### Hooks (`.claude/hooks/`)
- `pre-compact.sh` — Save context snapshot before auto-compression
- `post-merge.sh` — Generate quality report after merge

## Research Workflow

Typical flow for a new research task:
```
/research-ideation (what to study?)
  → /interview-me (formalize the idea)
  → /lit-review (what's been done?)
  → /data-analysis (run the analysis)
  → /review-paper (check the draft)
  → /devils-advocate (challenge before presenting)
  → /commit (save and merge)
```

## Non-Negotiables
1. All R paths via `config/paths.R` — never hardcoded
2. Staggered DID only via `did` or `fixest` (no naive TWFE)
3. Analysis window: 2014-01 to 2019-12 (pre-COVID)
4. Cluster at county level (not state — only 1 state)
5. Quality gates must pass before any commit

## Quick Reference
See `WORKFLOW_QUICK_REF.md` for a one-page overview.
