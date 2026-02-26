# Workflow Quick Reference

One-page reference for the Claude Code research workflow.

## Skills (type in chat to invoke)

| Command | What it does |
|---------|-------------|
| `/data-analysis [goal]` | Full R analysis pipeline: explore → estimate → publish-ready output |
| `/review-paper [file]` | Referee-style review of paper/slides |
| `/lit-review [topic]` | Literature search, synthesis, gap analysis + BibTeX |
| `/compile-latex [target]` | 3-pass XeLaTeX compile with bibtex |
| `/devils-advocate [topic]` | 5-7 critical challenges before you commit |
| `/commit [msg]` | Branch → stage → commit → PR → merge |
| `/research-ideation [topic]` | Generate 3-5 structured research questions |
| `/interview-me [topic]` | Interactive Q&A to formalize a research spec |

## Agents (invoked by skills or orchestrator)

| Agent | When it runs |
|-------|-------------|
| `r-reviewer` | After any R script changes |
| `domain-reviewer` | After new analysis or paper edits |
| `verifier` | After every task completion |
| `proofreader` | Before paper/slides submission |

## The Loop (Orchestrator Protocol)

```
Plan approved → Implement → Verify → Review → Fix → Score
                                ↑                    |
                                |____ if < 80 _______|
                                     (max 5 rounds)
```

## Quality Thresholds

| Score | Meaning |
|-------|---------|
| 95+ | Excellence |
| 90-94 | PR/presentation ready |
| 80-89 | Commit OK (with warnings) |
| < 80 | Blocked — must fix |

## Run Commands

```bash
Rscript R/99_run/run_all.R              # full pipeline
bash scripts/run_pipeline_strict.sh      # pipeline + quality gates
python3 scripts/quality_gates.py --root . # gates only
cd paper && make paper                   # compile paper
cd paper && make slides                  # compile slides
```

## Research Flow

```
/research-ideation → /interview-me → /lit-review → /data-analysis → /review-paper → /devils-advocate → /commit
```

## Key Rules
1. **Plan first** — spec before code for non-trivial tasks
2. **Replicate first** — reproduce existing results before extending
3. **Verify always** — run code, check outputs exist with non-zero size
4. **Log as you go** — session logs in `quality_reports/session_logs/`
5. **Knowledge base** — check `.claude/rules/knowledge-base.md` for notation + acronyms
