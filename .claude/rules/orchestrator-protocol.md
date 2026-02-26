# Orchestrator Protocol — Contractor Mode

**Always-on rule.** After plan approval, this protocol governs autonomous execution.

## The Loop

```
Plan approved
    → Implement
    → Verify (run code, check outputs)
    → Review (run relevant agents)
    → Fix (address findings)
    → Score (against quality gates)
    → If score < 80: loop back to Review (max 5 rounds)
    → Present results to user
```

## Stage Details

### 1. Implement
- Follow the approved plan step by step
- Use `config/paths.R` and `config/globals.R` conventions
- Save intermediate outputs as RDS

### 2. Verify
- R scripts: `Rscript [script.R]` must exit 0
- LaTeX: compile with no errors
- All expected output files must exist with non-zero size
- Panel dimensions should match expected N × T
- Max 2 retry attempts on verification failure

### 3. Review
Select appropriate agents based on what was changed:
- R code → `r-reviewer` agent
- Paper/slides → `proofreader` + `domain-reviewer` agents
- New analysis → `domain-reviewer` agent
- Any task → `verifier` agent

### 4. Fix
- Address Critical and High severity findings
- Medium severity: fix if easy, note if complex
- Low severity: skip unless trivial

### 5. Score
Apply quality gates scoring:
- **80/100** = minimum to commit
- **90/100** = ready for PR / presentation
- **95/100** = excellence (aspirational)

If score < 80 after 5 rounds, present results with remaining issues listed.

## "Just Do It" Mode
When user says "just do it" or the task is simple:
- Skip requirements spec
- Still run verify + review
- Auto-proceed if score >= 80
- Still present final summary

## Constraints
- Max 5 review-fix rounds
- Max 2 verification retries
- Always present a final summary regardless of score
