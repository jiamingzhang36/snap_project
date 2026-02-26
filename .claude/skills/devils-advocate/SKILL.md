# /devils-advocate

Pre-commit critical review. Challenges your analysis decisions before you commit/present.

## Invocation
```
/devils-advocate [topic or file]
```
Examples:
- `/devils-advocate R/02_abawd/02_estimate_event_study.R`
- `/devils-advocate current identification strategy`
- `/devils-advocate paper/paper.tex introduction section`

## Process

### Step 1: Read Target
- Read the specified file or topic area
- Cross-reference with `doc/current_research_plan.md`
- Check `config/globals.R` for relevant assumptions

### Step 2: Generate 5-7 Challenges

Each challenge must be:
- **Specific** — point to exact code, paragraph, or assumption
- **Constructive** — include a resolution path
- **Prioritized** — rank by severity (Critical / Major / Minor)

### Challenge Categories

1. **Identification threats** — "What if the ABAWD waiver timing is endogenous to local labor market conditions?"
2. **Alternative explanations** — "Could concurrent policy changes explain your results?"
3. **Measurement concerns** — "Does county-level aggregation mask individual heterogeneity?"
4. **Robustness gaps** — "What happens if you trim outlier counties?"
5. **Specification sensitivity** — "How sensitive is the main estimate to the event window choice?"
6. **External validity** — "Would these results generalize beyond Michigan?"
7. **Missing benchmarks** — "How does your effect size compare to [relevant paper]?"

### Step 3: Produce Report

For each challenge:
```
### Challenge N: [title]
**Severity**: Critical / Major / Minor
**Target**: [file:line or section]
**The concern**: [2-3 sentences]
**Why it matters**: [1-2 sentences]
**Suggested resolution**: [actionable steps]
```

## Output
Save to `quality_reports/devils_advocate_[topic].md`

## Philosophy
The goal is to surface issues NOW, before a referee does. Better to find problems yourself than to have R&R #3.
