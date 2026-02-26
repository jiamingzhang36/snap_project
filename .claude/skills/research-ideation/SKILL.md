# /research-ideation

Generate structured empirical research questions from a topic, dataset, or policy context.

## Invocation
```
/research-ideation [topic or phenomenon]
```
Examples:
- `/research-ideation SNAP benefit cliffs and labor supply`
- `/research-ideation front-of-pack labeling and purchase behavior`
- `/research-ideation county-level variation in ABAWD exemption usage`

## Process

### Step 1: Context Analysis
- Parse the input topic
- Read `doc/current_research_plan.md` for existing scope
- Identify relevant data sources, policy settings, and institutional details

### Step 2: Generate 3-5 Research Questions
Progress from descriptive to causal to policy-relevant:

1. **Descriptive**: "What does the data show?" (patterns, facts, magnitudes)
2. **Causal-reduced-form**: "What is the effect of X on Y?" (DID, RD, IV)
3. **Mechanism**: "Why/how does X affect Y?" (mediation, heterogeneity)
4. **Policy counterfactual**: "What would happen if policy Z changed?" (simulation, projection)
5. **Structural/welfare**: "What is the welfare impact?" (sufficient statistics, structural model)

### Step 3: Develop Each Question
For each research question:
- **Hypothesis**: testable prediction with expected sign
- **Identification strategy**: DiD, RD, IV, event study — which and why
- **Data requirements**: what data is needed? Available or obtainable?
- **Key assumptions**: what must hold? How to test?
- **Main threats**: top 2-3 identification concerns
- **Feasibility**: data access, sample size, variation
- **Contribution**: what's new relative to existing literature?

### Step 4: Rank & Recommend
| Question | Feasibility | Contribution | Priority |
|----------|------------|--------------|----------|
| Q1       | High       | Medium       | 1        |
| ...      | ...        | ...          | ...      |

### Step 5: Save Output
Save to `quality_reports/research_ideation_[topic].md`

## Principles
- Every question must be empirically actionable
- Referee-level scrutiny on causal claims
- Be pragmatic about data availability
- Flag when an idea requires data you don't have
