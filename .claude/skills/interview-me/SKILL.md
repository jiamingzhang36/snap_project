# /interview-me

Interactive structured interview to formalize a research idea into a specification document.

## Invocation
```
/interview-me [optional topic]
```
Examples:
- `/interview-me`
- `/interview-me SNAP recertification burden`

## Interview Protocol

Ask questions one or two at a time. Wait for responses before advancing. The interview progresses through 6 phases:

### Phase 1: Big Picture
- What phenomenon are you trying to understand?
- Why does it matter? (policy relevance, economic significance)
- Who cares? (policymakers, researchers, participants)

### Phase 2: Theoretical Motivation
- What does economic theory predict?
- What are the competing hypotheses?
- What's your prior / gut instinct?

### Phase 3: Data & Setting
- What data do you have or can access?
- What's the institutional setting? (which state, which program, what time period)
- What's the unit of observation? (individual, county, state)
- Sample size: rough order of magnitude?

### Phase 4: Identification
- What provides causal leverage? (policy change, threshold, random assignment)
- What's the treatment? What's the counterfactual?
- What are the biggest threats to internal validity?

### Phase 5: Expected Results
- What do you expect to find?
- What's the magnitude of the effect you'd consider meaningful?
- What would a null result tell us?

### Phase 6: Contribution
- What's the closest existing paper?
- How is your approach different/better?
- What's the "one sentence" pitch?

## After 5-8 Exchanges

Produce a **Research Specification Document**:

```markdown
# Research Specification: [Title]

## Research Question
[1-2 sentences]

## Motivation
[2-3 sentences on why this matters]

## Hypotheses
1. [Primary hypothesis]
2. [Alternative hypothesis]

## Empirical Strategy
- Design: [DID / RD / IV / event study]
- Treatment: [what]
- Comparison: [what]
- Key assumption: [what must hold]

## Data Requirements
- Source: [dataset]
- Unit: [observation level]
- Period: [time range]
- Key variables: [list]

## Expected Results
[What you expect to find and why]

## Contribution
[1-2 sentences on what's new]
```

Save to `quality_reports/research_spec_[topic].md`

## Interview Style
- Curious, not prescriptive
- Gently probe weak points
- Build on responses naturally
- Recognize when enough clarity has been achieved — don't over-interview
