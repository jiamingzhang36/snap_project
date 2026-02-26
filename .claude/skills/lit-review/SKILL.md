# /lit-review

Structured literature search and synthesis with citation extraction and gap identification.

## Invocation
```
/lit-review [topic or research question]
```
Examples:
- `/lit-review ABAWD work requirements and SNAP participation`
- `/lit-review staggered DID methods in policy evaluation`
- `/lit-review front-of-pack nutrition labeling consumer behavior`

## Process

### Step 1: Parse & Scope
- Interpret the topic/question
- Define search boundaries (time range, field, methods)
- Check `refs.bib` for papers already cited in the project

### Step 2: Systematic Search
- Search via web for recent papers (2018-2026)
- Check NBER working papers, SSRN, Google Scholar
- Prioritize top-5 econ journals + field journals (AJAE, Food Policy, JHEC, AEPP)
- Include relevant working papers and policy reports (USDA-ERS, FNS, CBO)

### Step 3: Categorize Findings
Organize into:
- **Theoretical contributions** (models, frameworks)
- **Empirical results** (estimates, magnitudes, signs)
- **Methodological innovations** (estimators, identification strategies)
- **Unresolved debates** (conflicting findings, open questions)

### Step 4: Gap Analysis
- What questions remain unanswered?
- Where do existing studies conflict?
- What data/methods would resolve the gaps?
- How does this project's contribution fit?

### Step 5: Citation Database
- Compile BibTeX entries for all discussed papers
- Flag which ones are already in `refs.bib`
- Provide entries ready to append

## Output Requirements
- Executive summary (2-3 paragraphs)
- Annotated key papers (5-15 entries): authors, year, contribution, method, findings, relevance to this project
- Thematic groupings
- Explicit gap analysis with research implications
- BibTeX entries
- Save to `quality_reports/lit_review_[topic].md`

## Safeguards
- NEVER fabricate citations — if uncertain, flag it explicitly
- Distinguish published vs. working papers
- Prioritize peer-reviewed work
- Note any papers you couldn't verify
