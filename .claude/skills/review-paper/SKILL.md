# /review-paper

Comprehensive academic manuscript review — simulates a top-journal referee report.

## Invocation
```
/review-paper [filename]
```
Examples:
- `/review-paper paper/paper.tex`
- `/review-paper paper/slides.tex`

## Process

### Step 1: Read the Manuscript
- Read the target file (`.tex`, `.pdf`, or `.md`)
- Also read `refs.bib` for citation context
- Check `doc/current_research_plan.md` for stated goals

### Step 2: Evaluate Across 6 Dimensions (rate each 1-5)

**A. Argument Clarity**
- Is the research question clearly stated in the introduction?
- Does the paper make a clear contribution claim?
- Is the roadmap logical?

**B. Identification Strategy**
- Is the DID/event-study design convincingly motivated?
- Are threats to identification discussed and addressed?
- Is the ABAWD waiver timing exogeneity argument compelling?

**C. Econometric Rigor**
- Correct estimator for staggered treatment?
- Appropriate standard error clustering?
- Robustness checks meaningful (not just padding)?

**D. Literature Positioning**
- Key papers cited? (SNAP, ABAWD, food policy, causal inference)
- Contribution clearly differentiated from existing work?
- Any important omissions?

**E. Writing Quality**
- Academic English clear and concise?
- Technical precision (causal language appropriate)?
- Abstract informative?

**F. Presentation**
- Tables readable with clear notes?
- Figures publication-quality?
- Appendix material well-organized?

### Step 3: Generate Deliverables

1. **Overall recommendation**: Strong Accept / Accept / Minor Revision / Major Revision / Reject
2. **Top 3 strengths** (specific, with page/section references)
3. **Top 5 concerns** (specific, actionable, with suggested fixes)
4. **3-5 "Referee Objections"** — tough questions a top-5 journal reviewer would raise
5. **Dimension scores** (1-5 each)
6. **Line-level comments** (up to 15)

## Output
Save to `quality_reports/paper_review_[filename].md`
