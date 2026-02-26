# Plan-First Workflow

**Always-on rule.** For any non-trivial task, plan before implementing.

## When to Plan

A requirements spec + plan is needed when:
- The task is vague or has multiple valid interpretations
- It touches 3+ files
- It involves new analysis, estimation, or data pipeline changes
- The user says "let's think about..." or "how should we..."

A plan is NOT needed for:
- Single-file edits with clear intent
- Bug fixes where the fix is obvious
- Running existing scripts
- Formatting or comment changes

## Planning Protocol

### Step 1: Enter Plan Mode
- Consult CLAUDE.md, PROJECT_RULES.md, and doc/current_research_plan.md
- Check recent session logs in `quality_reports/session_logs/`

### Step 2: Requirements Specification (if warranted)
Ask clarifying questions, then document:

```markdown
## Requirements: [task name]

### MUST (non-negotiable)
- [ ] ...

### SHOULD (strongly preferred)
- [ ] ...

### MAY (nice to have)
- [ ] ...

### Clarity Status
| Requirement | Status |
|-------------|--------|
| ...         | CLEAR / ASSUMED / BLOCKED |
```

### Step 3: Implementation Plan
- List files to create/modify
- Specify the order of operations
- Define what "done" looks like (verification criteria)
- Identify risks and mitigations

### Step 4: Save & Approve
- Save plan to `quality_reports/plans/YYYY-MM-DD_[task-slug].md`
- Present to user for approval
- Do NOT implement until approved

## After Plan Approval
Hand off to the orchestrator protocol (implement → verify → review → fix → score).
