# Session Logging

**Always-on rule.** Document work as it happens, not after.

## Three Triggers

### 1. Post-Plan Log
After plan approval, immediately capture:
- Goal
- Approach
- Rationale
- Key context (data files, scripts involved)

### 2. Incremental Logging
Append 1-3 lines whenever:
- A design decision is made
- A problem is encountered and resolved
- User provides new direction
- Strategy changes

Do NOT batch these. Log immediately.

### 3. End-of-Session Log
When wrapping up:
- High-level: what was accomplished
- Quality scores (if review agents ran)
- Unresolved questions
- Next steps
- Blocking issues (if any)

## Log Format

Save to `quality_reports/session_logs/YYYY-MM-DD_[description].md`

```markdown
# Session Log: [description]
Date: YYYY-MM-DD
Duration: [approximate]

## Goal
[1-2 sentences]

## Changes Made
| File | Action | Status |
|------|--------|--------|
| ...  | ...    | Done/Partial/Blocked |

## Decisions
1. [Decision]: [rationale]
2. ...

## Quality
- R-reviewer score: XX/100
- Domain reviewer: [assessment]

## Open Questions
1. ...

## Next Steps
1. ...
```

## Context Preservation
Before auto-compression (context window getting full):
- Verify session log is current
- Ensure any active plans are saved to disk
- Update MEMORY.md if needed
