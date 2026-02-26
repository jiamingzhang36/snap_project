# Quality Report: [BRANCH NAME]

Date: YYYY-MM-DD
Merged to: main

## Summary

| Metric | Value |
|--------|-------|
| Overall score | XX/100 |
| Files changed | N |
| R scripts reviewed | N |
| LaTeX files reviewed | N |
| Critical issues | N |
| High issues | N |

## Changes Included
[1-3 bullet summary]

## Review Results

### R Code Review
- Score: XX/100
- Critical issues: [list or "none"]
- High issues: [list or "none"]

### Domain Review
- Assessment: [Accept / Major / Minor]
- Key concerns: [list or "none"]

### Verification
- Status: PASS / PARTIAL / FAIL
- Failed checks: [list or "none"]

## Quality Gates
```
python3 scripts/quality_gates.py --root .
```
- PASS: N
- WARN: N
- FAIL: N

## Sign-off
- [ ] All critical issues resolved
- [ ] Quality gates pass
- [ ] Results reproduce from clean state
