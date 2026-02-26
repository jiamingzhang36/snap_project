# /commit

Stage, commit, create PR, and merge. Standard commit-PR-merge cycle.

## Invocation
```
/commit [optional message]
```
Examples:
- `/commit`
- `/commit "Add heterogeneity analysis by urban/rural status"`

## Process

### Step 1: Examine State
```bash
git status
git diff --stat
git log --oneline -5
```

### Step 2: Create Branch
- Branch name: `feature/[descriptive-slug]` or `fix/[descriptive-slug]`
- Never commit directly to `main`

### Step 3: Stage Files
- Stage specific files (NOT `git add -A`)
- Exclude: `.claude/settings.local.json`, `.env`, credentials, `.DS_Store`
- Exclude: large data files (`.rds`, `.parquet`, `.csv` in `data/`)

### Step 4: Commit
- If message provided: use it exactly
- If no message: craft one explaining WHY the change was made
- Format: imperative mood, < 72 chars first line
- Include `Co-Authored-By: Claude Opus 4.6 <noreply@anthropic.com>` if Claude contributed

### Step 5: Push & PR
```bash
git push -u origin [branch-name]
gh pr create --title "[title]" --body "[summary + test plan]"
```

### Step 6: Merge (if approved)
```bash
gh pr merge --merge
git checkout main && git pull
git branch -d [branch-name]
```

## Safety Rules
- Never force push
- Never commit secrets or large data files
- Always create fresh branches from latest main
- Use `--merge` strategy (not squash/rebase) unless instructed otherwise
