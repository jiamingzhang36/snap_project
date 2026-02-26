# /compile-latex

Compile LaTeX paper or Beamer slides with full bibliography resolution.

## Invocation
```
/compile-latex [target]
```
Examples:
- `/compile-latex paper` (compiles paper/paper.tex)
- `/compile-latex slides` (compiles paper/slides.tex)

## Process

### Compilation (3-pass + bibtex)
```bash
cd paper
xelatex -interaction=nonstopmode [target].tex   # pass 1: create .aux
bibtex [target]                                  # resolve citations
xelatex -interaction=nonstopmode [target].tex   # pass 2: integrate bib
xelatex -interaction=nonstopmode [target].tex   # pass 3: finalize refs
```

Alternative (if latexmk available):
```bash
cd paper && latexmk -xelatex -interaction=nonstopmode [target].tex
```

Or use Makefile if present:
```bash
cd paper && make [target]
```

### Post-Compilation Checks
1. Exit code 0? If not, report the first LaTeX error
2. Count overfull `\hbox` warnings (target: 0, acceptable: < 5)
3. Count undefined references/citations
4. Verify PDF exists and has expected page count
5. Report file size

### Quality Metrics
```
Compilation:  PASS/FAIL
Warnings:     N overfull hbox, M undefined refs
Pages:        N
File size:    X MB
```

## Notes
- Use `xelatex` (not `pdflatex`) for Unicode support
- `TEXINPUTS` and `BIBINPUTS` should include `paper/` directory
- If compilation fails, show the relevant error lines and suggest fixes
