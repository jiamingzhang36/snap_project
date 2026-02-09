# Paper and slides (LaTeX)

Compile the paper and Beamer slides from this directory or from the project root.

## Requirements

- **pdflatex** (e.g. TeX Live, MacTeX, or install via [TinyTeX](https://yihui.org/tinytex/) in R: `tinytex::install_tinytex()`).

## Compile

From **project root**:

```bash
make -C paper
```

From **paper/**:

```bash
cd paper
make
```

Outputs: `paper/paper.pdf`, `paper/slides.pdf`.

## From R

You can compile from R (project root):

```r
setwd("/Users/jiamingzhang/Desktop/snap_project")
system("make -C paper")
# Or: tinytex::pdflatex("paper/paper.tex") and similar for slides
```

Or use the helper script:

```r
source("R/99_run/compile_paper.R")
```

## Figures

- The templates include figures from `../outputs/step1_did/` (e.g. `fig_es_main.png`, `fig_es_ddd_adult_child.png`). Run the analysis pipeline first so these files exist.
- To use `outputs/figures/` (e.g. `abawd_es_main.png`), change paths in `paper.tex` and `slides.tex` to `../outputs/figures/...`.

## Citations

When you add `\cite{}` in the text, uncomment in `paper.tex`:

```latex
\bibliographystyle{plain}
\bibliography{refs}
```

Then run from **paper/**: `pdflatex paper`, `bibtex paper`, `pdflatex paper`, `pdflatex paper` (or add a `paper-bib` target in the Makefile).
