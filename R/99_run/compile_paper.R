# R/99_run/compile_paper.R — Compile paper and slides (LaTeX) from project root
# Requires: pdflatex (e.g. TinyTeX: tinytex::install_tinytex())

if (!dir.exists("paper")) stop("Run from project root (directory containing paper/).")

ok <- system("make -C paper", ignore.stderr = FALSE)
if (ok != 0) {
  message("Make failed. Install TinyTeX in R? tinytex::install_tinytex()")
  stop("Compilation failed.")
}
message("Done. Outputs: paper/paper.pdf, paper/slides.pdf")
