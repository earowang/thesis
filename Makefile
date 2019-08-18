pdfbook:
	Rscript --quiet _render.R "bookdown::pdf_book"

gitbook:
	Rscript --quiet _render.R "bookdown::gitbook"

open:
	open _thesis/thesis.pdf

preview:
	open _thesis/index.html
