pdfbook:
	Rscript --quiet _render.R "bookdown::pdf_book"

gitbook:
	Rscript --quiet _render.R "bookdown::gitbook"

open:
	open _thesis/thesis.pdf

preview:
	open _thesis/index.html

response:
	Rscript -e 'Sys.setenv("RSTUDIO_PANDOC" = "/Applications/RStudio.app/Contents/MacOS/pandoc"); rmarkdown::render("report/response.Rmd")'
