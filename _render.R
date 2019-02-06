if (Sys.getenv("RSTUDIO") != "1" && Sys.info()['sysname'] == "Darwin") {
  Sys.setenv('RSTUDIO_PANDOC' = '/Applications/RStudio.app/Contents/MacOS/pandoc')
}

formats <- commandArgs(TRUE)

options(warn = 1)

# provide default formats if necessary
if (length(formats) == 0)
	formats <- c("bookdown::pdf_book", "bookdown::gitbook")

# render the book to all formats
for (fmt in formats)
  bookdown::render_book("index.Rmd", fmt, quiet = FALSE)
