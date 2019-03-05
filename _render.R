options(warn = 1)
library(cliapp)
start_app(theme = simple_theme())

cli_h1("Preprocessing")
# spelling check
cli_h2("Spelling check")
rmd_files <- list.files(pattern = "*.Rmd", full.names = TRUE, recursive = TRUE)
wordlist <- readLines("WORDLIST")
spell_gb <- spelling::spell_check_files(rmd_files, wordlist, "en_GB")
spell_us <- spelling::spell_check_files(rmd_files, wordlist, "en_US")
spell_res <- intersect(spell_gb[[1]], spell_us[[1]])
if (length(spell_res) > 0) {
  print(subset(spell_gb, spell_gb[[1]] %in% spell_res))
  stop("Can you please fix typos listed above first?", call. = FALSE)
}

# convert pdf to png for html output
cli_h2("Coverting pdf to png")
img_pdf <- list.files("img/", pattern = "*.pdf")
for (i in img_pdf) {
  file_pdf <- paste0("img/", i)
  dest_pdf <- paste0("img/", sub("pdf$", "png", i))
  magick::image_write(
    magick::image_read(file_pdf, 300), dest_pdf, "png", 
    density = 300
  )
}

cli_h1("Compiling")

if (Sys.getenv("RSTUDIO") != "1" && Sys.info()['sysname'] == "Darwin") {
  Sys.setenv('RSTUDIO_PANDOC' = '/Applications/RStudio.app/Contents/MacOS/pandoc')
}

# provide default formats if necessary
formats <- commandArgs(trailingOnly = TRUE)
if (length(formats) == 0) {
  formats <- "bookdown::pdfbook"
}
# render the book to all formats
for (fmt in formats)
  bookdown::render_book("index.Rmd", fmt, quiet = FALSE)

gif_file <- list.files("figure", "*.gif", full.names = TRUE)
invisible(file.copy(gif_file, "_thesis/figure/animate.gif"))
cli_alert_success("You Rock!")
