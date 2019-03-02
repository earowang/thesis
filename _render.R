if (Sys.getenv("RSTUDIO") != "1" && Sys.info()['sysname'] == "Darwin") {
  Sys.setenv('RSTUDIO_PANDOC' = '/Applications/RStudio.app/Contents/MacOS/pandoc')
}

options(warn = 1)

img_pdf <- list.files("img/", pattern = "*.pdf")
for (i in img_pdf) {
  file_pdf <- paste0("img/", i)
  dest_pdf <- paste0("img/", sub("pdf$", "png", i))
  magick::image_write(
    magick::image_read(file_pdf, 300), dest_pdf, "png", 
    density = 300
  )
}

# provide default formats if necessary
formats <- c("bookdown::pdf_book", "bookdown::gitbook")

# render the book to all formats
for (fmt in formats)
  bookdown::render_book("index.Rmd", fmt, quiet = FALSE)

gif_file <- list.files("figure", "*.gif", full.names = TRUE)

invisible(file.copy(gif_file, "_thesis/figure/animate.gif"))
# dir.create("_thesis/img", showWarnings = FALSE)
# img_files <- list.files("img", "*.png", full.names = TRUE)
# invisible(file.copy(img_files, "_thesis/img"))
