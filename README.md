# PhD thesis

This repo contains source files for my PhD thesis titled, "Tidy tools for supporting fluent workflow in temporal data analysis", at Monash University.

## Clone with `git-lfs`

Since this repo contains two large data files (>= 50MB), you need to first download and install a git plugin called [`git-lfs`](https://git-lfs.github.com) for versioning large files, and set up Git LFS using command `git lfs install` in console, in order to fully clone this repo.

## Reproducible research

This research project uses the [renv](https://rstudio.github.io/renv/) package to create reproducible environments. A project-specific library will be created when you open this repo with R. Run `renv::restore()` to install the same versions of packages that are used to produce this manuscript.

## Directories

* `Rmd/`: R Markdown source documents for thesis document.
* `scripts/`: R code to reproduce tables, figures and analysis.
* `data/`: Cleaned data used for thesis document.
* `data-raw/`: R code to generate data in `data/`.
* `img/`: Images made with other tools to illustrate ideas. 
* `bib/`: Bibliography files.
* `template/`: Monash thesis template from [robjhydman/MonashThesis](https://github.com/robjhyndman/MonashThesis).

## License

This work is licensed under a [![CC BY NC SA 4.0](https://img.shields.io/badge/License-CC%20BY%20NC%20SA%204.0-green.svg)](https://creativecommons.org/licenses/by-nc-sa/4.0/). The code contained in this work is available under the [MIT license](https://opensource.org/licenses/MIT).
