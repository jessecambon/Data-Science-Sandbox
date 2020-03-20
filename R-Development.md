# Notes on Developing R Packages

* The man documents are created with roxygen2 based on `R/` directory code files. Run this with CTRL+SHIFT+D (in RStudio) or `royxenize()` before building the package.
* Test package with `devtools::test()` (see `/tests` directory)
* Use `devtools::check()` to check for issues (also runs `devtools::test()`)
* Use `devtools::build()` to build the package with vignettes included (creates .tar.gz file)
* Use `install.packages(path_to_file, repos = NULL)` to load the package. May need to close out and reopen RStudio to see changes in vignettes and for cross-references in documentation to work. It also may be necessary to knit the vignette `.Rmd` file from a fresh RStudio session (close out the project) to update the HTML file.

### Development Resources
* General Instructions: http://r-pkgs.had.co.nz/
* More general instructions: https://rstats-pkgs.readthedocs.io
* roxygen2 : https://cran.r-project.org/web/packages/roxygen2/
* Devtools cheat sheet: https://www.rstudio.com/wp-content/uploads/2015/03/devtools-cheatsheet.pdf
