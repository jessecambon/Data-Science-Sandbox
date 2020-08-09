# Notes on Developing R Packages

* The man documents are created with roxygen2 based on `R/` directory code files with `devtools::document()`
* Test package with `devtools::test()` (see `/tests` directory)
* Use `devtools::check()` to check for issues (also runs `devtools::test()`)
* Use `devtools::build()` to build the package with vignettes included (creates .tar.gz file)
* Check package on other environments for CRAN release using [rhub::check_for_cran()](https://r-hub.github.io/rhub/reference/check_for_cran.html)
* To run all code examples in the package documentation, use this command from devtools: `run_examples(test=TRUE)`
* Use [pkgdown::build_site()](https://pkgdown.r-lib.org/reference/build_site.html) to build the website.

### Development Resources
* General Instructions: http://r-pkgs.had.co.nz/
* More general instructions: https://rstats-pkgs.readthedocs.io
* roxygen2 : https://cran.r-project.org/web/packages/roxygen2/
* Devtools cheat sheet: https://www.rstudio.com/wp-content/uploads/2015/03/devtools-cheatsheet.pdf
