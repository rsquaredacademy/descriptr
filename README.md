
<!-- README.md is generated from README.Rmd. Please edit that file -->
descriptr <img src="descriptr.jpg" align="right" />
===================================================

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/descriptr)](https://cran.r-project.org/package=descriptr) [![Travis-CI Build Status](https://travis-ci.org/rsquaredacademy/descriptr.svg?branch=master)](https://travis-ci.org/rsquaredacademy/descriptr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/rsquaredacademy/descriptr?branch=master&svg=true)](https://ci.appveyor.com/project/rsquaredacademy/descriptr) [![Coverage Status](https://img.shields.io/codecov/c/github/rsquaredacademy/descriptr/master.svg)](https://codecov.io/github/rsquaredacademy/descriptr?branch=master) [![](https://cranlogs.r-pkg.org/badges/grand-total/descriptr)](http://cran.rstudio.com/web/packages/descriptr/index.html)

Overview
--------

The goal of descriptr is to ease the process of generating descriptive statistics and exploring statistical distributions.

Installation
------------

``` r
# install descriptr from CRAN
install.packages("descriptr")

# the development version from github
# install.packages("devtools")
devtools::install_github("rsquaredacademy/descriptr")
```

Usage
-----

### Descriptive Statistics

The following functions are available for generating descriptive statistics:

-   `summary_stats`
-   `cross_table`
-   `freq_table`
-   `freq_cont`
-   `group_summary`
-   `oway_tables`
-   `tway_tables`

### Explore Distributions

Functions have been defined to visualize and compute percentiles/probabilities for the following distributions:

-   Normal
-   Binomial
-   Chi Square
-   F
-   t

Links
-----

-   [Shiny App](http://rsquaredlabs.com:3838/explorer/)
-   [Website](https://rsquaredacademy.github.io/descriptr/)
-   [Tutorials](http://rsquaredacademy.com/)
-   [YouTube](https://www.youtube.com/user/rsquaredin)

Similar Packages
----------------

-   [tigerstats](https://cran.r-project.org/web/packages/tigerstats/)
-   [describer](https://cran.r-project.org/web/packages/describer/)

Credits
-------

We have extensively referred to [R Packages](http://r-pkgs.had.co.nz/) by Hadley Wickham and [Writing R Extensions](https://cran.r-project.org/doc/manuals/r-release/R-exts.html) while creating this package. The [devtools](https://cran.r-project.org/web/packages/devtools/) and [testthat](https://cran.r-project.org/web/packages/testthat/) package were of immense help. Before submitting the package to [CRAN](https://cran.r-project.org/), we submitted it to [rhub](https://builder.r-hub.io/) several times to ensure there were no errors. We also went through the repositories of several R packages and learnt a lot. Thanks to R commmunity, R Core team, R Consortium and Hadley Wickham.

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
