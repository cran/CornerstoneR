# Cornerstone R Scripts

[![Lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![CRAN status](https://www.r-pkg.org/badges/version/CornerstoneR)](https://cran.r-project.org/package=CornerstoneR)
[![CRAN Checks](https://cranchecks.info/badges/worst/CornerstoneR)](https://cran.r-project.org/web/checks/check_results_CornerstoneR.html)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/CornerstoneR)](https://cran.r-project.org/package=CornerstoneR)
[![Pipeline Status](https://gitlab.com/camLine/CornerstoneR/badges/master/pipeline.svg)](https://gitlab.com/camLine/CornerstoneR/commits/master)
[![Coverage Report](https://gitlab.com/camLine/CornerstoneR/badges/master/coverage.svg)](https://camLine.gitlab.io/CornerstoneR/coverage.html)

**CornerstoneR** package provides generic R scripts which enable you to use existing R routines in Cornerstone.

The desktop application [Cornerstone](https://www.camline.com/en/products/cornerstone/cornerstone-core.html) is a data analysis software provided by camLine that empowers engineering teams to find solutions even faster.
The engineers incorporate intensified hands-on statistics into their projects.
They benefit from an intuitive and uniquely designed graphical Workmap concept: you design experiments (DoE) and explore data, analyze dependencies, and find answers you can act upon, immediately, interactively, and without any programming.

While Cornerstone's interface to the statistical programming language [R](https://www.r-project.org/) has been available since version 6.0, the latest interface with R is even much more efficient.
Cornerstone release 7.1.1 allows you to integrate user defined R packages directly into the standard Cornerstone GUI.
Your engineering team stays in Cornerstone's graphical working environment and can apply R routines, immediately and without the need to deal with programming code.

Learn how to use R packages in Cornerstone 7.1.1 on [camLineTV YouTube](https://www.youtube.com/watch?v=HEQHwq_laXU) channel (available in German).


## Documentation

1. [Overview](https://camLine.gitlab.io/CornerstoneR/docs/)
1. [Function Reference](https://camLine.gitlab.io/CornerstoneR/docs/reference/)
1. [News](https://camLine.gitlab.io/CornerstoneR/docs/news/)


## Installation

### CRAN Version

Install the version from CRAN via
```r
install.packages("CornerstoneR")
```

### Development Version

For the development version from GitLab we use [remotes](https://cran.r-project.org/package=remotes).
Installation the development version via
```r
install.packages("remotes")
remotes::install_gitlab("camLine/CornerstoneR")
```
To install a certain branch or commit or tag, append the corresponding `name` after an `@` to the repo name:
```r
remotes::install_gitlab("camLine/CornerstoneR@name")
```
