# Cornerstone R Scripts

[![CRAN status](https://www.r-pkg.org/badges/version/CornerstoneR)](https://cran.r-project.org/package=CornerstoneR)
[![Pipeline status](https://gitlab.com/camLine/CornerstoneR/badges/master/pipeline.svg)](https://gitlab.com/camLine/CornerstoneR/commits/master)
[![Coverage report](https://gitlab.com/camLine/CornerstoneR/badges/master/coverage.svg)](https://camLine.gitlab.io/CornerstoneR/coverage.html)

**CornerstoneR** package provides generic R scripts which are capable to use R solutions in Cornerstone.
The application [Cornerstone](http://www.camline.com/en/products/cornerstone/cornerstone-core.html) is a data analysis software designed for engineers and design of experiments.
Since Version 6.0 Cornerstone supports a full integration of an interface to the statistical programming language [R](https://www.r-project.org/).


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
