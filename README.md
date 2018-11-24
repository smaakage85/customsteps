
<!-- README.md is generated from README.Rmd. Please edit that file -->
customsteps <img src="man/figures/logo.png" align="right" />
============================================================

[![Travis-CI Build Status](https://travis-ci.org/smaakage85/customsteps.svg?branch=master)](https://travis-ci.org/smaakage85/customsteps) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/smaakage85/customsteps?branch=master&svg=true)](https://ci.appveyor.com/project/smaakage85/customsteps)

This is a small package that offers a set of customizable higher-order recipe step functions compatible with the [`recipes`](https://cran.r-project.org/web/packages/recipes/recipes.pdf) package.

Installation
------------

`customsteps` can be installed from CRAN with `install.packages('customsteps')`. If you want the development version then install directly from GitHub:

``` r
devtools::install_github("smaakage85/customsteps")
```

Customizable higher-order step functions
----------------------------------------

Let me just remind you of the definition of [**higher-order functions**](https://en.wikipedia.org/wiki/Higher-order_function):

> *In mathematics and computer science, a higher-order function is a function that does at least one of the following: 1. takes one or more functions as arguments, 2. returns a function as its result.*

`customsteps` offers higher-order recipe step functions, that create specifications of recipe steps, that will transform or filter the data in accordance with (custom) input functions.

For more details on how to use `customsteps` please take a look at the package vignette.
