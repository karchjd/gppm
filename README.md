
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->
[![R-CMD-check](https://github.com/karchjd/gppm/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/karchjd/gppm/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/karchjd/gppm/graph/badge.svg)](https://app.codecov.io/gh/karchjd/gppm)
<!-- badges: end -->

## Overview

gppm is an implementation of Gaussian process panel modeling.

## Installation

``` r
devtools::install_github("karchjd/gppm")
```

## Examples

For examples, consult the demos. To see a list of all demos do the
following:

``` r
demo(package = "gppm")
```

To run a particular example, you can do the following. Here, exemplified
for ‘example1linearModel’:

``` r
demo("example1linearModel", package = "gppm")
```

However, the recommended approach is to look at the source directly. To
locate the demo folder in which all examples resides on your computer,
do the following:

``` r
system.file("demo", package = "gppm")
#> [1] "/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/gppm/demo"
```

## Learning Gaussian Process Panel Modeling

This [dissertation](https://edoc.hu-berlin.de/handle/18452/18293) and
this
[paper](https://www.frontiersin.org/journals/psychology/articles/10.3389/fpsyg.2020.00351/full)
describe the method.

## Getting help

Send an email to <j.d.karch@fsw.leidenuniv.nl>.
