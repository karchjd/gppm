
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis-CI Build Status](https://travis-ci.org/karchjd/gppm.svg?branch=master)](https://travis-ci.org/karchjd/gppm) <!-- [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ggplot2)](https://cran.r-project.org/package=ggplot2)-->

Overview
--------

gppm is an implementation of Gaussian process panel modeling.

Installation
------------

``` r
devtools::install_github('karchjd/gppm')
```

Examples
--------

For examples, consult the demos. To see a list of all demos do the following:

``` r
demo(package='gppm')
```

To run a particular example, you can do the following. Here, exemplified for 'example1linearModel':

``` r
demo('example1linearModel',package='gppm')
```

However, the recommended approach is to look at the source directly. To locate the demo folder in which all examples resides on your computer, do the following:

``` r
system.file("demo", package ="gppm")
#> [1] "/Library/Frameworks/R.framework/Versions/3.5/Resources/library/gppm/demo"
```

Learning Gaussian Process Panel Modeling
----------------------------------------

This [dissertation](https://edoc.hu-berlin.de/handle/18452/18293) and this [paper](https://psyarxiv.com/kvw5y/) describe the method.

Getting help
------------

Send an email to <j.d.karch@fsw.leidenuniv.nl>.
