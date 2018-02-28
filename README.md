---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# cattonum

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/cattonum)](http://cran.r-project.org/package=cattonum)
[![Build Status](https://travis-ci.org/bfgray3/cattonum.svg?branch=master)](https://travis-ci.org/bfgray3/cattonum)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/bfgray3/cattonum?branch=master&svg=true)](https://ci.appveyor.com/project/bfgray3/cattonum)
[![Coverage status](https://codecov.io/gh/bfgray3/cattonum/branch/master/graph/badge.svg)](https://codecov.io/github/bfgray3/cattonum?branch=master)

## Summary

`cattonum` (cat to num) provides different ways to encode categorical features as numerics.  It includes the following:

* dummy encoding: `catto_dummy`
* feature hashing (future)
* frequency encoding: `catto_freq`
* label encoding: `catto_label`
* leave-one-out encoding: `catto_loo`
* mean encoding: `catto_mean`
* median encoding: `catto_median`
* one-hot encoding: `catto_onehot`

There are many existing packages with which to encode categorical features, including (among others):

* [CatEncoders](https://cran.r-project.org/package=CatEncoders)
* [dummies](https://cran.r-project.org/package=dummies)
* [FeatureHashing](https://CRAN.R-project.org/package=FeatureHashing)
* [fastDummies](https://cran.r-project.org/package=fastDummies)
* [h2o](https://cran.r-project.org/package=h2o)
* [makedummies](https://cran.r-project.org/package=makedummies)
* [recipes](https://cran.r-project.org/package=recipes)
* [vtreat](https://CRAN.R-project.org/package=vtreat)

The goal of `cattonum` is to be a one-stop shop for all categorical encoding needs.  Nothing more, nothing less.

## Installation

The development version of `cattonum` can be installed from GitHub.


```r
# install.packages("devtools")
devtools::install_github("bfgray3/cattonum")
```

The latest official release of `cattonum` can be installed from CRAN.


```r
install.packages("cattonum")
```

## Usage


```r
library(cattonum)
data(iris)
head(catto_loo(iris, response = Sepal.Length))
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width  Species
#> 1          5.1         3.5          1.4         0.2 5.004082
#> 2          4.9         3.0          1.4         0.2 5.008163
#> 3          4.7         3.2          1.3         0.2 5.012245
#> 4          4.6         3.1          1.5         0.2 5.014286
#> 5          5.0         3.6          1.4         0.2 5.006122
#> 6          5.4         3.9          1.7         0.4 4.997959
```
