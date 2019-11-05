
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cattonum

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/cattonum)](https://cran.r-project.org/package=cattonum)
[![Travis build
status](https://travis-ci.org/bfgray3/cattonum.svg?branch=develop)](https://travis-ci.org/bfgray3/cattonum)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/bfgray3/cattonum?branch=develop&svg=true)](https://ci.appveyor.com/project/bfgray3/cattonum)
[![Codecov test
coverage](https://codecov.io/gh/bfgray3/cattonum/branch/develop/graph/badge.svg)](https://codecov.io/gh/bfgray3/cattonum?branch=develop)
[![Total
Downloads](http://cranlogs.r-pkg.org/badges/grand-total/cattonum)](https://cran.r-project.org/package=cattonum)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Licence](https://img.shields.io/github/license/mashape/apistatus.svg)](http://choosealicense.com/licenses/mit/)

## Special note

I am currently looking for someone who would be interested in becoming
the official maintainer of the package. Advanced skill in R or previous
package development experience is not necessary. I will be able to help
out with code review, etc. Please comment on [this
issue](https://github.com/bfgray3/cattonum/issues/40) if you are
interested.

## Summary

cattonum (“cat to num”) provides different ways to encode categorical
features as numerics. Its goal is to be a one-stop shop for all
categorical encoding needs. It includes the following:

  - dummy encoding: `catto_dummy()`
  - frequency encoding: `catto_freq()`
  - label encoding: `catto_label()`
  - leave-one-out encoding: `catto_loo()`
  - mean encoding: `catto_mean()`
  - median encoding: `catto_median()`
  - one-hot encoding: `catto_onehot()`

There are many existing packages with which to encode categorical
features, including (among others):

  - [CatEncoders](https://cran.r-project.org/package=CatEncoders)
  - [dummies](https://cran.r-project.org/package=dummies)
  - [embed](https://github.com/tidymodels/embed)
  - [fastDummies](https://cran.r-project.org/package=fastDummies)
  - [FeatureHashing](https://cran.r-project.org/package=FeatureHashing)
  - [h2o](https://cran.r-project.org/package=h2o)
  - [makedummies](https://cran.r-project.org/package=makedummies)
  - [recipes](https://cran.r-project.org/package=recipes)
  - [vtreat](https://cran.r-project.org/package=vtreat)

## Installation

The development version can be installed from GitHub.

``` r
remotes::install_github("bfgray3/cattonum", ref = "develop")
```

The latest official release can be installed from CRAN.

``` r
install.packages("cattonum")
```

## Usage

``` r
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

## Code of Conduct

Please note that the ‘cattonum’ project is released with a [Contributor
Code of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this
project, you agree to abide by its terms.
