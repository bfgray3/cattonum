
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cattonum

[![Build
Status](https://travis-ci.org/bfgray3/cattonum.svg?branch=master)](https://travis-ci.org/bfgray3/cattonum)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/bfgray3/cattonum?branch=master&svg=true)](https://ci.appveyor.com/project/bfgray3/cattonum)
[![Coverage
status](https://codecov.io/gh/bfgray3/cattonum/branch/master/graph/badge.svg)](https://codecov.io/github/bfgray3/cattonum?branch=master)

`cattonum` (cat to num) turns categorical features into numeric
features. It is designed to provide a uniform API for various encodings
of categorical features and combines the best aspects of the several
other existing packages with similar functionality. The following are
hoped to eventually be included in the package:

  - dummy encoding
  - one-hot encoding
  - label encoding
  - frequency encoding
  - leave-one-out encoding
  - mean encoding
  - feature hashing (future)

There are many existing packages with which to encode categorical
features, including (among
    others):

  - [recipes](https://cran.r-project.org/web/packages/recipes/index.html)
  - [CatEncoders](https://cran.r-project.org/web/packages/CatEncoders/index.html)
  - [dummies](https://cran.r-project.org/web/packages/dummies/index.html)
  - [fastDummies](https://cran.r-project.org/web/packages/fastDummies/index.html)
  - [makedummies](https://cran.r-project.org/web/packages/makedummies/index.html)
  - [h2o](https://cran.r-project.org/web/packages/h2o/index.html)

## Installation

You can install `cattonum` from GitHub.

``` r
# install.packages("devtools")
devtools::install_github("bfgray3/cattonum")
```

## Example
