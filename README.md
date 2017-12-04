
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cattonum

[![Build
Status](https://travis-ci.org/bfgray3/cattonum.svg?branch=master)](https://travis-ci.org/bfgray3/cattonum)

`cattonum` (cat to num) is a brand new, under-construction package
designed to provide a uniform API for various encodings of categorical
features. It turns categorical features into numeric features. The
following are hoped to eventually be included in the package:

  - dummy encoding
  - one-hot encoding
  - label encoding
  - frequency encoding
  - leave-one-out encoding
  - mean encoding
  - feature hashing?

There are many existing packages with which to encode categorical
features, including (among
    others):

  - [CatEncoders](https://cran.r-project.org/web/packages/CatEncoders/index.html)
  - [dummies](https://cran.r-project.org/web/packages/dummies/index.html)
  - [fastDummies](https://cran.r-project.org/web/packages/fastDummies/index.html)
  - [makedummies](https://cran.r-project.org/web/packages/makedummies/index.html)
  - [sklearn.preprocessing](http://scikit-learn.org/stable/modules/classes.html#module-sklearn.preprocessing)
  - [sklearn.preprocessing.CategoricalEncoder](https://github.com/jorisvandenbossche/scikit-learn/blob/c39aa0c289a632cd22d733195a672f3b93265c61/sklearn/preprocessing/data.py)
  - [h2o](http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/algo-params/categorical_encoding.html)

The following are other resources to consult for information about
categorical
    encodings:

  - <https://www.coursera.org/learn/competitive-data-science>
  - <https://www.kaggle.com/c/caterpillar-tube-pricing/discussion/15748>
  - <https://medium.com/data-design/visiting-categorical-features-and-encoding-in-decision-trees-53400fa65931>
  - <http://www.willmcginnis.com/2015/11/29/beyond-one-hot-an-exploration-of-categorical-variables/>
  - <https://roamanalytics.com/2016/10/28/are-categorical-variables-getting-lost-in-your-random-forests/>

## Installation

You can install `cattonum` from GitHub.

``` r
# install.packages("devtools")
devtools::install_github("bfgray3/cattonum")
```

## Example
