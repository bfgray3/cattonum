## cattonum 0.0.2

* `catto_label` can now encode different columns with different orderings.
* `catto_median` has been added, thanks to Mark Roepke in [#10](https://github.com/bfgray3/cattonum/pull/10).
* `catto_dummy` and `catto_onehot` now both return a `tibble` when one is passed.
* The following people have contributed to this release:
    * [@dominickg](https://github.com/dominickg)
    * [@markroepke](https://github.com/markroepke)

## cattonum 0.0.1

* This is the first release of `cattonum`.  It currently includes the following encodings:

  * dummy encoding: `catto_dummy`
  * frequency encoding: `catto_freq`
  * label encoding: `catto_label`
  * leave-one-out encoding: `catto_loo`
  * mean encoding: `catto_mean`
  * one-hot encoding: `catto_onehot`