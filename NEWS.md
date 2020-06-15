# cattonum 0.0.5

* This release has no changes to functionality.  Minimum versions for R and some dependencies have been increased, a `packageStartupMessage` has been added, and some tests have been updated.
* The following people have contributed to this release:
    * [romainfrancois](https://github.com/romainfrancois)

# cattonum 0.0.4

* This is a maintenance release due to a change in a dependency.  There are no user-facing changes.

# cattonum 0.0.3

* `catto_label()` no longer has a `seed` argument.  Users must now set their own seeds outside of cattonum functions.
* Added constructors `cattonum_df()` and `cattonum_df2()`, as well as `print()` and `summary()` methods for classes `cattonum_df` and `cattonum_df2`.
* The `catto_*()` functions now default the parameter `test` to `NULL` instead of checking `missing()`.
* The `catto_*()` functions are now generics and return an object of class `cattonum_df` or `cattonum_df2`.
* Logicals are now treated as a a two-level factor (@bcbeidel, #6).
* Added `catto_aggregate()`.
* The following people have contributed to this release:
    * [@bcbeidel](https://github.com/bcbeidel)

# cattonum 0.0.2

* `catto_label()` can now encode different columns with different orderings and encode columns with user-specified orderings.
* `catto_median()` has been added, thanks to Mark Roepke in [#10](https://github.com/bfgray3/cattonum/pull/10).
* `catto_dummy()` and `catto_onehot()` now both return a tibble when one is passed.
* The following people have contributed to this release:
    * [@dominickg](https://github.com/dominickg)
    * [@markroepke](https://github.com/markroepke)

# cattonum 0.0.1

* This is the first release of cattonum.  It currently includes the following encodings:

  * dummy encoding: `catto_dummy()`
  * frequency encoding: `catto_freq()`
  * label encoding: `catto_label()`
  * leave-one-out encoding: `catto_loo()`
  * mean encoding: `catto_mean()`
  * one-hot encoding: `catto_onehot()`
