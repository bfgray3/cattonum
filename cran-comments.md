## Release summary

* `catto_label()` no longer has a `seed` argument.  Users must now set their own seeds outside of cattonum functions.
* Added constructors `cattonum_df()` and `cattonum_df2()`, as well as `print()` and `summary()` methods for classes `cattonum_df` and `cattonum_df2`.
* The `catto_*()` functions now default the parameter `test` to `NULL` instead of checking `missing()`.
* The `catto_*()` functions are now generics and return an object of class `cattonum_df` or `cattonum_df2`.
* Logicals are now treated as a a two-level factor (@bcbeidel, #6).
* Added `catto_aggregate()`.

## Test environments

* local: x86_64-apple-darwin15.6.0 (64-bit)
* appveyor: Windows Server 2012 R2 x64 (build 9600)
* travis: 3.3, 3.4, oldrel, release, devel
* TODO: r-hub: windows-x86_64-devel, ubuntu-gcc-release, fedora-clang-devel
* TODO: win-builder: windows-x86_64-devel

## R CMD check results

TODO: R CMD check results
0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies.
