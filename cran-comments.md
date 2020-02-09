## Release summary

This is a maintenance release due to a change in dependency {tidyselect}.  There are no user-facing changes.

## Test environments

* local: macOS Mojave 10.14.6
* GitHub Actions: windows-latest, macOS-latest, ubuntu-16.04
    * The Windows build is erroring with `vignette builder 'knitr' not found` but I believe this is unrelated to cattonum.  This is not happening in other test environments and {knitr} is in `Suggests` in `DESCRIPTION`.
* r-hub: x86_64-pc-linux-gnu (64-bit)

## R CMD check results

R CMD check results
0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies.
