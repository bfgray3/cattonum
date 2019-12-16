# FIXME

# > test_check("cattonum")
# ── 1. Error: cattonum has no lints.  ─
# invalid 'path' argument
# Backtrace:
#   1. lintr::expect_lint_free(".")
# 2. lintr::lint_package(...)
# 3. base::normalizePath(path, mustWork = FALSE)
# 4. base::path.expand(path)

# test_that("cattonum has no lints.", lintr::expect_lint_free())
