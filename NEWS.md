# modelbpp 0.1.2.19

## New Features

- Updated `model_set()` to work with
  user-supplied models. These models
  are supplied as parameter tables
  through the argument `partables`.
  (0.1.2.7)
- Updated the `print`-method of
  `model_set`-class objects. Users can
  set the prior probabilities of one or
  more models of their choice. (0.1.2.7)
- Updated `model_graph()` to plot
  user-supplied models. (0.1.2.7)
- Added a `c`-method for `partables`-class
  and `model_set`-class objects. For the
  ease of adding user models when calling
  `model_set()`. (0.1.2.7)
- Added more functions to manipulate
  a `partables`-class object. (0.1.2.8)
- Added a few helper functions for
  `partables`-class objects. (0.1.2.9, 0.1.2.12)
- Updated `model_graph()` to determine
  nested relation using the method
  by Bentler and Satorra (2010). This
  can be done only if `fixed.x`
  is set to `FALSE`. (0.1.2.10, 0.1.2.19)
- Users can supplied a named list of
  fitted models (`lavaan`-class objects)
  to `model_set()` through the
  argument `sem_out`. (0.1.2.17)

## Miscellaneous

- Updated info related to the first CRAN
  release (0.1.2.1).
- Added a logo.
- Suppress the warning from
  `lavaan::modificationIndices()` about
  equality constraints. (0.1.2.5)
- Updated `unique_models()` to handle
  user-supplied models. (0.1.2.6)
- Duplicated models with identical
  parameter tables
  will be removed by `model_set()`. (0.1.2.11)
- Improved the text in the `print`
  method of `partables`. (0.1.2.13)
- `model_set()` will check whether
  the sum of user-supplied prior
  probabilities is less than 1. (0.1.2.14)
- Updated the `print` method of
  `model_set` objects to print original
  model *df*s. (0.1.2.15)
- When calling `fit_many()`, can set the
  model with which`fit_many()` will compute
  model *df* difference. (0.1.2.16)
- Reorganized some test files. (0.1.2.18)

## Bug Fixes

- Fixed a bug in which error occurs in
  `model_set()` if no paths are dropped
  or no paths are added. (0.1.2.3)
- Corrected a typo in the help page of
  `gen_models()`'s argument, `output`.

# modelbpp 0.1.2

- First version submitted to CRAN.
- Fixed a few typos in Get Started. (0.1.0.9001)
- Used a bibliography file in vignettes. (0.1.0.9002)
- Disabled parallel processing in a test. (0.1.0.9002)
- Fixed typos in README.md. (0.1.0.9003)
- Used `tinytest` for tests. (0.1.0.9004 - 0.1.0.9006)
- Simplified some examples. (0.1.0.9007)
- Pregenerated some figures (0.1.1.9001)
- Finalized to 0.1.2.