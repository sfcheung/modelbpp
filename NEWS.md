# modelbpp 0.1.3.14

## New Features

- Added the argument `exclude_xy_cov`
  and `exclude_feedback` to `get_add()`
  and `model_set()`, for excluding paths
  that create feedback loops, and
  covariances involving a predictor
  and an outcome variable (including
  those linked by indirect paths).
  Default values has been changed
  to `TRUE` since 0.1.3.5. To
  reproduce results from previous version,
  set them to `FALSE`. (0.1.3.2, 0.1.3.5)

- Added `min_bpp_labelled` to
  `model_graph()`, to hide the labels
  of models with small BPPs.
  (0.1.3.5)

- Added the argument `drop_equivalent_models`,
  to `model_set()`. If `TRUE`, the
  default, the models fitted will be
  checked for equivalence. If two or
  more more models are equivalent, only
  one of them will be retained.
  The groups of equivalent models identified,
  and the models dropped, will be
  printed by the print method. (0.1.3.9)

- Added `measurement_invariance_models()`,
  for generating metric and scalar
  invariance models and their partial
  invariance versions. (0.1.3.10 - 0.1.3.11)

## Miscellaneous

- Because it is very likely that users
  would like to see come fit measures
  along with BPPs, the default of
  `more_fit_measures` of the print
  method of `model_set`-class object
  changed to `c("cfi", "rmseas")`.
  (0.1.3.7)

- Revised `fit_many()` to support
  multigroup models. (0.1.3.8)

- A progress bar can be displayed when
  `model_set()` is identifying nested
  models. (0.1.3.13)

- Shortened `BIC Posterior Probability`
  to `BPP` in some sections of the
  printout of `print.model_set()`.
  (0.1.3.14)

## Bug Fixes

- The `must_not_add` argument should
  work now for some parameters that may
  not be recognized as interchangeable.
  (0.1.3.1)

- Fixed a bug in `must_not_drop` and
  `must_drop` of `get_drop()`. They
  should work properly now. (0.1.3.5)

- Fixed a bug in `model_graph()`.
  Short names should now be properly
  constructed. (0.1.3.3)

- Fixed some bugs in `print.model_set()`
  about the printing of additional fit
  measures. (0.1.3.6, 0.1.3.7)

- Fixed a bug in checking whether two
  models are equivalent. (0.1.3.12)

# modelbpp 0.1.3

## New Features

### `model_set()`

- Updated `model_set()` to work with
  user-supplied models. These models
  are supplied as parameter tables
  through the argument `partables`.
  (0.1.2.7)
- Updated the `print`-method of
  `model_set`-class objects. Users can
  set the prior probabilities of one or
  more models of their choice. (0.1.2.7)
- Added a `c`-method for `partables`-class
  and `model_set`-class objects. For the
  ease of adding user models when calling
  `model_set()`. (0.1.2.7)
- Users can supplied a named list of
  fitted models (`lavaan`-class objects)
  to `model_set()` through the
  argument `sem_out`. (0.1.2.17)
- The `print` method of `model_set()`
  supports printing additional fit
  measures available from
  `lavaan::fitMeasures()`. Check the
  argument `more_fit_measures`.
  (0.1.2.27)
- The `print` method of `model_set()`
  support printing short model names,
  which can be used to interpret
  the output of `model_graph()`.
  (0.1.2.29, 0.1.2.30)
- Updated `model_graph()` to determine
  nested relation using the method
  by Bentler and Satorra (2010). This
  can be done only if `fixed.x`
  is set to `FALSE`. (0.1.2.10, 0.1.2.19)
- A progress bar can be shown by
  `model_graph()` if nested relations
  need to be determined. (0.1.2.28)

### `model_graph()`

- Updated `model_graph()` to plot
  user-supplied models. (0.1.2.7)
- Updated `model_graph()` with new options.
  If `drop_redundant_direct_paths`
  is `TRUE` (default), redundant
  direct paths will be removed.
- Models differ by more than one *df*
  will now be connected in `model_graph()`.
  (0.1.2.20)
- New arguments are added to `model_graph()`
  to label arrows by model *df*
  differences (see `label_arrow_by_df`),
  and weight arrow widths by
  model *df* differences (see
  `weight_arrows_by_df`). (0.1.2.20)

### Others

- Added more functions to manipulate
  a `partables`-class object. (0.1.2.8)
- Added a few helper functions for
  `partables`-class objects. (0.1.2.9, 0.1.2.12)
- The models generating progress can
  display a progress bar.
  (0.1.2.24, 0.1.2.25, 0.1.2.26)
- Users can request `model_graph()`
  to use short names in the
  graph, if they are created and
  stored by `model_set()`.
  (0.1.2.29, 0.1.2.30)

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
- Improved the speed in identifying
  identical parameter tables.
  (0.1.2.22, 0.1.2.23)
- Revised several functions to handle
  nonconvergence. (0.1.2.31)
- Revised the print method for
  `model_set`-class object to print
  models that failed the past.check
  of `lavaan`. (0.1.2.32)

## Bug Fixes

- Fixed a bug in which error occurs in
  `model_set()` if no paths are dropped
  or no paths are added. (0.1.2.3)
- Corrected a typo in the help page of
  `gen_models()`'s argument, `output`.
- Fixed a bug in normalizing the width
  of arrows. (0.1.2.21)

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