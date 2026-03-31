# Changelog

## modelbpp 0.2.0.2

### Miscellaneous

- Add the argument `exclude_x_changed_to_y` to
  [`model_set()`](https://sfcheung.github.io/modelbpp/reference/model_set.md)
  and friends, as well as
  [`get_add()`](https://sfcheung.github.io/modelbpp/reference/get_add.md).
  Though this is rarely desirable, setting this argument to `FALSE`
  allow changes that make a “pure” x-variable a y-variable. (0.2.0.1)

- Add `drop_equivalent_models` to
  [`gen_models()`](https://sfcheung.github.io/modelbpp/reference/model_set.md).
  (0.2.0.2)

## modelbpp 0.2.0

CRAN release: 2026-03-01

### New Features

- **Breaking Changes**: Added the arguments `cross_add` and `cross_sets`
  to
  [`model_set()`](https://sfcheung.github.io/modelbpp/reference/model_set.md)
  and
  [`get_add()`](https://sfcheung.github.io/modelbpp/reference/get_add.md)
  to better handle factor loadings. Some factor loadings are no longer
  added by default, such as an indicator loading on two latent factors
  and one of the factors regresses on another. These cross-loadings are
  not meaningful in some models and may lead to nonconvergence. To
  reproduce results in previous versions of `modelbpp`, users may need
  to manually add these loadings using `cross_add` and `cross_sets`.
  (0.1.6.3)

- **Breaking Changes**: Added the argument
  `loadings_to_exclude_from_drop` to
  [`model_set()`](https://sfcheung.github.io/modelbpp/reference/model_set.md)
  (and `loadings_to_exclude` to
  [`get_add()`](https://sfcheung.github.io/modelbpp/reference/get_add.md))
  to better handle factor loadings. Some factor loadings will no longer
  be considered to be dropped by default, such as an indicator that
  loads on only one latent factor. Cross-loadings will still be
  considered. To reproduce results in previous versions of `modelbpp`,
  users may need to set `loadings_to_exclude_from_drop` to `"none"`.
  (0.1.6.3)

### Miscellaneous

- Added a few more tests (0.1.6.2)

- Updated
  [`gen_models()`](https://sfcheung.github.io/modelbpp/reference/model_set.md)
  with the new arguments added to
  [`model_set()`](https://sfcheung.github.io/modelbpp/reference/model_set.md).
  (0.1.6.4)

### Bug Fixes

- Fixed a bug in the internal function `add_list_duplicate_cov()`, which
  makes `must_not_add` failed to exclude some covariances. It should
  work now. (0.1.6.1)

## modelbpp 0.1.6

CRAN release: 2025-09-24

### New Features

- Added
  [`model_set_combined()`](https://sfcheung.github.io/modelbpp/reference/model_set_combined.md)
  for computing BPPs for models from two or more calls to
  [`model_set()`](https://sfcheung.github.io/modelbpp/reference/model_set.md).
  (0.1.5.3, 0.1.5.4)

### Miscellaneous

- The default of `more_fit_measures` of the print method of
  `model_set`-class object was changed to `c("cfi", "rmsea", "srmr)`.
  (0.1.5.1)

- Changed the vignettes to precomputed Rmarkdown files. (0.1.5.2)

## modelbpp 0.1.5

CRAN release: 2024-09-16

### New Features

- Added the argument `exclude_xy_cov` and `exclude_feedback` to
  [`get_add()`](https://sfcheung.github.io/modelbpp/reference/get_add.md)
  and
  [`model_set()`](https://sfcheung.github.io/modelbpp/reference/model_set.md),
  for excluding paths that create feedback loops, and covariances
  involving a predictor and an outcome variable (including those linked
  by indirect paths). Default values has been changed to `TRUE` since
  0.1.3.5. To reproduce results from previous version, set them to
  `FALSE`. (0.1.3.2, 0.1.3.5)

- Added `min_bpp_labelled` to
  [`model_graph()`](https://sfcheung.github.io/modelbpp/reference/model_graph.md),
  to hide the labels of models with small BPPs. (0.1.3.5)

- Added the argument `drop_equivalent_models`, to
  [`model_set()`](https://sfcheung.github.io/modelbpp/reference/model_set.md).
  If `TRUE`, the default, the models fitted will be checked for
  equivalence. If two or more more models are equivalent, only one of
  them will be retained. The groups of equivalent models identified, and
  the models dropped, will be printed by the print method. (0.1.3.9)

- Added
  [`measurement_invariance_models()`](https://sfcheung.github.io/modelbpp/reference/measurement_invariance_models.md),
  for generating metric and scalar invariance models and their partial
  invariance versions. (0.1.3.10 - 0.1.3.11)

### Miscellaneous

- Because it is very likely that users would like to see come fit
  measures along with BPPs, the default of `more_fit_measures` of the
  print method of `model_set`-class object changed to
  `c("cfi", "rmsea")`. (0.1.3.7)

- Revised
  [`fit_many()`](https://sfcheung.github.io/modelbpp/reference/fit_many.md)
  to support multigroup models. (0.1.3.8)

- A progress bar can be displayed when
  [`model_set()`](https://sfcheung.github.io/modelbpp/reference/model_set.md)
  is identifying nested models. (0.1.3.13)

- Shortened `BIC Posterior Probability` to `BPP` in some sections of the
  printout of
  [`print.model_set()`](https://sfcheung.github.io/modelbpp/reference/print.model_set.md).
  (0.1.3.14)

- Cumulative BPPs no longer displayed by default in
  [`print.model_set()`](https://sfcheung.github.io/modelbpp/reference/print.model_set.md).
  Print them by setting `cumulative_bpp` to `TRUE`. (0.1.3.15)

- Update an internal function to handle nonconvergence in checking
  nested relation. Only affect the graphs and only happen in some rare
  cases. (0.1.3.16)

### Bug Fixes

- The `must_not_add` argument should work now for some parameters that
  may not be recognized as interchangeable. (0.1.3.1)

- Fixed a bug in `must_not_drop` and `must_drop` of
  [`get_drop()`](https://sfcheung.github.io/modelbpp/reference/get_drop.md).
  They should work properly now. (0.1.3.5)

- Fixed a bug in
  [`model_graph()`](https://sfcheung.github.io/modelbpp/reference/model_graph.md).
  Short names should now be properly constructed. (0.1.3.3)

- Fixed some bugs in
  [`print.model_set()`](https://sfcheung.github.io/modelbpp/reference/print.model_set.md)
  about the printing of additional fit measures. (0.1.3.6, 0.1.3.7)

- Fixed a bug in checking whether two models are equivalent. (0.1.3.12)

## modelbpp 0.1.3

CRAN release: 2024-02-20

### New Features

#### `model_set()`

- Updated
  [`model_set()`](https://sfcheung.github.io/modelbpp/reference/model_set.md)
  to work with user-supplied models. These models are supplied as
  parameter tables through the argument `partables`. (0.1.2.7)
- Updated the `print`-method of `model_set`-class objects. Users can set
  the prior probabilities of one or more models of their choice.
  (0.1.2.7)
- Added a `c`-method for `partables`-class and `model_set`-class
  objects. For the ease of adding user models when calling
  [`model_set()`](https://sfcheung.github.io/modelbpp/reference/model_set.md).
  (0.1.2.7)
- Users can supplied a named list of fitted models (`lavaan`-class
  objects) to
  [`model_set()`](https://sfcheung.github.io/modelbpp/reference/model_set.md)
  through the argument `sem_out`. (0.1.2.17)
- The `print` method of
  [`model_set()`](https://sfcheung.github.io/modelbpp/reference/model_set.md)
  supports printing additional fit measures available from
  [`lavaan::fitMeasures()`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html).
  Check the argument `more_fit_measures`. (0.1.2.27)
- The `print` method of
  [`model_set()`](https://sfcheung.github.io/modelbpp/reference/model_set.md)
  support printing short model names, which can be used to interpret the
  output of
  [`model_graph()`](https://sfcheung.github.io/modelbpp/reference/model_graph.md).
  (0.1.2.29, 0.1.2.30)
- Updated
  [`model_graph()`](https://sfcheung.github.io/modelbpp/reference/model_graph.md)
  to determine nested relation using the method by Bentler and Satorra
  (2010). This can be done only if `fixed.x` is set to `FALSE`.
  (0.1.2.10, 0.1.2.19)
- A progress bar can be shown by
  [`model_graph()`](https://sfcheung.github.io/modelbpp/reference/model_graph.md)
  if nested relations need to be determined. (0.1.2.28)

#### `model_graph()`

- Updated
  [`model_graph()`](https://sfcheung.github.io/modelbpp/reference/model_graph.md)
  to plot user-supplied models. (0.1.2.7)
- Updated
  [`model_graph()`](https://sfcheung.github.io/modelbpp/reference/model_graph.md)
  with new options. If `drop_redundant_direct_paths` is `TRUE`
  (default), redundant direct paths will be removed.
- Models differ by more than one *df* will now be connected in
  [`model_graph()`](https://sfcheung.github.io/modelbpp/reference/model_graph.md).
  (0.1.2.20)
- New arguments are added to
  [`model_graph()`](https://sfcheung.github.io/modelbpp/reference/model_graph.md)
  to label arrows by model *df* differences (see `label_arrow_by_df`),
  and weight arrow widths by model *df* differences (see
  `weight_arrows_by_df`). (0.1.2.20)

#### Others

- Added more functions to manipulate a `partables`-class object.
  (0.1.2.8)
- Added a few helper functions for `partables`-class objects. (0.1.2.9,
  0.1.2.12)
- The models generating progress can display a progress bar. (0.1.2.24,
  0.1.2.25, 0.1.2.26)
- Users can request
  [`model_graph()`](https://sfcheung.github.io/modelbpp/reference/model_graph.md)
  to use short names in the graph, if they are created and stored by
  [`model_set()`](https://sfcheung.github.io/modelbpp/reference/model_set.md).
  (0.1.2.29, 0.1.2.30)

### Miscellaneous

- Updated info related to the first CRAN release (0.1.2.1).
- Added a logo.
- Suppress the warning from
  [`lavaan::modificationIndices()`](https://rdrr.io/pkg/lavaan/man/modificationIndices.html)
  about equality constraints. (0.1.2.5)
- Updated `unique_models()` to handle user-supplied models. (0.1.2.6)
- Duplicated models with identical parameter tables will be removed by
  [`model_set()`](https://sfcheung.github.io/modelbpp/reference/model_set.md).
  (0.1.2.11)
- Improved the text in the `print` method of `partables`. (0.1.2.13)
- [`model_set()`](https://sfcheung.github.io/modelbpp/reference/model_set.md)
  will check whether the sum of user-supplied prior probabilities is
  less than 1. (0.1.2.14)
- Updated the `print` method of `model_set` objects to print original
  model *df*s. (0.1.2.15)
- When calling
  [`fit_many()`](https://sfcheung.github.io/modelbpp/reference/fit_many.md),
  can set the model with
  which[`fit_many()`](https://sfcheung.github.io/modelbpp/reference/fit_many.md)
  will compute model *df* difference. (0.1.2.16)
- Reorganized some test files. (0.1.2.18)
- Improved the speed in identifying identical parameter tables.
  (0.1.2.22, 0.1.2.23)
- Revised several functions to handle nonconvergence. (0.1.2.31)
- Revised the print method for `model_set`-class object to print models
  that failed the past.check of `lavaan`. (0.1.2.32)

### Bug Fixes

- Fixed a bug in which error occurs in
  [`model_set()`](https://sfcheung.github.io/modelbpp/reference/model_set.md)
  if no paths are dropped or no paths are added. (0.1.2.3)
- Corrected a typo in the help page of
  [`gen_models()`](https://sfcheung.github.io/modelbpp/reference/model_set.md)’s
  argument, `output`.
- Fixed a bug in normalizing the width of arrows. (0.1.2.21)

## modelbpp 0.1.2

CRAN release: 2023-09-21

- First version submitted to CRAN.
- Fixed a few typos in Get Started. (0.1.0.9001)
- Used a bibliography file in vignettes. (0.1.0.9002)
- Disabled parallel processing in a test. (0.1.0.9002)
- Fixed typos in README.md. (0.1.0.9003)
- Used `tinytest` for tests. (0.1.0.9004 - 0.1.0.9006)
- Simplified some examples. (0.1.0.9007)
- Pregenerated some figures (0.1.1.9001)
- Finalized to 0.1.2.
