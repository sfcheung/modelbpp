# modelbpp 0.0.0.9016

- Migrating. (0.0.0.9001)
- Initial setup. (0.0.0.9001)
- Migrated all files from the previous
  repo. (0.0.0.9001)
- Parameters and constraints are stored
  as lists instead of strings. (0.0.0.9002)
- Factored some lines from `get_add()`
  and `get_drop()` to internal helpers.
  (0.0.0.9002)
- Main functions now return custom
  S3 class objects. (0.0.0.9003)
- Added a print method for
  `partables`-class objects. (0.0.0.9004)
- Added a print method for
  `sem_outs`-class objects. (0.0.0.9005)
- Added a print method for
  `model_set`-class objects. (0.0.0.9006)
- Updated the help pages of functions
  (0.0.0.9007)
- Users can set the prior probability
  of the target model. (0.0.0.9008)
- Users can reuse model list from the
  output of `model_se()`. (0.0.0.9008)
- Added `min_prior()` for computing
  the minimum prior required to achieve
  the desired minimum BIC posterior
  probability. (0.0.0.9008)
- Added `model_graph()` for plotting
  models as a network, with sizes affected
  by BIC posterior probabilities.
  (0.0.0.9009)
- Added the argument `compute_bpp` to
  `model_set()`. BPPs will be computed
  only if is it `TRUE`. (0.0.0.9010)
- Add `gen_models()`, a wrapper of
  `model_set()` for generating
  a list of models. (0.0.0.9011)
- Renamed `postprob` to `bpp`, for
  consistency in naming. (0.0.0.9012)
- Changed the default of `output`
  of `gen_models()` to `"partables"`.
  (0.0.0.9013)
- The default of `fit_models` of
  `model_set()` depends on the input.
  If models have been fitted, they will
  not be refitted by default. (0.0.0.9014)
- Added `check_sem_out()` to check whether
  the `sem_out`` argument is of a
  supported type. (0.0.0.9015)
- Updated the vignette. (0.0.0.9016)