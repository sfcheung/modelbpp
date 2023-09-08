# modelbpp 0.0.0.9009

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