# Models That Are Less Restricted

Generate a list of models with one or more fixed parameter freed.

## Usage

``` r
get_add(
  sem_out,
  must_add = NULL,
  must_not_add = NULL,
  remove_constraints = TRUE,
  exclude_error_cov = TRUE,
  exclude_feedback = FALSE,
  exclude_xy_cov = FALSE,
  cross_add = c("pure_x", "pure_y"),
  cross_sets = NULL,
  df_change = 1,
  model_id = NA,
  keep_correct_df_change = TRUE,
  remove_duplicated = TRUE,
  progress = FALSE
)
```

## Arguments

- sem_out:

  The original model, which is the output from an structural equation
  modeling function. Currently support
  [lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
  objects only.

- must_add:

  A character vector of parameters, named in
  [`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html) style
  (e.g., `"y ~ x"`), that must be added. Default is `NULL`.

- must_not_add:

  A character vector of parameters, named in
  [`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html) style
  (e.g., `"x1 ~~ x1"`), that must not be added. Default is `NULL`.

- remove_constraints:

  Whether equality constraints will be removed. Default is `TRUE`.

- exclude_error_cov:

  Exclude error covariances of indicators. Default is `TRUE`.

- exclude_feedback:

  Exclude paths that will result in a feedback loop. For example, if
  there is path from `x` through `m` to `y`, then the path `x ~ y` will
  create a feedback loop. Default is `FALSE` for now, to maintain
  backward compatibility. Do not rely on the default value because it
  will be changed to `TRUE` in a future major version.

- exclude_xy_cov:

  Exclude covariance between two variables, in which one has a path to
  another. For example, if there is path from `x` through `m` to `y`,
  then the covariance `x ~~ y`, which denotes the covariance between `x`
  and the error term of `y`, will be excluded if this argument is
  `TRUE`. Default is `FALSE` for now, to maintain backward
  compatibility. Do not rely on the default value because it will be
  changed to `TRUE` in a future major version.

- cross_add:

  A character vector of whether and how cross-loadings (an indicator
  loads on two or more latent factors) will be added. If `NULL`, no
  cross-loadings will be added. If `"lav_x"` is in the vector, then
  cross-loadings among "pure-x" latent factors, latent factors that do
  not regress on any other variables, will be considered. If `"lav_y"`
  is in the vector, then cross-loadings among "pure-y" latent factors,
  latent factors that regress on at least one variables but they
  themselves do not predict any other variables, will be considered. If
  `"user"` is in the vector, then cross-loadings among latent factors
  listed in `cross_sets` will be considered.

- cross_sets:

  A character vector of latent variables for which cross-loadings will
  be considered. Ignored if `"user"` is not in `cross_add`. Note that
  cross-loadings involving latent variables which are mediators will not
  be considered. To add them, they must be specified explicitly in
  `must_add`.

- df_change:

  How many degrees of freedom (*df*) away in the list. All models with
  *df* change less than or equal to this number will be included, taking
  into account requirements set by other arguments. Default is 1.

- model_id:

  The identification number of the starting model. Default is `NA`, no
  identification number.

- keep_correct_df_change:

  Keep only models with actual *df* change equal to expected *df*
  change.

- remove_duplicated:

  If `TRUE`, the default, duplicated models are removed.

- progress:

  Whether a progress bar will be displayed, implemented by the `pbapply`
  package. Default is `FALSE`.

## Value

An object of the class `partables`, a named list of parameter tables,
each of them to be used by
[`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html) or
[`update()`](https://rdrr.io/r/stats/update.html) for fitting a model
with the added parameters.

## Details

It generates a list of models with one or more fixed parameter freed
(and the degrees of freedom, *df*, increases by one or more). If a model
has equality constraints, models with one or more of the constraints
between two free parameters released will also be included.

Graphically, paths or covariances are "added" to form the list of
models.

The models to be included are identified by
[`lavaan::modificationIndices()`](https://rdrr.io/pkg/lavaan/man/modificationIndices.html).

The models will be checked by `lavaan` to make sure that the decrease in
model degrees of freedom is of the expected value.

This function is called by
[`model_set()`](https://sfcheung.github.io/modelbpp/reference/model_set.md)
and usually users do not need to call it. It is exported for advanced
users.

## See also

[`print.partables()`](https://sfcheung.github.io/modelbpp/reference/print.partables.md)

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r
library(lavaan)
dat <- dat_path_model
mod <-
"
x3 ~ a*x1 + b*x2
x4 ~ a*x1
ab := a*b
"
fit <- sem(mod, dat_path_model, fixed.x = TRUE)
mod_to_add <- get_add(fit)
mod_to_add
#> $`add: x4~x2`
#>    id  lhs op  rhs user block group free ustart exo label plabel level start
#> 1   1   x3  ~   x1    1     1     1    1  0.299   0     a   .p1.     1 0.299
#> 2   2   x3  ~   x2    1     1     1    2  0.372   0     b   .p2.     1 0.372
#> 3   3   x4  ~   x1    1     1     1    3  0.299   0     a   .p3.     1 0.299
#> 4   4   x3 ~~   x3    1     1     1    4  0.403   0         .p4.     1 0.403
#> 5   5   x4 ~~   x4    1     1     1    5  0.617   0         .p5.     1 0.617
#> 6   6   x3 ~~   x4    1     1     1    6  0.212   0         .p6.     1 0.212
#> 7   7   x1 ~~   x1    1     1     1    0  1.109   1         .p7.     1 1.109
#> 8   8   x1 ~~   x2    1     1     1    0  0.040   1         .p8.     1 0.040
#> 9   9   x2 ~~   x2    1     1     1    0  1.148   1         .p9.     1 1.148
#> 10 10 .p1. == .p3.    1     0     0    0  0.000   0                  0 0.000
#> 11 11   x4  ~   x2    1     1     1    7  0.000   0                  1 0.000
#>      est se
#> 1  0.282 NA
#> 2  0.511 NA
#> 3  0.282 NA
#> 4  0.382 NA
#> 5  0.430 NA
#> 6  0.148 NA
#> 7  1.109 NA
#> 8  0.040 NA
#> 9  1.148 NA
#> 10 0.000 NA
#> 11 0.402 NA
#> 
#> $`add: (x3~x1),(x4~x1)`
#>   id lhs op rhs user block group free ustart exo label plabel start   est se
#> 1  1  x3  ~  x1    1     1     1    1     NA   0         .p1. 0.299 0.319 NA
#> 2  2  x3  ~  x2    1     1     1    2     NA   0     b   .p2. 0.372 0.369 NA
#> 3  3  x4  ~  x1    1     1     1    3     NA   0         .p3. 0.299 0.259 NA
#> 4  4  x3 ~~  x3    0     1     1    4     NA   0         .p4. 0.403 0.404 NA
#> 5  5  x4 ~~  x4    0     1     1    5     NA   0         .p5. 0.617 0.615 NA
#> 6  6  x3 ~~  x4    0     1     1    6     NA   0         .p6. 0.212 0.215 NA
#> 7  7  x1 ~~  x1    0     1     1    0     NA   1         .p7. 1.109 1.109 NA
#> 8  8  x1 ~~  x2    0     1     1    0     NA   1         .p8. 0.040 0.040 NA
#> 9  9  x2 ~~  x2    0     1     1    0     NA   1         .p9. 1.148 1.148 NA
#> 
```
