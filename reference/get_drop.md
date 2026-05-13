# Models That Are More Restricted

Generate a list of models with one or more free parameter dropped (fixed
to zero).

## Usage

``` r
get_drop(
  sem_out,
  must_drop = NULL,
  must_not_drop = NULL,
  loadings_to_exclude = c("single", "none", "all"),
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

- must_drop:

  A character vector of parameters, named in
  [`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html) style
  (e.g., `"y ~ x"`), that must be included. Default is `NULL`.

- must_not_drop:

  A character vector of parameters, named in
  [`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html) style
  (e.g., `"x1 ~~ x1"`), that must not be included. Default is `NULL`.

- loadings_to_exclude:

  Whether factor loadings will be excluded. If `"single"`, then only
  "single" loadings (an indicator loads on only one latent factor) will
  be excluded. If `"all"`, then all factor loadings will be excluded. If
  `"none"`, then no loadings will be excluded. Be careful when using
  `"none"` because the models may not make sense. The settings in
  `must_drop` and `must_not_drop` will override this argument.

- df_change:

  How many degrees of freedom away in the list. All models with *df*
  change less than or equal to this number will be included, taking into
  account requirements set by other arguments. Default is 1.

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

It generates a list of models with one or more free parameters dropped,
that is, fixed to zero (with degrees of freedom, *df*, increases by one
or more).

All free parameters are included in the pool of candidates, except for
those explicitly requested to be kept.

The models will be checked by `lavaan` to make sure that the increase in
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
x4 ~ a*x1 + x2
ab := a*b
"

fit <- sem(mod, dat_path_model, fixed.x = TRUE)
mod_to_drop <- get_drop(fit)
mod_to_drop
#> $`drop: x3~x2`
#>    id  lhs op  rhs user block group free ustart exo label plabel start   est
#> 1   1   x3  ~   x1    1     1     1    1     NA   0     a   .p1. 0.282 0.279
#> 2   2   x3  ~   x2    1     1     1    0      0   0     b   .p2. 0.000 0.000
#> 3   3   x4  ~   x1    1     1     1    2     NA   0     a   .p3. 0.282 0.279
#> 4   4   x4  ~   x2    1     1     1    3     NA   0         .p4. 0.402 0.204
#> 5   5   x3 ~~   x3    0     1     1    4     NA   0         .p5. 0.382 0.683
#> 6   6   x4 ~~   x4    0     1     1    5     NA   0         .p6. 0.430 0.475
#> 7   7   x3 ~~   x4    0     1     1    6     NA   0         .p7. 0.148 0.264
#> 8   8   x1 ~~   x1    0     1     1    0     NA   1         .p8. 1.109 1.109
#> 9   9   x1 ~~   x2    0     1     1    0     NA   1         .p9. 0.040 0.040
#> 10 10   x2 ~~   x2    0     1     1    0     NA   1        .p10. 1.148 1.148
#> 11 11 .p1. == .p3.    2     0     0    0     NA   0              0.000 0.000
#>       se
#> 1  0.050
#> 2  0.058
#> 3  0.050
#> 4  0.061
#> 5  0.054
#> 6  0.061
#> 7  0.043
#> 8  0.000
#> 9  0.000
#> 10 0.000
#> 11 0.000
#> 
#> $`drop: x4~x2`
#>    id  lhs op  rhs user block group free ustart exo label plabel start   est
#> 1   1   x3  ~   x1    1     1     1    1     NA   0     a   .p1. 0.282 0.299
#> 2   2   x3  ~   x2    1     1     1    2     NA   0     b   .p2. 0.511 0.372
#> 3   3   x4  ~   x1    1     1     1    3     NA   0     a   .p3. 0.282 0.299
#> 4   4   x4  ~   x2    1     1     1    0      0   0         .p4. 0.000 0.000
#> 5   5   x3 ~~   x3    0     1     1    4     NA   0         .p5. 0.382 0.403
#> 6   6   x4 ~~   x4    0     1     1    5     NA   0         .p6. 0.430 0.617
#> 7   7   x3 ~~   x4    0     1     1    6     NA   0         .p7. 0.148 0.212
#> 8   8   x1 ~~   x1    0     1     1    0     NA   1         .p8. 1.109 1.109
#> 9   9   x1 ~~   x2    0     1     1    0     NA   1         .p9. 0.040 0.040
#> 10 10   x2 ~~   x2    0     1     1    0     NA   1        .p10. 1.148 1.148
#> 11 11 .p1. == .p3.    2     0     0    0     NA   0              0.000 0.000
#>       se
#> 1  0.050
#> 2  0.058
#> 3  0.050
#> 4  0.061
#> 5  0.054
#> 6  0.061
#> 7  0.043
#> 8  0.000
#> 9  0.000
#> 10 0.000
#> 11 0.000
#> 
#> $`drop: x3~~x4`
#>    id  lhs op  rhs user block group free ustart exo label plabel start   est
#> 1   1   x3  ~   x1    1     1     1    1     NA   0     a   .p1. 0.282 0.281
#> 2   2   x3  ~   x2    1     1     1    2     NA   0     b   .p2. 0.511 0.511
#> 3   3   x4  ~   x1    1     1     1    3     NA   0     a   .p3. 0.282 0.281
#> 4   4   x4  ~   x2    1     1     1    4     NA   0         .p4. 0.402 0.402
#> 5   5   x3 ~~   x3    0     1     1    5     NA   0         .p5. 0.382 0.382
#> 6   6   x4 ~~   x4    0     1     1    6     NA   0         .p6. 0.430 0.430
#> 7   7   x3 ~~   x4    0     1     1    0      0   0         .p7. 0.000 0.000
#> 8   8   x1 ~~   x1    0     1     1    0     NA   1         .p8. 1.109 1.109
#> 9   9   x1 ~~   x2    0     1     1    0     NA   1         .p9. 0.040 0.040
#> 10 10   x2 ~~   x2    0     1     1    0     NA   1        .p10. 1.148 1.148
#> 11 11 .p1. == .p3.    2     0     0    0     NA   0              0.000 0.000
#>       se
#> 1  0.050
#> 2  0.058
#> 3  0.050
#> 4  0.061
#> 5  0.054
#> 6  0.061
#> 7  0.043
#> 8  0.000
#> 9  0.000
#> 10 0.000
#> 11 0.000
#> 
```
