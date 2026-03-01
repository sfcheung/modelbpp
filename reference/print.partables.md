# Print a `partables`-Class Object

Print the content of a `partables`-class object.

## Usage

``` r
# S3 method for class 'partables'
print(x, max_tables = 10, ...)
```

## Arguments

- x:

  A `partables`-class object.

- max_tables:

  The maximum number of models to be printed. Default is 10.

- ...:

  Optional arguments. Ignored.

## Value

`x` is returned invisibly. Called for its side effect.

## Details

The print method for the output of
[`gen_models()`](https://sfcheung.github.io/modelbpp/reference/model_set.md),
[`get_add()`](https://sfcheung.github.io/modelbpp/reference/get_add.md),
and
[`get_drop()`](https://sfcheung.github.io/modelbpp/reference/get_drop.md).

## See also

[`gen_models()`](https://sfcheung.github.io/modelbpp/reference/model_set.md),
[`get_add()`](https://sfcheung.github.io/modelbpp/reference/get_add.md),
and
[`get_drop()`](https://sfcheung.github.io/modelbpp/reference/get_drop.md).

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
print(mod_to_add, max_tables = 1)
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
mod_to_drop <- get_drop(fit)
mod_to_drop
#> $`drop: x3~x2`
#>    id  lhs op  rhs user block group free ustart exo label plabel start   est
#> 1   1   x3  ~   x1    1     1     1    1     NA   0     a   .p1. 0.299 0.291
#> 2   2   x3  ~   x2    1     1     1    0      0   0     b   .p2. 0.000 0.000
#> 3   3   x4  ~   x1    1     1     1    2     NA   0     a   .p3. 0.299 0.291
#> 4   4   x3 ~~   x3    0     1     1    3     NA   0         .p4. 0.403 0.681
#> 5   5   x4 ~~   x4    0     1     1    4     NA   0         .p5. 0.617 0.616
#> 6   6   x3 ~~   x4    0     1     1    5     NA   0         .p6. 0.212 0.384
#> 7   7   x1 ~~   x1    0     1     1    0     NA   1         .p7. 1.109 1.109
#> 8   8   x1 ~~   x2    0     1     1    0     NA   1         .p8. 0.040 0.040
#> 9   9   x2 ~~   x2    0     1     1    0     NA   1         .p9. 1.148 1.148
#> 10 10 .p1. == .p3.    2     0     0    0     NA   0              0.000 0.000
#>       se
#> 1  0.056
#> 2  0.054
#> 3  0.056
#> 4  0.057
#> 5  0.087
#> 6  0.054
#> 7  0.000
#> 8  0.000
#> 9  0.000
#> 10 0.000
#> 
#> $`drop: x3~~x4`
#>    id  lhs op  rhs user block group free ustart exo label plabel start   est
#> 1   1   x3  ~   x1    1     1     1    1     NA   0     a   .p1. 0.299 0.293
#> 2   2   x3  ~   x2    1     1     1    2     NA   0     b   .p2. 0.372 0.511
#> 3   3   x4  ~   x1    1     1     1    3     NA   0     a   .p3. 0.299 0.293
#> 4   4   x3 ~~   x3    0     1     1    4     NA   0         .p4. 0.403 0.382
#> 5   5   x4 ~~   x4    0     1     1    5     NA   0         .p5. 0.617 0.616
#> 6   6   x3 ~~   x4    0     1     1    0      0   0         .p6. 0.000 0.000
#> 7   7   x1 ~~   x1    0     1     1    0     NA   1         .p7. 1.109 1.109
#> 8   8   x1 ~~   x2    0     1     1    0     NA   1         .p8. 0.040 0.040
#> 9   9   x2 ~~   x2    0     1     1    0     NA   1         .p9. 1.148 1.148
#> 10 10 .p1. == .p3.    2     0     0    0     NA   0              0.000 0.000
#>       se
#> 1  0.056
#> 2  0.054
#> 3  0.056
#> 4  0.057
#> 5  0.087
#> 6  0.054
#> 7  0.000
#> 8  0.000
#> 9  0.000
#> 10 0.000
#> 
print(mod_to_drop, max_tables = 1)
#> $`drop: x3~x2`
#>    id  lhs op  rhs user block group free ustart exo label plabel start   est
#> 1   1   x3  ~   x1    1     1     1    1     NA   0     a   .p1. 0.299 0.291
#> 2   2   x3  ~   x2    1     1     1    0      0   0     b   .p2. 0.000 0.000
#> 3   3   x4  ~   x1    1     1     1    2     NA   0     a   .p3. 0.299 0.291
#> 4   4   x3 ~~   x3    0     1     1    3     NA   0         .p4. 0.403 0.681
#> 5   5   x4 ~~   x4    0     1     1    4     NA   0         .p5. 0.617 0.616
#> 6   6   x3 ~~   x4    0     1     1    5     NA   0         .p6. 0.212 0.384
#> 7   7   x1 ~~   x1    0     1     1    0     NA   1         .p7. 1.109 1.109
#> 8   8   x1 ~~   x2    0     1     1    0     NA   1         .p8. 0.040 0.040
#> 9   9   x2 ~~   x2    0     1     1    0     NA   1         .p9. 1.148 1.148
#> 10 10 .p1. == .p3.    2     0     0    0     NA   0              0.000 0.000
#>       se
#> 1  0.056
#> 2  0.054
#> 3  0.056
#> 4  0.057
#> 5  0.087
#> 6  0.054
#> 7  0.000
#> 8  0.000
#> 9  0.000
#> 10 0.000
#> 
#> $`drop: x3~~x4`
#>    id  lhs op  rhs user block group free ustart exo label plabel start   est
#> 1   1   x3  ~   x1    1     1     1    1     NA   0     a   .p1. 0.299 0.293
#> 2   2   x3  ~   x2    1     1     1    2     NA   0     b   .p2. 0.372 0.511
#> 3   3   x4  ~   x1    1     1     1    3     NA   0     a   .p3. 0.299 0.293
#> 4   4   x3 ~~   x3    0     1     1    4     NA   0         .p4. 0.403 0.382
#> 5   5   x4 ~~   x4    0     1     1    5     NA   0         .p5. 0.617 0.616
#> 6   6   x3 ~~   x4    0     1     1    0      0   0         .p6. 0.000 0.000
#> 7   7   x1 ~~   x1    0     1     1    0     NA   1         .p7. 1.109 1.109
#> 8   8   x1 ~~   x2    0     1     1    0     NA   1         .p8. 0.040 0.040
#> 9   9   x2 ~~   x2    0     1     1    0     NA   1         .p9. 1.148 1.148
#> 10 10 .p1. == .p3.    2     0     0    0     NA   0              0.000 0.000
#>       se
#> 1  0.056
#> 2  0.054
#> 3  0.056
#> 4  0.057
#> 5  0.087
#> 6  0.054
#> 7  0.000
#> 8  0.000
#> 9  0.000
#> 10 0.000
#> 
```
