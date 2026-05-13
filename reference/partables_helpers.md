# Helper Functions For `partables`-Class Objects

For tasks such as comparing two parameter tables inside a
`partables`-class object.

## Usage

``` r
identical_partables(object1, object2)

is_partable(x)

same_variables(x)

get_partables(x)

to_partables(...)
```

## Arguments

- object1:

  A `lavaan` parameter table or similar object.

- object2:

  A `lavaan` parameter table or similar object.

- x:

  An object to be checked.

- ...:

  The objects to be combined.

## Value

The function `identical_partables()` returns either `TRUE` or `FALSE`.

The function `is_partable()` returns either `TRUE` or `FALSE`.

The function `same_variables()` returns either `TRUE` or `FALSE`.

The function `get_partables()` returns a `partables`-class object.

The function `to_partables()` returns a `partables`-class object,
created from the objects supplied.

## Details

The function `identical_partables()` compare two `lavaan` parameter
tables and see whether they are identical. (Adapted from a similar
function in the package `semhelpinghands`).

The function `is_partable()` tries to check whether an object is
"likely" to be a parameter table that can be used by
[`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html) and its
wrappers, such as
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html).

Note that there is no guarantee the the parameter table makes sense or
will not lead to error when fitted. It can only check whether it has the
required columns.

The function `same_variables()` check whether all parameter labels in a
`partables`-class object use the same observed variables.

The function `get_partables()` extract the `partable` object from a
`model_set`-class object.

The function `to_partables()` combine objects to create a
`partables`-class object. The objects to be combined can be a
`lavaan`-class object (e.g., the output of
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html)) or a
parameter table of `lavaan`.

## Examples

``` r

library(lavaan)
mod1 <-
"
x3 ~ x1
x2 ~ x4
"
mod2 <-
"
x2 ~ x4
x3 ~ x1
"
fit1 <- sem(mod1, dat_path_model)
fit2 <- sem(mod2, dat_path_model)
pt1 <- parameterTable(fit1)
pt2 <- parameterTable(fit2)
identical_partables(pt1, pt2)
#> [1] TRUE


is_partable(pt1)
#> [1] TRUE


out <- model_set(fit1,
                 fit_models = FALSE)
#> 
#> Generate 2 less restrictive model(s):
#>   |                                                  | 0 % ~calculating    |+++++++++++++++++++++++++                         | 50% ~00s            |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=00s  
#> 
#> Generate 3 more restrictive model(s):
#>   |                                                  | 0 % ~calculating    |+++++++++++++++++                                 | 33% ~00s            |++++++++++++++++++++++++++++++++++                | 67% ~00s            |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=00s  
#> 
#> Check for duplicated models (6 model[s] to check):
#>   |                                                          |                                                  |   0%  |                                                          |+++                                               |   7%  |                                                          |+++++++                                           |  13%  |                                                          |++++++++++                                        |  20%  |                                                          |+++++++++++++                                     |  27%  |                                                          |+++++++++++++++++                                 |  33%  |                                                          |++++++++++++++++++++                              |  40%  |                                                          |+++++++++++++++++++++++                           |  47%  |                                                          |+++++++++++++++++++++++++++                       |  53%  |                                                          |++++++++++++++++++++++++++++++                    |  60%  |                                                          |+++++++++++++++++++++++++++++++++                 |  67%  |                                                          |+++++++++++++++++++++++++++++++++++++             |  73%  |                                                          |++++++++++++++++++++++++++++++++++++++++          |  80%  |                                                          |+++++++++++++++++++++++++++++++++++++++++++       |  87%  |                                                          |+++++++++++++++++++++++++++++++++++++++++++++++   |  93%  |                                                          |++++++++++++++++++++++++++++++++++++++++++++++++++| 100%
same_variables(get_partables(out))
#> [1] TRUE



out <- model_set(fit1,
                 fit_models = FALSE)
#> 
#> Generate 2 less restrictive model(s):
#>   |                                                  | 0 % ~calculating    |+++++++++++++++++++++++++                         | 50% ~00s            |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=00s  
#> 
#> Generate 3 more restrictive model(s):
#>   |                                                  | 0 % ~calculating    |+++++++++++++++++                                 | 33% ~00s            |++++++++++++++++++++++++++++++++++                | 67% ~00s            |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=00s  
#> 
#> Check for duplicated models (6 model[s] to check):
#>   |                                                          |                                                  |   0%  |                                                          |+++                                               |   7%  |                                                          |+++++++                                           |  13%  |                                                          |++++++++++                                        |  20%  |                                                          |+++++++++++++                                     |  27%  |                                                          |+++++++++++++++++                                 |  33%  |                                                          |++++++++++++++++++++                              |  40%  |                                                          |+++++++++++++++++++++++                           |  47%  |                                                          |+++++++++++++++++++++++++++                       |  53%  |                                                          |++++++++++++++++++++++++++++++                    |  60%  |                                                          |+++++++++++++++++++++++++++++++++                 |  67%  |                                                          |+++++++++++++++++++++++++++++++++++++             |  73%  |                                                          |++++++++++++++++++++++++++++++++++++++++          |  80%  |                                                          |+++++++++++++++++++++++++++++++++++++++++++       |  87%  |                                                          |+++++++++++++++++++++++++++++++++++++++++++++++   |  93%  |                                                          |++++++++++++++++++++++++++++++++++++++++++++++++++| 100%
get_partables(out)
#> 
#> Call:
#> NULL
#> 
#> Number of parameter tables: 6
#> 
#> The modifications/models:
#> add: x3~x4
#> add: x2~x1
#> drop: x3~x1
#> drop: x2~x4
#> drop: x3~~x2
#> original 


fit1 <- sem(mod1, dat_path_model)
fit2 <- sem(mod2, dat_path_model)
pt1 <- parameterTable(fit1)
pt2 <- parameterTable(fit2)

to_partables(fit1, fit2)
#> 
#> Call:
#> NULL
#> 
#> Number of parameter tables: 2
#> 
#> The modifications/models:
#> fit1
#> fit2 
to_partables(pt1, pt2)
#> 
#> Call:
#> NULL
#> 
#> Number of parameter tables: 2
#> 
#> The modifications/models:
#> pt1
#> pt2 
```
