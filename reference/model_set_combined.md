# Two or More Hypothesized Models

Combine the 'model_set()' results of two or more hypothesis models.

## Usage

``` r
model_set_combined(model_set_outputs, ...)
```

## Arguments

- model_set_outputs:

  This must be a named list of the outputs of
  [`model_set()`](https://sfcheung.github.io/modelbpp/reference/model_set.md).
  The names will be used as prefixes to name the models.

- ...:

  Additional arguments to be passed to
  [`model_set()`](https://sfcheung.github.io/modelbpp/reference/model_set.md).

## Value

A `model_set`-class object, which is simply an output of
[`model_set()`](https://sfcheung.github.io/modelbpp/reference/model_set.md).
All methods and functions for the output of
[`model_set()`](https://sfcheung.github.io/modelbpp/reference/model_set.md)
will also work on this object.

## Details

There are cases in which users have more than one hypothesized model,
each with its own set of neighboring models.

The function `model_set_combined()` let users combine the
[`model_set()`](https://sfcheung.github.io/modelbpp/reference/model_set.md)
results two or more hypothesized model. Users can then compare the BPPs
of these hypothesized models, as well as their neighboring models.
Equivalent models will be removed in the process.

## See also

[`model_set()`](https://sfcheung.github.io/modelbpp/reference/model_set.md)

## Examples

``` r

library(lavaan)

mod1 <-
"
x4 ~ x1
x7 ~ x4
"

mod2 <-
"
x1 ~ x4
x7 ~ x4
"

fit1 <- sem(mod1,
            HolzingerSwineford1939,
            fixed.x = FALSE)
fit2 <- sem(mod2,
            HolzingerSwineford1939,
            fixed.x = FALSE)

out1 <- model_set(fit1)
#> 
#> Generate 1 less restrictive model(s):
#>   |                                                  | 0 % ~calculating    |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=00s  
#> 
#> Generate 2 more restrictive model(s):
#>   |                                                  | 0 % ~calculating    |+++++++++++++++++++++++++                         | 50% ~00s            |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=00s  
#> 
#> Check for duplicated models (4 model[s] to check):
#>   |                                                          |                                                  |   0%  |                                                          |++++++++                                          |  17%  |                                                          |+++++++++++++++++                                 |  33%  |                                                          |+++++++++++++++++++++++++                         |  50%  |                                                          |+++++++++++++++++++++++++++++++++                 |  67%  |                                                          |++++++++++++++++++++++++++++++++++++++++++        |  83%  |                                                          |++++++++++++++++++++++++++++++++++++++++++++++++++| 100%
#> 
#> Fit the 4 model(s) (duplicated models removed):
#> 
#> Check for nested models (6 pair[s] of models to check):
#>   |                                                          |                                                  |   0%  |                                                          |++++++++                                          |  17%  |                                                          |+++++++++++++++++                                 |  33%  |                                                          |+++++++++++++++++++++++++                         |  50%  |                                                          |+++++++++++++++++++++++++++++++++                 |  67%  |                                                          |++++++++++++++++++++++++++++++++++++++++++        |  83%  |                                                          |++++++++++++++++++++++++++++++++++++++++++++++++++| 100%
out2 <- model_set(fit2)
#> 
#> Generate 3 more restrictive model(s):
#>   |                                                  | 0 % ~calculating    |+++++++++++++++++                                 | 33% ~00s            |++++++++++++++++++++++++++++++++++                | 67% ~00s            |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=00s  
#> 
#> Check for duplicated models (4 model[s] to check):
#>   |                                                          |                                                  |   0%  |                                                          |++++++++                                          |  17%  |                                                          |+++++++++++++++++                                 |  33%  |                                                          |+++++++++++++++++++++++++                         |  50%  |                                                          |+++++++++++++++++++++++++++++++++                 |  67%  |                                                          |++++++++++++++++++++++++++++++++++++++++++        |  83%  |                                                          |++++++++++++++++++++++++++++++++++++++++++++++++++| 100%
#> 
#> Fit the 4 model(s) (duplicated models removed):
#> 
#> Check for nested models (6 pair[s] of models to check):
#>   |                                                          |                                                  |   0%  |                                                          |++++++++                                          |  17%  |                                                          |+++++++++++++++++                                 |  33%  |                                                          |+++++++++++++++++++++++++                         |  50%  |                                                          |+++++++++++++++++++++++++++++++++                 |  67%  |                                                          |++++++++++++++++++++++++++++++++++++++++++        |  83%  |                                                          |++++++++++++++++++++++++++++++++++++++++++++++++++| 100%

out1
#> 
#> Call:
#> model_set(sem_out = fit1)
#> 
#> Number of model(s) fitted           : 4
#> Number of model(s) converged        : 4
#> Number of model(s) passed post.check: 4
#> 
#> The models (sorted by BPP):
#>             model_df df_diff Prior      BIC   BPP   cfi rmsea  srmr
#> original           1       0 0.250 2770.178 0.814 1.000 0.000 0.001
#> drop: x7~x4        2      -1 0.250 2773.706 0.139 0.859 0.110 0.076
#> add: x7~x1         0       1 0.250 2775.883 0.047 1.000 0.000 0.000
#> drop: x4~x1        2      -1 0.250 2809.487 0.000 0.161 0.267 0.155
#> 
#> Note:
#> - BIC: Bayesian Information Criterion.
#> - BPP: BIC posterior probability.
#> - model_df: Model degrees of freedom.
#> - df_diff: Difference in df compared to the original/target model.
#> - To show cumulative BPPs, call print() with 'cumulative_bpp = TRUE'.
#> - Since Version 0.1.6.3, the default ways to handle factor loadings
#>   have changed. Check the NEWS by news(package = 'modelbpp') to see how
#>   to reproduce results from previous versions.
out2
#> 
#> Call:
#> model_set(sem_out = fit2)
#> 
#> Number of model(s) fitted           : 4
#> Number of model(s) converged        : 4
#> Number of model(s) passed post.check: 4
#> 
#> The models (sorted by BPP):
#>              model_df df_diff Prior      BIC   BPP   cfi rmsea  srmr
#> drop: x1~~x7        1      -1 0.250 2770.178 0.937 1.000 0.000 0.001
#> original            0       0 0.250 2775.883 0.054 1.000 0.000 0.000
#> drop: x7~x4         1      -1 0.250 2779.412 0.009 0.839 0.165 0.076
#> drop: x1~x4         1      -1 0.250 2815.193 0.000 0.141 0.382 0.154
#> 
#> Note:
#> - BIC: Bayesian Information Criterion.
#> - BPP: BIC posterior probability.
#> - model_df: Model degrees of freedom.
#> - df_diff: Difference in df compared to the original/target model.
#> - To show cumulative BPPs, call print() with 'cumulative_bpp = TRUE'.
#> - Since Version 0.1.6.3, the default ways to handle factor loadings
#>   have changed. Check the NEWS by news(package = 'modelbpp') to see how
#>   to reproduce results from previous versions.

outb <- model_set_combined(
            list(fit1 = out1,
                 fit2 = out2))
#> 
#> Check for nested models (28 pair[s] of models to check):
#>   |                                                          |                                                  |   0%  |                                                          |++                                                |   4%  |                                                          |++++                                              |   7%  |                                                          |+++++                                             |  11%  |                                                          |+++++++                                           |  14%  |                                                          |+++++++++                                         |  18%  |                                                          |+++++++++++                                       |  21%  |                                                          |++++++++++++                                      |  25%  |                                                          |++++++++++++++                                    |  29%  |                                                          |++++++++++++++++                                  |  32%  |                                                          |++++++++++++++++++                                |  36%  |                                                          |++++++++++++++++++++                              |  39%  |                                                          |+++++++++++++++++++++                             |  43%  |                                                          |+++++++++++++++++++++++                           |  46%  |                                                          |+++++++++++++++++++++++++                         |  50%  |                                                          |+++++++++++++++++++++++++++                       |  54%  |                                                          |+++++++++++++++++++++++++++++                     |  57%  |                                                          |++++++++++++++++++++++++++++++                    |  61%  |                                                          |++++++++++++++++++++++++++++++++                  |  64%  |                                                          |++++++++++++++++++++++++++++++++++                |  68%  |                                                          |++++++++++++++++++++++++++++++++++++              |  71%  |                                                          |++++++++++++++++++++++++++++++++++++++            |  75%  |                                                          |+++++++++++++++++++++++++++++++++++++++           |  79%  |                                                          |+++++++++++++++++++++++++++++++++++++++++         |  82%  |                                                          |+++++++++++++++++++++++++++++++++++++++++++       |  86%  |                                                          |+++++++++++++++++++++++++++++++++++++++++++++     |  89%  |                                                          |++++++++++++++++++++++++++++++++++++++++++++++    |  93%  |                                                          |++++++++++++++++++++++++++++++++++++++++++++++++  |  96%  |                                                          |++++++++++++++++++++++++++++++++++++++++++++++++++| 100%

outb
#> 
#> Call:
#> model_set(sem_out = all_fits)
#> 
#> Number of model(s) fitted           : 6
#> Number of model(s) converged        : 6
#> Number of model(s) passed post.check: 6
#> 
#> The models (sorted by BPP):
#>                  model_df df_diff Prior      BIC   BPP   cfi rmsea  srmr
#> fit1_original           1      NA 0.167 2770.178 0.807 1.000 0.000 0.001
#> fit1_drop: x7~x4        2      NA 0.167 2773.706 0.138 0.859 0.110 0.076
#> fit2_original           0      NA 0.167 2775.883 0.047 1.000 0.000 0.000
#> fit2_drop: x7~x4        1      NA 0.167 2779.412 0.008 0.839 0.165 0.076
#> fit1_drop: x4~x1        2      NA 0.167 2809.487 0.000 0.161 0.267 0.155
#> fit2_drop: x1~x4        1      NA 0.167 2815.193 0.000 0.141 0.382 0.154
#> 
#> Models that are equivalent:
#>   Cluster                         
#> 1 fit2_original, fit1_add: x7~x1  
#> 2 fit1_original, fit2_drop: x1~~x7
#> 
#> Equivalent model(s) excluded from the analysis:
#> fit1_add: x7~x1, fit2_drop: x1~~x7
#> 
#> Note:
#> - BIC: Bayesian Information Criterion.
#> - BPP: BIC posterior probability.
#> - model_df: Model degrees of freedom.
#> - df_diff: Difference in df compared to the original/target model.
#> - To show cumulative BPPs, call print() with 'cumulative_bpp = TRUE'.
#> - Since Version 0.1.6.3, the default ways to handle factor loadings
#>   have changed. Check the NEWS by news(package = 'modelbpp') to see how
#>   to reproduce results from previous versions.
```
