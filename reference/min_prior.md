# Minimum Prior

Find the minimum prior probability required to achieve the desired BIC
posterior probability.

## Usage

``` r
min_prior(bic, bpp_target, target_name = "original")
```

## Arguments

- bic:

  A named vector of BIC values for a set of models.

- bpp_target:

  A value from zero to 1. The desired BIC posterior probability.

- target_name:

  The name of the original model, as appeared in the names of `bic`.

## Value

A scalar. The required prior probability.

## Details

It assumes that all models other than the original model have the same
prior probabilities.

This function is called by
[`model_set()`](https://sfcheung.github.io/modelbpp/reference/model_set.md)
or
[`print.model_set()`](https://sfcheung.github.io/modelbpp/reference/print.model_set.md)
and usually users do not need to call it. It is exported for advanced
users.

## References

Wu, H., Cheung, S. F., & Leung, S. O. (2020). Simple use of BIC to
assess model selection uncertainty: An illustration using mediation and
moderation models. *Multivariate Behavioral Research*, *55*(1), 1–16.
[doi:10.1080/00273171.2019.1574546](https://doi.org/10.1080/00273171.2019.1574546)

## See also

[`model_set()`](https://sfcheung.github.io/modelbpp/reference/model_set.md)
and
[`print.model_set()`](https://sfcheung.github.io/modelbpp/reference/print.model_set.md)

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

out <- model_set(fit)
#> 
#> Generate 2 less restrictive model(s):
#>   |                                                  | 0 % ~calculating    |+++++++++++++++++++++++++                         | 50% ~00s            |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=00s  
#> 
#> Generate 2 more restrictive model(s):
#>   |                                                  | 0 % ~calculating    |+++++++++++++++++++++++++                         | 50% ~00s            |++++++++++++++++++++++++++++++++++++++++++++++++++| 100% elapsed=00s  
#> 
#> Check for duplicated models (5 model[s] to check):
#>   |                                                          |                                                  |   0%  |                                                          |+++++                                             |  10%  |                                                          |++++++++++                                        |  20%  |                                                          |+++++++++++++++                                   |  30%  |                                                          |++++++++++++++++++++                              |  40%  |                                                          |+++++++++++++++++++++++++                         |  50%  |                                                          |++++++++++++++++++++++++++++++                    |  60%  |                                                          |+++++++++++++++++++++++++++++++++++               |  70%  |                                                          |++++++++++++++++++++++++++++++++++++++++          |  80%  |                                                          |+++++++++++++++++++++++++++++++++++++++++++++     |  90%  |                                                          |++++++++++++++++++++++++++++++++++++++++++++++++++| 100%
#> 
#> Fit the 5 model(s) (duplicated models removed):
min_prior(out$bic, bpp_target = .8)
#> [1] 0.9999998
```
