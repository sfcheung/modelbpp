# Measurement Invariance Models

Generate metric and scalar invariance models and their partial
invariance versions.

## Usage

``` r
measurement_invariance_models(
  cfa_out,
  max_free = 1,
  metric = TRUE,
  scalar = TRUE,
  progress = TRUE
)
```

## Arguments

- cfa_out:

  The output of
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html).

- max_free:

  The maximum number of constraints to be released when generating the
  partial invariance models. For example, if set to 1, then only the
  partial metric invariance model only has at most one item allowed to
  have different loadings across group. Default is 1. If set to zero,
  then no partial invariance models will be generated.

- metric:

  Logical. If `TRUE`, the default, then metric invariance model and its
  partial invariance versions are generated.

- scalar:

  Logical. If `TRUE`, the default, then scalar invariance model and its
  partial invariance versions are generated.

- progress:

  Logical. If `TRUE`, the default, progress bars will be displayed when
  fitting partial invariance models.

## Value

A list of [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html)
output. The names are automatically generated to indicate whether a
model is configural, metric, or scalar invariance, or the item(s)
without between-group constraints on the loadings (for partial metric
invariance) or intercepts (for partial scalar invariance).

## Details

This a helper function to generate, based on a multigroup confirmatory
factor analysis (CFA) model with no between-group equality constraints,
the following models:

- A metric invariance model (loadings constrained to be equal across
  groups).

- A scalar invariance model (intercepts and loadings constrained to be
  equal across groups).

- Partial invariance versions of the previous two models, such as a
  model with the loadings of all items, except for one, constrained to
  be equal across groups.

The models generated can then be used in
[`model_set()`](https://sfcheung.github.io/modelbpp/reference/model_set.md)
to compute BPPs.

### Requirements

The model used as the input needs to be fitted with no between group
constrains, that is, it is a configural invariance model. Although not a
must, it is advised to use the default way to identify each factor (that
is, fixing a loading to one).

### Implementation

This function simply use the `group.partial` and `group.equal` argument
of [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html) to
generate the models.

## See also

[`model_set()`](https://sfcheung.github.io/modelbpp/reference/model_set.md)

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r

library(lavaan)
# For illustration, only one factor is used,
# with one item from another factor added
# just to make the model not saturated.
HSmod <-
"
spatial =~ x1 + x2 + x3 + x4
"
fit_config <- cfa(model = HSmod,
                  data = HolzingerSwineford1939,
                  group = "school")
fit_mi <- measurement_invariance_models(fit_config)
#> 
#> Fitting 3 partial metric invariance models:
#> 
#> Fitting 4 partial scalar invariance models:
names(fit_mi)
#>  [1] "config"      "metric"      "scalar"      "spatial=~x2" "spatial=~x3"
#>  [6] "spatial=~x4" "x1~1"        "x2~1"        "x3~1"        "x4~1"       
# Need to add 'skip_check_sem_out = TRUE' to use multigroup models.
out <- model_set(sem_out = fit_mi,
                 skip_check_sem_out = TRUE,
                 progress = FALSE,
                 parallel = FALSE)
print(out)
#> 
#> Call:
#> model_set(sem_out = fit_mi, parallel = FALSE, progress = FALSE, 
#>     skip_check_sem_out = TRUE)
#> 
#> Number of model(s) fitted           : 10
#> Number of model(s) converged        : 10
#> Number of model(s) passed post.check: 10
#> 
#> The models (sorted by BPP):
#>             model_df df_diff Prior      BIC   BPP   cfi rmsea  srmr
#> metric             7      NA 0.100 3694.067 0.713 0.934 0.102 0.052
#> x3~1               9      NA 0.100 3697.849 0.108 0.855 0.134 0.073
#> spatial=~x2        6      NA 0.100 3698.785 0.067 0.934 0.110 0.051
#> spatial=~x3        6      NA 0.100 3699.109 0.057 0.932 0.112 0.053
#> spatial=~x4        6      NA 0.100 3699.429 0.049 0.930 0.114 0.049
#> x4~1               9      NA 0.100 3705.119 0.003 0.812 0.152 0.081
#> config             4      NA 0.100 3705.216 0.003 0.952 0.115 0.038
#> scalar            10      NA 0.100 3720.435 0.000 0.692 0.185 0.105
#> x2~1               9      NA 0.100 3721.705 0.000 0.712 0.188 0.099
#> x1~1               9      NA 0.100 3725.973 0.000 0.687 0.197 0.105
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
