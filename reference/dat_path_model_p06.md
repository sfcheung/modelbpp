# A Sample Dataset Based On a Complex Path Model (For Testing)

Generated from a complex path model (n = 200).

## Usage

``` r
dat_path_model_p06
```

## Format

A data frame with six variables:

- x1:

  Predictor

- x2:

  Predictor

- x3:

  Predictor

- y4:

  Mediator

- y5:

  Mediator

- y6:

  Outcome

## Details

The model used to generate this dataset:

    y4 ~  x1 + x2 + x3
    y5 ~  y4 + x1 + x2
    y6 ~  y4 + y5 + x1 + x2 + x3
    x1 ~~ x2 + x3
    x2 ~~ x3
