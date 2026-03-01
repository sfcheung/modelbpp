# A Sample Dataset Based on a Path Model (For Testing)

Generated from the a path model (n = 100).

## Usage

``` r
dat_path_model
```

## Format

A data frame with four variables:

- x1:

  Predictor

- x2:

  Predictor

- x3:

  Mediator

- x4:

  Outcome

## Details

The model used to generate this dataset:

    x1 ~~ x2
    x3 ~ x1 + x2
    x4 ~ x3 + x1 + x2
