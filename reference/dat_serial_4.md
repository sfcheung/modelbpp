# A Sample Dataset Based On a Serial Mediation Model (For Testing)

Generated from a serial mediation model (n = 100).

## Usage

``` r
dat_serial_4
```

## Format

A data frame with four variables:

- x:

  Predictor

- m1:

  Mediator

- m2:

  Mediator

- y:

  Outcome

## Details

The model used to generate this dataset:

    m1 ~ x
    m2 ~ m1
    y  ~ m2
