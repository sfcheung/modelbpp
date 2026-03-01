# A Sample Dataset Based On a Serial Mediation Model With Weak Paths (For Testing)

Generated from a serial mediation model (n = 100).

## Usage

``` r
dat_serial_4_weak
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

The model to be fitted:

    m1 ~ x
    m2 ~ m1 + x
    y  ~ m2 + m1 + x
