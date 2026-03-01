# A Sample Dataset Based On a Confirmatory Factor Analysis Model (For Testing)

Generated from a confirmatory factor analysis model (n = 200).

## Usage

``` r
dat_cfa
```

## Format

A data frame with six variables:

- x1:

  Indicator

- x2:

  Indicator

- x3:

  Indicator

- x4:

  Indicator

- x5:

  Indicator

- x6:

  Indicator

## Details

The model used to generate this dataset:

    f1 =~ x1 + x2 + x3 + x5
    f2 =~ x3 + x4 + x5 + x6
    f1 ~~ f2
