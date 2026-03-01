# A Sample Dataset Based On a Structural Model (For Testing)

Generated from a structural model with latent variables (n = 250).

## Usage

``` r
dat_sem
```

## Format

An object of class `data.frame` with 250 rows and 16 columns.

## Details

The model to be fitted:

    f1 =~ x1 + x2 + x3 + x4
    f2 =~ x5 + x6 + x7 + x8
    f3 =~ x9 + x10 + x11 + x12
    f4 =~ x13 + x14 + x15 + x16
    f3 ~ f1 + f2
    f4 ~ f3
