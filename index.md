# modelbpp: Model BIC Posterior Probability

(Version 0.2.0.4 updated on 2026-03-31, [release
history](https://sfcheung.github.io/modelbpp/news/index.html))

This package is for assessing model uncertainty in structural equation
modeling (SEM) by the BIC posterior probabilities of the fitted model
and its neighboring models, based on the method presented in Wu, Cheung,
and Leung (2020). The package name, `modelbpp`, stands for `model`
`b`ayesian `p`osterior `p`robability. An introduction to the package can
be found in the following article:

- Pesigan, I. J. A., Cheung, S. F., Wu, H., Chang, F., & Leung, S. O.
  (2026). How plausible is my model? Assessing model plausibility of
  structural equation models using Bayesian posterior probabilities
  (BPP). *Behavior Research Methods, 58*(3), Article 73.
  <https://doi.org/10.3758/s13428-025-02921-x>

# Homepage

For more information on this package, please visit its GitHub page:

<https://sfcheung.github.io/modelbpp/>

A quick introduction on how to use this package can be found in the
Get-Started article
([`vignette("modelbpp")`](https://sfcheung.github.io/modelbpp/articles/modelbpp.md)).

# Installation

The stable CRAN version can be installed by
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html):

``` r
install.packages("modelbpp")
```

The latest developmental-but-stable version of this package can be
installed by `remotes::install_github`:

``` r
remotes::install_github("sfcheung/modelbpp")
```

# Issues

If you have any suggestions or found any bugs, please feel free to open
a GitHub issue. Thanks.

<https://github.com/sfcheung/modelbpp/issues>

# Reference(s)

Wu, H., Cheung, S. F., & Leung, S. O. (2020). Simple use of BIC to
assess model selection uncertainty: An illustration using mediation and
moderation models. *Multivariate Behavioral Research*, *55*(1), 1–16.
<https://doi.org/10.1080/00273171.2019.1574546>
