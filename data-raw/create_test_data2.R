# Generate data

# Data based on a CFA model
library(lavaan)
mod_pop <-
"
f1 =~ .8 * x1 + .6 * x2 + .8 * x3 + .6 * x4 + .4 * x6
f2 =~ .8 * x5 + .1 * x6 + .8 * x7 + .4 * x8 + .3 * x3
f3 =~ .8 * x9 + .6 * x10 + .1 * x11 + .8 * x12
f4 =~ .7 * x13 + .6 * x14 + .7 * x15 + .6 * x16
f3 ~ .5 * f1 + .4 * f2
f4 ~ .5 * f3 + .25 * f2 + .2 * f1
f1 ~~ .0 * f2
"
set.seed(48714)
dat <- simulateData(model = mod_pop,
                    sample.nobs = 250,
                    standardized = TRUE,
                    return.fit = TRUE)
head(dat)
mod_sem <-
"
f1 =~ x1 + x2 + x3 + x4
f2 =~ x5 + x6 + x7 + x8
f3 =~ x9 + x10 + x11 + x12
f4 =~ x13 + x14 + x15 + x16
f3 ~ f1 + f2
f4 ~ f3
"
fit <- sem(mod_sem, dat)
summary(fit, fit.measures = TRUE)
dat_sem <- dat
usethis::use_data(dat_sem, overwrite = TRUE)
