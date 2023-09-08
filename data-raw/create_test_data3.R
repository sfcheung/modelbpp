# Data based on a serial mediaton model
library(MASS)
set.seed(613534)
n <- 100
gamma <- matrix(c(.2,
                  .1,
                  .05), 3, 1, byrow = TRUE)
beta <- matrix(c(.0, .0, .0,
                 .25, .0, .0,
                 .01, .2, .0), 3, 3, byrow = TRUE)
phi <- matrix(c(1), 1, 1, byrow = TRUE)
psi <- matrix(c(.3,  0,  0,
                 0, .3,  0,
                 0,  0, .2), 3, 3, byrow = TRUE)
x <- mvrnorm(n, c(x1 = 0), phi)
e <- mvrnorm(n, c(e2 = 0, e3 = 0, e4 = 0), psi)
y <- (x %*% t(gamma) + e) %*% t(solve(diag(3) - beta))
dat <- data.frame(x, y)
colnames(dat) <- c("x", "m1", "m2", "y")
mod <-
"
m1 ~ x
m2 ~ m1 + x
y  ~ m2 + x
"
library(lavaan)
fit <- sem(mod, dat, fixed.x = FALSE)
summary(fit)

mod <-
"
m1 ~ x
m2 ~ m1
y  ~ m2
"
fit <- sem(mod, dat, fixed.x = FALSE)
summary(fit)
out <- model_set(fit,
                 must_not_add = c("m1~~y",
                                  "m1~y"))
out

attr(dat, "mod") <- mod
dat_serial_4_weak <- dat
usethis::use_data(dat_serial_4_weak, overwrite = TRUE)

