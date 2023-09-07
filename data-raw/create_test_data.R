# Generate data

# Data based on a path analysis model
library(MASS)
set.seed(8150734)
n <- 100
gamma <- matrix(c(.4, .5,
                  .1, .2), 2, 2, byrow = TRUE)
beta <- matrix(c(.0, .0,
                 .3, .0), 2, 2, byrow = TRUE)
phi <- matrix(c( 1,.1,
                .1, 1), 2, 2, byrow = TRUE)
psi <- matrix(c(.4,  0,
                 0, .4), 2, 2, byrow = TRUE)
x <- mvrnorm(n, c(x1 = 0, x2 = 0), phi)
e <- mvrnorm(n, c(e3 = 0, e4 = 0), psi)
y <- (x %*% t(gamma) + e) %*% t(solve(diag(2) - beta))
dat <- data.frame(x, y)
colnames(dat) <- c("x1", "x2", "x3", "x4")
mod <-
"
x3 ~ x1 + x2
x4 ~ x3
"
library(lavaan)
fit <- sem(mod, dat, fixed.x = FALSE)
summary(fit)

attr(dat, "mod") <- mod
dat_path_model <- dat
usethis::use_data(dat_path_model, overwrite = TRUE)


# Data based on a CFA model
library(MASS)
set.seed(4343646)
n <- 200
lambda <- matrix(c(  1,   0,
                   0.8,   0,
                   1.2,  .3,
                     0,   1,
                    .4, 1.2,
                     0, 0.8), 6, 2, byrow = TRUE)
phi <- matrix(c( 1, .2,
                .2,  1), 2, 2)
psi <- diag(c(.8, .7, .5, .6, .8, .7))
f <- mvrnorm(n, c(f1 = 0, f2 = 0), phi)
e <- mvrnorm(n, rep(0, 6), psi)
x <- f %*% t(lambda) + e
dat <- data.frame(x)
colnames(dat) <- paste0("x", 1:6)
mod <-
"
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
"
library(lavaan)
fit <- cfa(mod, dat)
summary(fit)

attr(dat, "mod") <- mod
dat_cfa <- dat
usethis::use_data(dat_cfa, overwrite = TRUE)


# Data based more complicated path models with equality constraints and derived parameters
library(MASS)
set.seed(5535235)
n <- 200
gamma <- matrix(c(.4, .5, .3,
                  .1, .2, .0,
                  .1, .1, .3), 3, 3, byrow = TRUE)
beta <- matrix(c(.0, .0, .0,
                 .3, .0, .0,
                 .1, .2, .0), 3, 3, byrow = TRUE)
phi <- matrix(c( 1, .1, .1,
                .1,  1, .1,
                .1, .1,  1), 3, 3, byrow = TRUE)
psi <- matrix(c(.4,  0,  0,
                 0, .4,  0,
                 0,  0, .2), 3, 3, byrow = TRUE)
x <- mvrnorm(n, c(x1 = 0, x2 = 0, x3 = 0), phi)
e <- mvrnorm(n, c(e4 = 0, e5 = 0, e6 = 0), psi)
y <- (x %*% t(gamma) + e) %*% t(solve(diag(3) - beta))
dat <- data.frame(x, y)
colnames(dat) <- c("x1", "x2", "x3", "y4", "y5", "y6")
mod <-
"
y4 ~ a*x1 + a*x2
y5 ~ b*y4 + x1
y6 ~ c*y5
abc := a*b*c
ab := a*b
"
library(lavaan)
fit <- sem(mod, dat, fixed.x = FALSE)
summary(fit)

attr(dat, "mod") <- mod
dat_path_model_p06 <- dat
usethis::use_data(dat_path_model_p06, overwrite = TRUE)

# Data based on a serial mediaton model
library(MASS)
set.seed(613534)
n <- 100
gamma <- matrix(c(.8,
                  .0,
                  .0), 3, 1, byrow = TRUE)
beta <- matrix(c(.0, .0, .0,
                 .8, .0, .0,
                 .0, .8, .0), 3, 3, byrow = TRUE)
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

attr(dat, "mod") <- mod
dat_serial_4 <- dat
usethis::use_data(dat_serial_4, overwrite = TRUE)

