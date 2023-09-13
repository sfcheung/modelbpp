library(lavaan)

dat <- dat_path_model

mod <-
"
x3 ~ a*x1 + b*x2
x4 ~ a*x1
ab := a*b
"

fit <- sem(mod, dat_path_model, fixed.x = TRUE)

out <- model_set(fit)

test_that("Posterior probability as expected", {
    expect_equal(
        out$bpp,
        c(9.999998e-01, 2.382290e-08, 8.302384e-13, 1.289951e-09, 1.712318e-07),
        tolerance = 1e-5,
        ignore_attr = TRUE
      )
  })

test_that("BIC as expected", {
    expect_equal(
        out$bic,
        c(400.2915, 435.3967, 455.9256, 441.2288, 431.4519),
        tolerance = 1e-5,
        ignore_attr = TRUE
      )
  })

out <- model_set(fit, df_change_add = 2, df_change_drop = 2)

test_that("Posterior probability as expected", {
    expect_equal(
        out$bpp,
        c(8.567850e-01, 2.041111e-08, 1.432148e-01, 7.113360e-13, 1.105211e-09,
          2.891960e-21, 1.467088e-07),
        tolerance = 1e-5,
        ignore_attr = TRUE
      )
  })

test_that("BIC as expected", {
    expect_equal(
        out$bic,
        c(400.2915, 435.3967, 403.8691, 455.9256, 441.2288,
          494.5670, 431.4519),
        tolerance = 1e-5,
        ignore_attr = TRUE
      )
  })

# Test print

out

# User Prior

mod <-
"
x2 ~ x3 + 0*x4
x1 ~ x3
"

fit <- sem(mod, dat_path_model, fixed.x = TRUE)

prior0 <- .50
out <- model_set(fit,
                 prior_sem_out = prior0)
bic <- out$bic
i <- which(names(bic) == "original")
p <- length(bic)
prior <- rep((1 - prior0) / (p - 1), p)
prior[i] <- prior0
d <- exp(-.5 * (bic - bic[1])) * prior
chk_bpp <- d / sum(d)

test_that("Posterior probability as expected: User prior", {
    expect_equal(
        out$bpp,
        chk_bpp,
        ignore_attr = TRUE
      )
  })

# Target BPP

out <- model_set(fit)

postprob0 <- .75

e <- exp(-.5 * (out$bic - out$bic[1]))
e1 <- e[5]
estar <- sum(e[-5])
k <- length(e)
p1 <- (1 / (k - 1)) * postprob0 * estar / (e1 * (1 - postprob0) + postprob0 * estar / (k - 1))

p1_out <- min_prior(out$bic, bpp_target = .75)

out <- model_set(fit,
                 prior_sem_out = unname(p1))

test_that("Target BPP", {
    expect_equal(p1_out,
                 unname(p1))
    expect_equal(unname(out$bpp["original"]),
                 postprob0)
  })


# Test previous output

out0 <- model_set(fit)
out1 <- model_set(model_set_out = out0,
                  prior = .50)
out2 <- model_set(fit,
                  prior_sem_out = .50)

test_that("User supplied model_set", {
    expect_identical(out1$bpp,
                     out2$bpp)
  })

# Test user partables

out0 <- model_set(fit)
tmp <- out0$models[-5]
class(tmp) <- "partables"
out1 <- model_set(fit,
                  partables = tmp)

test_that("User supplied model_set", {
    expect_identical(out0$bic,
                     out1$bic)
    expect_identical(out0$bpp,
                     out1$bpp)
  })

# Test no fit


mod <-
"
x2 ~ x3 + 0*x4
x1 ~ x3
"

fit <- sem(mod, dat_path_model, fixed.x = TRUE)
out <- model_set(fit, fit_models = FALSE)
out

# Update bpp

out <- model_set(fit)

out2 <- model_set(fit,
                  prior_sem_out = .40)

out3 <- model_set(model_set_out = out,
                  fit_models = FALSE,
                  prior_sem_out = .40)

test_that("BPP update", {
    expect_equal(out2$bpp,
                 out3$bpp)
  })

# Gen models

out <- model_set(fit)
out2 <- gen_models(fit)
out3 <- model_set(model_set_out = out2)
out4 <- gen_models(fit, output = "partables")
out5 <- model_set(sem_out = fit,
                  partables = out4)

test_that("Test generated models", {
    expect_identical(out$models,
                     out2$models)
    expect_identical(out$bpp,
                     out3$bpp)
    expect_identical(out$bpp,
                     out5$bpp)
  })
