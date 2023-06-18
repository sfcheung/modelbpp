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
        out$postprob,
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
        out$postprob,
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
