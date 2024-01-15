suppressMessages(library(lavaan))

# Basic Tests

dat <- dat_path_model

mod <-
"
x3 ~ a*x1 + b*x2
x4 ~ a*x1
ab := a*b
"

fit <- sem(mod, dat_path_model, fixed.x = TRUE)

out <- model_set(fit,
                 progress = FALSE,
                 parallel = FALSE)

expect_equivalent(
    out$bpp,
    c(9.999998e-01, 2.382290e-08, 8.302384e-13, 1.289951e-09, 1.712318e-07),
    tolerance = 1e-5,
    info = "Posterior probability as expected"
  )

expect_equivalent(
    out$bic,
    c(400.2915, 435.3967, 455.9256, 441.2288, 431.4519),
    tolerance = 1e-5,
    info = "BIC as expected"
  )

out <- model_set(fit,
                 df_change_add = 2,
                 df_change_drop = 2,
                 progress = FALSE,
                 parallel = FALSE)

expect_equivalent(
    out$bpp,
    c(8.567850e-01, 2.041111e-08, 1.432148e-01, 7.113360e-13, 1.105211e-09,
      2.891960e-21, 1.467088e-07),
    tolerance = 1e-5,
    info = "Posterior probability as expected"
  )

expect_equivalent(
    out$bic,
    c(400.2915, 435.3967, 403.8691, 455.9256, 441.2288,
      494.5670, 431.4519),
    tolerance = 1e-5,
    info = "BIC as expected"
  )

expect_stdout(print(out),
              "The models (sorted by BPP)",
              fixed = TRUE)

# Test previous output

out0 <- model_set(fit,
                  progress = FALSE,
                  parallel = FALSE)
out1 <- model_set(model_set_out = out0,
                  prior = .50,
                  progress = FALSE,
                  parallel = FALSE)
out2 <- model_set(fit,
                  prior_sem_out = .50,
                  progress = FALSE,
                  parallel = FALSE)

expect_identical(out1$bpp,
                 out2$bpp,
                 info = "User supplied model_set")

# Test user partables

out0 <- model_set(fit,
                  progress = FALSE,
                  parallel = FALSE)
tmp <- out0$models[-5]
class(tmp) <- "partables"
out1 <- model_set(fit,
                  partables = tmp,
                  progress = FALSE,
                  parallel = FALSE)

expect_identical(out0$bic,
                  out1$bic,
                  info = "User supplied partables")
expect_identical(out0$bpp,
                  out1$bpp,
                  info = "User supplied partables")

# Test no fit

mod <-
"
x2 ~ x3 + 0*x4
x1 ~ x3
"

fit <- sem(mod,
           dat_path_model,
           fixed.x = TRUE)
out <- model_set(fit,
                 fit_models = FALSE,
                 progress = FALSE,
                 parallel = FALSE)
expect_stdout(print(out),
              "not fitted",
              fixed = TRUE,
              info = "fit_models = FALSE")

# Gen models

out <- model_set(fit,
                 progress = FALSE,
                 parallel = FALSE)
out2 <- gen_models(fit,
                   output = "model_set",
                   progress = FALSE)
out3 <- model_set(model_set_out = out2,
                  parallel = FALSE,
                  progress = FALSE)
out4 <- gen_models(fit,
                   progress = FALSE)
out5 <- model_set(sem_out = fit,
                  partables = out4,
                  progress = FALSE,
                  parallel = FALSE)

expect_identical(out$models,
                  out2$models,
                  info = "Test generated models")
expect_identical(out$bpp,
                  out3$bpp,
                  info = "Test generated models")
expect_identical(out$bpp,
                  out5$bpp,
                  info = "Test generated models")
