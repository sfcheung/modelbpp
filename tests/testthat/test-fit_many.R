library(lavaan)

dat <- dat_path_model

mod <-
"
x3 ~ a*x1 + b*x2
x4 ~ a*x1
ab := a*b
"

fit <- sem(mod, dat_path_model, fixed.x = TRUE)
mod_to_add <- get_add(fit)
mod_to_drop <- get_drop(fit)
mod_to_fit <- c(mod_to_add, mod_to_drop)
out <- fit_many(mod_to_fit, fit)

test_that("df changes as expected", {
    expect_equal(
        out$change,
        c(1, 1, -1, -1),
        ignore_attr = TRUE
      )
  })

# Test Print

out
print(out, max_models = 2)

skip("Parallel processing: Test in an interactive session")

mod_to_add <- get_add(fit, df_change = 2)
mod_to_drop <- get_drop(fit, df_change = 2)
mod_to_fit <- c(mod_to_add, mod_to_drop)
out <- fit_many(mod_to_fit, fit, parallel = TRUE, ncores = 3)
test_that("df changes as expected", {
    expect_equal(
        out$change,
        c(1, 1, 2, -1, -1, -2),
        ignore_attr = TRUE
      )
  })

