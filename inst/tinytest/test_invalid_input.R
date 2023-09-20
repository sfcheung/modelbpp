library(lavaan)

dat <- dat_path_model

mod <-
"
x3 ~ a*x1 + b*x2
x4 ~ a*x1
ab := a*b
"

fit <- sem(mod, dat_path_model, fixed.x = TRUE)

expect_error(fit_many(progress = FALSE))
expect_error(fit_many(mod, progress = FALSE))
expect_error(get_add())
expect_error(get_add(mod))
expect_error(get_drop())
expect_error(get_drop(mod))
expect_error(model_set(progress = FALSE))
expect_error(model_set(mod, progress = FALSE))
