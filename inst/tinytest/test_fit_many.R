suppressMessages(library(lavaan))

dat <- dat_path_model

mod <-
"
x3 ~ a*x1 + b*x2
x4 ~ a*x1
ab := a*b
"

fit <- sem(mod,
           dat_path_model,
           fixed.x = TRUE)
mod_to_add <- get_add(fit)
mod_to_drop <- get_drop(fit)
mod_to_fit <- c(mod_to_add, mod_to_drop)
out <- fit_many(mod_to_fit,
                fit,
                progress = FALSE)

expect_equivalent(
    out$change,
    c(1, 1, -1, -1)
  )

# Test Print

expect_stdout(print(out),
              "drop: x3~~x4",
              fixed = TRUE)
expect_stdout(print(out,
                    max_models = 2),
              "first 2",
              fixed = TRUE)

if (interactive()) {
    mod_to_add <- get_add(fit,
                          df_change = 2)
    mod_to_drop <- get_drop(fit,
                            df_change = 2)
    mod_to_fit <- c(mod_to_add, mod_to_drop)
    out <- fit_many(mod_to_fit,
                    fit,
                    parallel = TRUE,
                    ncores = 2,
                    progress = FALSE,
                    verbose = FALSE)
    expect_equivalent(
        out$change,
        c(1, 1, 2, -1, -1, -2),
        ignore_attr = TRUE
      )
  }
