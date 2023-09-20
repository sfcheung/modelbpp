suppressMessages(library(lavaan))

dat <- dat_path_model

mod <-
"
x3 ~ a*x1 + b*x2
x4 ~ a*x1
ab := a*b
"

fit <- sem(mod, dat_path_model, fixed.x = TRUE)
pt <- parameterTable(fit)
pt_no_user <- pt[pt$op != ":=", ]
mod_to_add <- get_add(fit)
fit_add <- lapply(mod_to_add,
                  function(x) update(fit, x))
anova_add <- lapply(fit_add,
                    function(x) anova(x, fit))

expect_true(
    all(names(mod_to_add) %in% c("add: x4~x2", "add: (x3~x1),(x4~x1)")),
    info = "Parameters to drop as expected"
  )

expect_true(
    all(sapply(anova_add,
               function(x) x[2, "Df diff"]) == 1),
    info = "All df differences are one"
  )

expect_true(
    all(sapply(mod_to_add,
               modelbpp:::get_diff, pt_no_user) == names(mod_to_add)),
    info = "Generated difference matches the names"
  )

mod_to_add <- modelbpp:::get_add(fit,
                                 df_change = 2)
fit_add <- lapply(mod_to_add,
                  function(x) update(fit, x))
anova_add <- lapply(fit_add,
                    function(x) anova(fit, x))

expect_true(
    all(names(mod_to_add) %in%
      c("add: x4~x2", "add: (x3~x1),(x4~x1)", "add: x4~x2;(x3~x1),(x4~x1)")),
    info = "Parameters to drop as expected"
  )

expect_true(
    all(sapply(anova_add,
               function(x) x[2, "Df diff"]) ==
        c(1, 1, 2)),
    info = "All df differences are one"
  )

expect_true(
    all(sapply(mod_to_add,
               modelbpp:::get_diff, pt_no_user) == names(mod_to_add)),
    info = "Generated difference matches the names"
  )

# Test Print

expect_stdout(print(mod_to_add),
              "$`add: x4~x2;(x3~x1),(x4~x1)`",
              fixed = TRUE)
