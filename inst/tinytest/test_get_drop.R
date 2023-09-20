suppressMessages(library(lavaan))

dat <- dat_path_model

mod <-
"
x3 ~ a*x1 + b*x2
x4 ~ a*x1 + x2
ab := a*b
"

fit <- sem(mod, dat_path_model, fixed.x = TRUE)
pt <- parameterTable(fit)
pt_no_user <- pt[pt$op != ":=", ]
mod_to_drop <- get_drop(fit)
fit_drop <- lapply(mod_to_drop, function(x) update(fit, x))
anova_drop <- lapply(fit_drop, function(x) anova(fit, x))

expect_true(
    all(names(mod_to_drop) %in% c("drop: x3~x2", "drop: x4~x2", "drop: x3~~x4")),
    info = "Parameters to drop as expected"
  )

expect_true(
    all(sapply(anova_drop, function(x) x["x", "Df diff"]) == 1),
    info = "All df differences are one"
  )

expect_true(
    all(sapply(mod_to_drop,
                modelbpp:::get_diff_drop, pt_no_user) == names(mod_to_drop)),
    info = "Generated difference matches the names"
  )

mod_to_drop <- get_drop(fit,
                        df_change = 2)
fit_drop <- lapply(mod_to_drop,
                   function(x) update(fit, x))
anova_drop <- lapply(fit_drop,
                     function(x) anova(fit, x))

expect_true(
    all(names(mod_to_drop) %in%
      c("drop: x3~x2", "drop: x4~x2", "drop: x3~~x4",
        "drop: x3~x2;x4~x2", "drop: x3~x2;x3~~x4", "drop: x4~x2;x3~~x4")),
    info = "Parameters to drop as expected"
  )

expect_true(
    all(sapply(anova_drop,
               function(x) x["x", "Df diff"]) ==
        c(1, 1, 1, 2, 2, 2)),
    info = "All df differences are one"
  )

expect_true(
    all(sapply(mod_to_drop,
               modelbpp:::get_diff_drop, pt_no_user) == names(mod_to_drop)),
    info = "Generated difference matches the names"
  )

# Test Print

expect_stdout(print(mod_to_drop),
              "`drop: x4~x2;x3~~x4`",
              fixed = TRUE)
