library(lavaan)

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

get_diff <- function(x, pt_no_user) {
  out <- x[which(apply(x != pt_no_user, 1, any, na.rm = TRUE)), ]
  out_ops <- sapply(seq_len(nrow(out)), function(x) {
      paste0(out[x, "lhs"], out[x, "op"], out[x, "rhs"], collapse = "")
    })
  out_ops <- paste(out_ops, collapse = ";")
  out_ops <- paste("drop:", out_ops)
}

test_that("Parameters to drop as expected", {
    expect_true(
        all(names(mod_to_drop) %in% c("drop: x3~x2", "drop: x4~x2", "drop: x3~~x4"))
      )
  })

test_that("All df differences are one", {
    expect_true(
        all(sapply(anova_drop, function(x) x["x", "Df diff"]) == 1)
      )
  })

test_that("Generated difference matches the names", {
    expect_true(
        all(sapply(mod_to_drop, get_diff, pt_no_user) == names(mod_to_drop))
      )
  })

mod_to_drop <- get_drop(fit, df_change = 2)
fit_drop <- lapply(mod_to_drop, function(x) update(fit, x))
anova_drop <- lapply(fit_drop, function(x) anova(fit, x))

test_that("Parameters to drop as expected", {
    expect_true(
        all(names(mod_to_drop) %in% 
          c("drop: x3~x2", "drop: x4~x2", "drop: x3~~x4",
            "drop: x3~x2;x4~x2", "drop: x3~x2;x3~~x4", "drop: x4~x2;x3~~x4"))
      )
  })

test_that("All df differences are one", {
    expect_true(
        all(sapply(anova_drop, function(x) x["x", "Df diff"]) == 
           c(1, 1, 1, 2, 2, 2))
      )
  })

test_that("Generated difference matches the names", {
    expect_true(
        all(sapply(mod_to_drop, get_diff, pt_no_user) == names(mod_to_drop))
      )
  })