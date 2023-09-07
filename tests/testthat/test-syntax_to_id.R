library(lavaan)

dat <- dat_path_model

mod <-
"
x3 ~ a*x1 + b*x2
x4 ~ a*x1
ab := a*b
"

fit <- sem(mod, dat_path_model, meanstructure = TRUE, fixed.x = TRUE)

ptable <- parameterTable(fit)

out1 <- syntax_to_id(list("x3 ~ x1", "x3 ~~ x4", "x3 ~1",
                          "m1 ~~ m2"),
                     ptable = ptable)

out2 <- syntax_to_id(list("m3 ~ m71",
                          "m1 ~~ m2"),
                     ptable = ptable)

test_that("syntax_to_id", {
    expect_equal(sort(out1), c(1, 6, 10))
    expect_true(length(out2) == 0)
  })