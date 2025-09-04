suppressMessages(library(lavaan))

mod1 <-
"
x2 ~ x1 + x4
x3 ~ x2
"

mod2 <-
"
x3 ~ x1
x3 ~ x2
x4 ~ x3
"

fit1 <- sem(mod1, HolzingerSwineford1939, fixed.x = FALSE)
fit2 <- sem(mod2, HolzingerSwineford1939, fixed.x = FALSE)

out1 <- model_set(fit1,
                  progress = FALSE,
                  parallel = FALSE)
out2 <- model_set(fit2,
                  progress = FALSE,
                  parallel = FALSE)

expect_error(
    model_set_combined(
            list(out1,
                 out2),
            progress = FALSE,
            parallel = FALSE)
  )

expect_error(
    model_set_combined(
            list(a = out1,
                 out2),
            progress = FALSE,
            parallel = FALSE)
  )
