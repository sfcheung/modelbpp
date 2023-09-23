time0 <- proc.time()

suppressMessages(library(lavaan))

dat <- dat_serial_4_weak

mod0 <-
"
m1 ~ x
m2 ~ m1 + x
y ~ m1 + m2 + x
"
fit0 <- sem(mod0, dat_serial_4_weak)

out0 <- model_set(fit0,
                  progress = FALSE,
                  parallel = FALSE)

mod1 <-
"
m1 ~ x
m2 ~ m1
"
fit1 <- sem(mod1, dat_serial_4_weak)

out1 <- model_set(fit1,
                  must_not_drop = c("m2 ~ m1", "m1 ~ x"),
                  progress = FALSE,
                  parallel = FALSE)

out2 <- model_set(fit1,
                  must_not_drop = c("m2 ~ m1", "m1 ~ x"),
                  must_not_add = c("m2 ~ x"),
                  progress = FALSE,
                  parallel = FALSE)

expect_false(
    any(grepl("add", names(out0$fit))),
    info = "No add"
  )
expect_false(
    any(grepl("drop", names(out1$fit))),
    info = "No drop"
  )
expect_true(
    identical(names(out2$fit), "original"),
    info = "original only"
  )

time1 <- proc.time()
print(as.vector((time1 - time0)))
