library(lavaan)

data(dat_path_model)
set.seed(8970431)
dat_path_model$group <- sample(c("Gp1", "Gp2"), nrow(dat_path_model), replace = TRUE)
dat_path_model2 <- dat_path_model
dat_path_model2[1, 2] <- NA

mod <-
"
x3 ~ x1 + x2
x4 ~ x1
"

fit1 <- sem(mod, dat_path_model, fixed.x = TRUE)
fit2 <- sem(mod, dat_path_model, group = "group")
fit3 <- sem(mod, dat_path_model, estimator = "GLS")
fit4 <- sem(mod, dat_path_model, missing = "fiml")

test_that("check_sem_out", {
    expect_true(check_sem_out(fit1))
    expect_error(check_sem_out(fit2), "groups")
    expect_error(check_sem_out(fit3), "estimator")
    expect_error(model_set(fit2), "groups")
    expect_error(model_set(fit3), "estimator")
    expect_true(check_sem_out(fit4))
  })

