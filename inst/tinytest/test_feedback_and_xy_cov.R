suppressMessages(library(lavaan))

dat <- dat_path_model

mod <-
"
x1 ~ x2
x3 ~ x1
x4 ~ x3 + x2
"

fit <- sem(mod,
           dat_path_model,
           meanstructure = TRUE,
           fixed.x = TRUE)

mod2 <-
"
f1 =~ x4 + x2 + x3
"

fit2 <- cfa(mod2,
            dat_path_model,
            meanstructure = TRUE,
            fixed.x = TRUE)

out <- modelbpp:::feedback_and_xy_cov(fit)
out2 <- modelbpp:::feedback_and_xy_cov(fit2)

feedback_check <- structure(list(lhs = c("x1", "x2", "x2"), op = c("~", "~", "~"
), rhs = c("x4", "x3", "x4")), row.names = c(NA, 3L), class = "data.frame")

xy_cov_check <- structure(list(lhs = c("x1", "x2", "x2"), op = c("~~", "~~",
"~~"), rhs = c("x4", "x3", "x4")), row.names = c(NA, 3L), class = "data.frame")

expect_true(setequal(out$all_feedback,
                     feedback_check),
            info = "modelbpp:::feedback_and_xy_cov")
expect_true(setequal(out$all_xy_cov,
                     xy_cov_check),
            info = "modelbpp:::feedback_and_xy_cov")

lor_feedback_check <- list(c(lhs = "x1", op = "~", rhs = "x4"), c(lhs = "x2", op = "~",
rhs = "x3"), c(lhs = "x2", op = "~", rhs = "x4"))

lor_xy_cov_check <- list(c(lhs = "x1", op = "~~", rhs = "x4"), c(lhs = "x2", op = "~~",
rhs = "x3"), c(lhs = "x2", op = "~~", rhs = "x4"))

expect_true(setequal(modelbpp:::df_to_lor(out$all_feedback),
                     lor_feedback_check),
            info = "modelbpp:::feedback_and_xy_cov")
expect_true(setequal(modelbpp:::df_to_lor(out$all_xy_cov),
                     lor_xy_cov_check),
            info = "modelbpp:::feedback_and_xy_cov")
