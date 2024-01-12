suppressMessages(library(lavaan))

data(dat_path_model)
set.seed(8970431)
dat_path_model$group <- sample(c("Gp1", "Gp2"),
                               nrow(dat_path_model),
                               replace = TRUE)
dat_path_model2 <- dat_path_model
dat_path_model2[1, 2] <- NA

mod <-
"
x3 ~ x1 + x2
x4 ~ x1
"

fit1 <- sem(mod, dat_path_model)
pt1 <- parameterTable(fit1)
pt0 <- parameterTable(sem(mod))
pt2 <- lavaanify(mod)
pt3 <- pt1[c("id", "lhs", "op", "rhs",
             "free", "ustart")]
fit3 <- sem(pt3, dat_path_model)
coef(fit3)
coef(fit1)

expect_error(identical_partables(pt1, as.list(pt3)))
expect_error(identical_partables(pt3[1:3], pt1))

modb <-
"
x3 ~ x2 + x1
x4 ~ x1
"
fitb1 <- sem(modb, dat_path_model)

ptb1 <- parameterTable(fitb1)
ptb1
pt1

expect_true(identical_partables(pt1, ptb1))
expect_true(identical_partables(pt1, pt0))
expect_false(identical_partables(pt1, pt2))

mod <-
"
x3 ~ a*x1 + b*x2
x4 ~ a*x1
ab := a*b
"

fit <- sem(mod, dat_path_model, fixed.x = TRUE)
out <- model_set(fit,
                 fit_models = FALSE)
pts <- out$models

modb <-
"
x3 ~ x1
x2 ~ a*x1
"

fitb <- sem(modb, dat_path_model, fixed.x = TRUE)
ptsb <- c(pts, parameterTable(fitb))

expect_true(same_variables(pts))
expect_false(same_variables(ptsb))
