suppressMessages(library(lavaan))
suppressMessages(library(igraph))

dat <- dat_path_model

mod1 <-
"
x3 ~ x1 + x2
x4 ~ x3
"
fit1 <- sem(mod1, dat, fixed.x = FALSE)

mod2 <-
"
x2 ~ x1
x3 ~ x2
x4 ~ x3
"
fit2 <- sem(mod2, dat, fixed.x = FALSE)

mod3 <-
"
x2 ~ x4
x3 ~ x4
x1 ~ x2 + x3
"
fit3 <- sem(mod3, dat, fixed.x = FALSE)

mod4 <-
"
x2 ~ x4
x3 ~ x4
x1 ~ x2 + x3 + x4
"
fit4 <- sem(mod4, dat, fixed.x = FALSE)

out <- model_set(sem_out = list(fit1 = fit1,
                                fit3 = fit3,
                                fit2 = fit2,
                                fit4 = fit4),
                 prior_sem_out = c(fit1 = .00005,
                                   fit2 = .60,
                                   fit4 = .20),
                 progress = FALSE,
                 parallel = FALSE)

fit_names <- c("fit1", "fit3", "fit2", "fit4")

expect_true(all(is.na(names(out$change))))
expect_equal(names(out$fit), fit_names)
expect_equal(names(out$models), fit_names)
g <- model_graph(out)
expect_equal(V(g)$name, fit_names)
