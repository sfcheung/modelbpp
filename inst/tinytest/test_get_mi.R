if (TRUE &&
    interactive() &&
    length(unclass(packageVersion("modelbpp"))[[1]]) == 4) {

suppressMessages(library(lavaan))

dat <- dat_sem
set.seed(1234)
dat_sem$group <- sample(c("gp1", "gp2", "gp3"), size = nrow(dat_sem), replace = TRUE)

mod <-
"
f1 =~ x1 + x2 + x3 + x4
f2 =~ x5 + x6 + x7 + x8
"
fit_config <- cfa(mod, dat_sem, group = "group")
fit_metric <- cfa(mod, dat_sem, group = "group",
                  group.equal = "loadings")
fit_scalar <- cfa(mod, dat_sem, group = "group",
                  group.equal = c("intercepts", "loadings"))

# fit_partial_metric <- partial_invariance(fit_metric, max_free = 2, pars = "loadings")
# fit_partial_scalar <- partial_invariance(fit_scalar, max_free = 2, pars = "intercepts")
# names(fit_partial_metric)
# names(fit_partial_scalar)
fit_mi <- measurement_invariance_models(fit_config,
                                        progress = FALSE)
expect_equal(length(fit_mi), 17)
expect_true(all(c("config", "metric", "scalar") %in% names(fit_mi)))

fit_mi2 <- measurement_invariance_models(fit_config,
                                         max_free = 0,
                                         progress = FALSE)
expect_equal(length(fit_mi2), 3)
expect_true(setequal(c("config", "metric", "scalar"), names(fit_mi2)))

fit_mi3 <- measurement_invariance_models(fit_config,
                                         scalar = FALSE,
                                         max_free = 2,
                                         progress = FALSE)
expect_equal(length(fit_mi3), 23)
expect_false("scalar" %in% names(fit_mi3))

fit_mi4 <- measurement_invariance_models(fit_config,
                                         metric = FALSE,
                                         max_free = 1,
                                         progress = FALSE)
expect_equal(length(fit_mi4), 10)
expect_false("metric" %in% names(fit_mi4))

out <- model_set(sem_out = fit_mi4,
                 skip_check_sem_out = TRUE,
                 progress = FALSE,
                 parallel = FALSE)
out
}

