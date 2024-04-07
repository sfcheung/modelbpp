if (TRUE &&
    interactive() &&
    length(unclass(packageVersion("modelbpp"))[[1]]) == 4) {

# Long tests

suppressMessages(library(lavaan))

data(dat_path_model)

mod1 <-
"
x2 ~ x1
x3 ~ x2
"
fit1_nofix <- sem(mod1, dat_path_model, fixed.x = FALSE)

mod2 <-
"
x2 ~ x3
x1 ~ x2
"
fit2_nofix <- sem(mod2, dat_path_model, fixed.x = FALSE)

mod3 <-
"
x2 ~~ x3
x1 ~ x2
"
fit3_nofix <- sem(mod3, dat_path_model, fixed.x = FALSE)

mod4 <-
"
x2 ~ x3
x2 ~ x1
x1 ~~ 0*x3
"
fit4_nofix <- sem(mod4, dat_path_model, fixed.x = FALSE)

mod5 <-
"
x2 ~ x3
x2 ~~ x1
x1 ~~ 0*x3
"
fit5_nofix <- sem(mod5, dat_path_model, fixed.x = FALSE)

mod6 <-
"
x3 ~ x1
x2 ~ x3
"
fit6_nofix <- sem(mod6, dat_path_model, fixed.x = FALSE)

out <- model_set(list(a = fit1_nofix,

                      b = fit2_nofix,
                      c = fit3_nofix,
                      d = fit4_nofix,
                      e = fit5_nofix,
                      f = fit6_nofix),
                 progress = FALSE)
expect_true(setequal(names(out$equivalent_clusters),
                     c("d", "a")))
expect_true(setequal(out$equivalent_clusters$d,
                     c("d", "e")))
expect_true(setequal(out$equivalent_clusters$a,
                     c("a", "b", "c")))
}


