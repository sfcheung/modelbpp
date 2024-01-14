suppressMessages(library(lavaan))
suppressMessages(library(igraph))

mod1 <-
"
m1 ~ x
m2 ~ m1
y ~ m2
"
fit1 <- sem(mod1, dat_serial_4_weak, fixed.x = FALSE)

mod2 <-
"
m1 ~ x
m2 ~ m1 + x
y ~ m2 + x
"
fit2 <- sem(mod2, dat_serial_4_weak, fixed.x = FALSE)

mod3 <-
"
m1 ~ x
m2 ~ m1 + x
y ~ m2 + x + m1
"
fit3 <- sem(mod3, dat_serial_4_weak, fixed.x = FALSE)

mod4 <-
"
m1 ~ x
m2 ~ m1 + x
y ~ m2 + m1
"
fit4 <- sem(mod4, dat_serial_4_weak, fixed.x = FALSE)

mod5 <-
"
m1 ~ x
m2 ~ m1
y ~ m2 + m1
"
fit5 <- sem(mod5, dat_serial_4_weak, fixed.x = FALSE)


fits <- list(fit1 = fit1,
             fit2 = fit2,
             fit3 = fit3,
             fit4 = fit4,
             fit5 = fit5)

out <- model_set(fits,
                 progress = FALSE,
                 prior_sem_out = c(fit5 = .05,
                                   fit1 = .05,
                                   fit3 = .40,
                                   fit2 = .20,
                                   fit4 = .28))
out
g <- model_graph(out)
if (interactive()) {
plot(g)
}
g2 <- model_graph(out, drop_redundant_direct_paths = FALSE)
if (interactive()) {
plot(g2)
}

expect_false(are_adjacent(g, "fit1", "fit3"))
expect_true(are_adjacent(g2, "fit1", "fit3"))
