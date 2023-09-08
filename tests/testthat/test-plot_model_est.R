library(lavaan)
library(igraph)

mod <-
"
m1 ~ x
m2 ~ m1
y ~ m2
"

fit <- sem(mod, dat_serial_4_weak, fixed.x = TRUE)
summary(fit)
fitMeasures(fit, output = "text")

out1 <- model_set(fit,
                  df_change_add = 1,
                  df_change_drop = 1,
                  must_not_add = c("m1~~y",
                                   "m1~y"))
out1

out3 <- model_set(fit,
                  prior_sem_out = .30,
                  df_change_add = 1,
                  df_change_drop = 1,
                  must_not_add = c("m1~~y",
                                   "m1~y"))
out3

g1 <- model_graph(out1)
plot(g1)

g3 <- model_graph(out3)
plot(g3)

skip("To be tested in an interactive session")

out2 <- model_set(fit,
                  df_change_add = 2,
                  df_change_drop = 2,
                  must_not_add = c("m1~~y",
                                   "m1~y"))
out2

g2 <- model_graph(out2)
plot(g2)
