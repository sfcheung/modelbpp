if (TRUE &&
    interactive() &&
    length(unclass(packageVersion("modelbpp"))[[1]]) == 4) {

# Long tests

suppressMessages(library(lavaan))

data(dat_path_model)

mod1 <-
"
x3 ~ x1 + x2
x4 ~ x1
"
fit1_fixed <- sem(mod1, dat_path_model, fixed.x = TRUE)
fit1_nofix <- sem(mod1, dat_path_model, fixed.x = FALSE)
fit1_mean <- sem(mod1, dat_path_model, fixed.x = FALSE, meanstructure = TRUE)

mod2 <-
"
x3 ~ x2 + 0*x1
x4 ~ x1
"
fit2_fixed <- sem(mod2, dat_path_model, fixed.x = TRUE)
fit2_nofix <- sem(mod2, dat_path_model, fixed.x = FALSE)
fit2_mean <- sem(mod2, dat_path_model, fixed.x = FALSE, meanstructure = TRUE)

mod3 <-
"
x3 ~ x1 + x2
x4 ~ x1 + x2
"
fit3_fixed <- sem(mod3, dat_path_model, fixed.x = TRUE)
fit3_nofix <- sem(mod3, dat_path_model, fixed.x = FALSE)
fit3_mean <- sem(mod3, dat_path_model, fixed.x = FALSE, meanstructure = TRUE)

mod4 <-
"
x3 ~ x1
x4 ~ x1
"
fit4_fixed <- sem(mod4, dat_path_model, fixed.x = TRUE)
fit4_nofix <- sem(mod4, dat_path_model, fixed.x = FALSE)
fit4_mean <- sem(mod4, dat_path_model, fixed.x = FALSE, meanstructure = TRUE)

# suppressMessages(library(semTools))
# net(fit1_nofix, fit2_nofix, fit3_nofix)@test
# net(fit1_nofix, fit2_nofix)@test

# x_net_y(fit1_nofix, fit2_nofix)
# x_net_y(fit2_nofix, fit1_nofix)
# x_net_y(fit1_nofix, fit1_nofix)
# x_net_y(fit1_nofix, fit3_nofix)
# x_net_y(fit3_nofix, fit1_nofix)
# x_net_y(fit2_nofix, fit3_nofix)
# x_net_y(fit3_nofix, fit2_nofix)

out <- model_set(fit1_nofix,
                 df_change_add = 1,
                 df_change_drop = 2,
                 progress = FALSE)
expect_identical(models_network(out),
                 models_network2(out))

# User models

suppressMessages(library(igraph))

mod1 <-
"
x2 ~ x1
x3 ~ x2
x4 ~ x3
"
fit1 <- sem(mod1, dat_path_model, fixed.x = FALSE)

mod2 <-
"
x3 ~ x4
x2 ~ x3
x1 ~ x2
"
fit2 <- sem(mod2, dat_path_model, fixed.x = FALSE)

mod3 <-
"
x2 ~ x1
x3 ~ x2 + x1
x4 ~ x3
"
fit3 <- sem(mod3, dat_path_model, fixed.x = FALSE)

mod4 <-
"
x2 ~ x1
x3 ~ x2 + x1
x4 ~ x3 + x2
"
fit4 <- sem(mod4, dat_path_model, fixed.x = FALSE)

mod5 <-
"
x2 ~ x4
x1 ~ x4
x3 ~ x2 + x1
"
fit5 <- sem(mod5, dat_path_model, fixed.x = FALSE)

fits <- list(fit2 = fit2,
             fit3 = fit3,
             fit4 = fit4,
             fit5 = fit5)
ptables <- lapply(fits, parameterTable)
class(ptables) <- c("partables", class(ptables))
out <- model_set(fit1,
                 partables = ptables,
                 progress = FALSE)
g <- model_graph(out)
plot(g)
tmp <- lapply(E(g), attributes)
expect_equal(length(E(g)), 3)
expect_equal(length(V(g)), 5)
}

