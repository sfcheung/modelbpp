if (FALSE &&
    interactive() &&
    length(unclass(packageVersion("modelbpp"))[[1]]) == 4) {

# Work-In-Progress

suppressMessages(library(lavaan))

mod1 <-
"
m1 ~ x
m2 ~ m1
y ~ m2 + x
"
fit1 <- sem(mod1, dat_serial_4_weak)

mod2 <-
"
x ~ m1 + m2
y ~ x
"
fit2 <- sem(mod1, dat_serial_4_weak)

mod1_df2 <- gen_models(fit1,
                       df_change_add = 2,
                       df_change_drop = 2)
mod1_df2 <- c(mod1_df2, fit2)
out1_df2 <- model_set(fit1,
                      partables = mod1_df2,
                      progress = FALSE)

g <- model_graph(out1_df2)
models_network2(out1_df2)
tmp <- out1_df2$fit
x_net_y(tmp[[18]], tmp[[1]])
x_net_y(tmp[[1]], tmp[[18]])
tmp[[18]]
tmp[[1]]
names(tmp)[18]
names(tmp)[1]

mod18 <-
"
m1 ~ x
m2 ~ m1
y ~ m2 + x
m1 ~~ y
"
fit_tmp1 <- sem(mod18, dat_serial_4_weak, fixed.x = FALSE)
fit_tmp2 <- sem(mod1, dat_serial_4_weak, fixed.x = FALSE)
semTools::net(fit_tmp1, fit_tmp2)
tmp[[1]]
fit_tmp1
tmp[[18]]
fit_tmp2
x_net_y(tmp[[18]], tmp[[1]])
x_net_y(tmp[[1]], tmp[[18]])
x_net_y(fit_tmp1, fit_tmp2)
x_net_y(fit_tmp2, fit_tmp1)

plot(g)
V(g)$name
model_dfs <- sapply(out1_df2$fit, fitMeasures, fit.measures = "df")
tmp <- sort(unique(model_dfs), decreasing = TRUE)
model_layer <- model_dfs
for (i in seq_along(tmp)) {
    model_layer[which(model_dfs == tmp[i])] <- i
  }
model_layer <- model_layer - 1
cbind(model_dfs, model_layer)
g2 <- igraph::add_layout_(g, igraph::with_sugiyama(layers = model_layer))
plot(g2)

}