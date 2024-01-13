if (FALSE) {

# WIP

suppressMessages(library(lavaan))

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

fits <- list(original = fit1,
             fit2 = fit2,
             fit3 = fit3,
             fit4 = fit4)
pts <- lapply(fits, parameterTable)
class(pts) <- c("partables", class(pts))

out1_df2 <- model_set(fit1,
                      partables = pts,
                      progress = FALSE,
                      prior_sem_out = c(original = .001,
                                        fit2 = .0005,
                                        fit4 = .97))
out1_df2
g <- model_graph(out1_df2)
plot(g)
net_out1 <- models_network2(out1_df2, one_df_only = TRUE)
net_out2 <- models_network2(out1_df2, one_df_only = FALSE)
g2 <- igraph::graph_from_adjacency_matrix(net_out2,
                                          mode = "directed",
                                          weight = "width")
g2 <- igraph::add_layout_(g2, igraph::with_sugiyama())
g2 <- layer_by_df(g2,
                  model_set_out = out1_df2)
plot(g2)
g1 <- igraph::graph_from_adjacency_matrix(net_out1,
                                          mode = "directed",
                                          weight = "width")
g1 <- igraph::add_layout_(g1, igraph::with_sugiyama())
g1 <- layer_by_df(g1,
                  model_set_out = out1_df2)
plot(g1)
are_adjacent(g2, 1, 2)
are_adjacent(g2, 1, 3)
are_adjacent(g2, 1, 4)
is.weighted(g1)
is.weighted(g2)
}
