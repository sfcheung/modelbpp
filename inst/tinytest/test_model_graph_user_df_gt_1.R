if (FALSE) {

# WIP

all_simples <- function(i, j, graph) {
    igraph::all_simple_paths(graph = graph,
                             from = i,
                             to = j,
                             mode = "out")
  }
all_simples_list <- function(graph) {
    p <- length(V(graph))
    ij <- combn(p, 2)
    i0 <- ij[1, ]
    j0 <- ij[2, ]
    simples <- mapply(all_simples,
                      i = i0,
                      j = j0,
                      MoreArgs = list(graph = graph),
                      SIMPLIFY = FALSE)
  }
delete_redundant_direct <- function(x, graph) {
    if (length(x) <= 1) {
        return(graph)
      }
    x_length <- sapply(x, length)
    if (all(x_length == 2)) {
        return(graph)
      }
    i <- which(x_length == 2)
    if (length(i) == 0) {
        return(graph)
      }
    out <- graph
    for (xx in i) {
        out <- delete_edges(out,
                            paste(igraph::as_ids(x[[xx]]),
                                  collapse = "|"))
      }
    out
  }

delete_all_redundant_direct <- function(graph) {
    graph_simples <- all_simples_list(graph)
    out <- graph
    for (xx in graph_simples) {
        out <- delete_redundant_direct(xx,
                                       graph = out)
      }
    out
  }



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
plot(g)

net_out1 <- models_network2(out, one_df_only = TRUE)
net_out2 <- models_network2(out, one_df_only = FALSE)

# net_out2[net_out2 > 1] <- 1
g2 <- igraph::graph_from_adjacency_matrix(net_out2,
                                          mode = "directed",
                                          weight = "df")
g2 <- igraph::add_layout_(g2, igraph::with_sugiyama())
g2 <- layer_by_df(g2,
                  model_set_out = out)
plot(g2, edge.label = E(g2)$df)

g21 <- g2
plot(g21)
g21 <- delete_all_redundant_direct(g21)
plot(g21)

g21 <- igraph::add_layout_(g21, igraph::with_sugiyama())
g21 <- layer_by_df(g21,
                   model_set_out = out)
plot(g21,
     edge.width = 4 * 1 / E(g21)$df,
     edge.label = E(g21)$df,
     edge.label.cex = 2)

}
