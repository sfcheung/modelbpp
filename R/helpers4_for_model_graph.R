#' @title Set Layout Based on Model Degrees of Freedom
#'
#' @noRd

layer_by_df <- function(g,
                         model_set_out) {
  model_dfs <- sapply(model_set_out$fit,
                      lavaan::fitMeasures,
                      fit.measures = "df")
  names(model_dfs) <- names(model_set_out$fit)
  tmp <- sort(unique(model_dfs),
              decreasing = TRUE)
  model_layer <- model_dfs
  for (i in seq_along(tmp)) {
      model_layer[which(model_dfs == tmp[i])] <- i
    }
  model_layer <- model_layer - 1
  out <- igraph::add_layout_(g,
                             igraph::with_sugiyama(layers = model_layer))
  out
}

#' @title Identify All Simple Paths Between Two Models
#'
#' @noRd

all_simples <- function(i, j, graph) {
    igraph::all_simple_paths(graph = graph,
                             from = i,
                             to = j,
                             mode = "out")
  }

#' @title Identify All Simple Paths In A Network
#'
#' @return
#' A list of simple paths list, one for each
#' combination of models.
#'
#' @noRd

all_simples_list <- function(graph) {
    p <- length(V(graph))
    tmp <- combn(p, 2)
    ij <- cbind(tmp, tmp[2:1, ])
    i0 <- ij[1, ]
    j0 <- ij[2, ]
    simples <- mapply(all_simples,
                      i = i0,
                      j = j0,
                      MoreArgs = list(graph = graph),
                      SIMPLIFY = FALSE)
    simples
  }

#' @title Delete A Redundant Direct Path
#'
#' @description
#' Check the path between two models. If
#' They can be connected through any
#' other models, then the direct path
#' is removed.
#'
#' @return
#' A `igraph`, with the redundant path,
#' if any, removed.
#'
#' @noRd

delete_redundant_direct <- function(x,
                                    graph) {
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
        out <- igraph::delete_edges(out,
                  paste(igraph::as_ids(x[[xx]]),
                        collapse = "|"))
      }
    out
  }

#' @title Delete All Redundant Direct Paths
#'
#' @description
#' Call [delete_redundant_direct()] to delete
#' the redundant direct path, if any.
#'
#' @return
#' A `igraph`, with redundant paths removed.
#'
#' @noRd

delete_all_redundant_direct <- function(graph) {
    graph_simples <- all_simples_list(graph)
    out <- graph
    for (xx in graph_simples) {
        out <- delete_redundant_direct(xx,
                                       graph = out)
      }
    out
  }
