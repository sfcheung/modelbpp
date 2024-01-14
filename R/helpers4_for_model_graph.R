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
    p <- length(igraph::V(graph))
    tmp <- utils::combn(p, 2)
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

#' @title Store Basic Info to A Graph
#'
#' @noRd

doc_model_graph <- function(object,
                            graph) {
    p_V <- length(igraph::V(graph))
    V_names <- names(igraph::V(graph))
    V_order <- match(names(object$models), V_names)
    if (!is.null(object$change)) {
        igraph::V(graph)$change <- object$change[V_order]
      } else {
        igraph::V(graph)$change <- rep(NA, p_V)
      }
    if (!is.null(object$converged)) {
        igraph::V(graph)$converged <- object$converged[V_order]
      } else {
        igraph::V(graph)$converged <- rep(NA, p_V)
      }
    if (!is.null(object$post_check)) {
        igraph::V(graph)$post_check <- object$post_check[V_order]
      } else {
        igraph::V(graph)$post_check <- rep(NA, p_V)
      }
    igraph::V(graph)$model_name <- names(object$models)[V_order]
    if (!is.null(object$bic)) {
        igraph::V(graph)$bic <- object$bic[V_order]
      } else {
        igraph::V(graph)$bic <- rep(NA, p_V)
      }
    if (!is.null(object$prior)) {
        igraph::V(graph)$prior <- object$prior[V_order]
      } else {
        igraph::V(graph)$prior <- rep(NA, p_V)
      }
    if (!is.null(object$bpp)) {
        igraph::V(graph)$bpp <- object$bpp[V_order]
      } else {
        igraph::V(graph)$bpp <- rep(NA, p_V)
      }
    graph
  }

#' @title Label Arrows By 'df' Differences
#'
#' @noRd

label_by_df <- function(graph,
                              mode) {
    mode0 <- mode
    graph_df <- igraph::E(graph)$df
    if (is.null(mode)) {
        any_gt_1 <- (length(setdiff(graph_df, c(0, 1))) > 0)
        if (any_gt_1) {
            mode0 <- TRUE
          } else {
            mode0 <- FALSE
          }
      }
    if (mode0) {
        igraph::E(graph)$label <- graph_df
      } else {
        # Placehodler
      }
    graph
  }

#' @title Set Arrows Width By 'df' Differences
#'
#' @noRd

edge_weight <- function(graph,
                        mode = c("inverse", "normal", "none"),
                        min_width = .5,
                        max_width = 2) {
    if (is.null(igraph::E(graph)$df)) {
        igraph::E(graph)$width <- max_width
        return(graph)
      }
    tmp1 <- igraph::E(graph)$df
    tmp2 <- switch(mode,
                   inverse = min(tmp1) / tmp1,
                   normal = tmp1 / max(tmp1),
                   none = rep(max_width, length(tmp1)))
    tmp2 <- normalize_edge_width(tmp2,
                                 min_width = min_width,
                                 max_width = max_width)
    igraph::E(graph)$width <- tmp2
    graph
  }

#' @noRd

normalize_edge_width <- function(x,
                                 min_width = .5,
                                 max_width = 2) {
    x_min <- min(x)
    x_max <- max(x)
    x_range <- x_max - x_min
    x_out <- max_width * (x - x_min)/x_range +
             min_width
    x_out
  }