#' @title Generate a Graph of Models
#'
#' @description Generate an 'igraph' object from a
#' 'model_set' object.
#'
#' @details
#' It extracts the model list stored
#' in `object`, creates an adjacency
#' matrix, and then creates an `igraph`
#' object customized for visualizing
#' model relations.
#'
#' ## Construction of the Graph
#'
#' - Each model is connected by an
#'  arrow, pointing from one model to
#'  another model that is formed by
#'
#'    a. adding one free parameter, or
#'
#'    b, releasing one equality constraint
#'  between two parameters.
#'
#'  That is, it points to a model with
#'  one *less* degree of freedom.
#'
#' - By default, the size of the node
#'  for each model is scaled by its
#'  BIC posterior probability, if
#'  available. See *The Size of a Node*
#'  below.
#'
#' - The original model, the models with
#'  more degrees of freedom,
#'  and the models with fewer degrees
#'  of freedom are colored differently.
#'
#' - The default layout is the Sugiyama
#'  layout, with simpler models (models
#'  with fewer degrees of freedom) on
#'  the top. The lower a model is in
#'  the network, the more the degrees
#'  of freedom it has. This layout is
#'  suitable for showing the nested
#'  relations of the models.
#'
#' The output is an `igraph` object.
#' Users can customize it in any way
#' they want using functions from
#' the `igraph` package.
#'
#' ## The Size of a Node
#'
#' When a model is scaled by `x`,
#' which usually is the BIC posterior
#' probability, its size is determined
#' by:
#'
#' `max_size * (x - min(x))/(max(x) - min(x)) + min_size`
#'
#' @return
#' A `model_graph`-class object that
#' can be used as as an `igraph`-object,
#' with a plot method ([plot.model_graph()])
#' with settings
#' suitable for plotting a network
#' of models with BIC posterior probabilities
#' computed.
#'
#' @param object Must be a
#' `model_set`-class object for now.
#'
#' @param node_size_by_x Logical. Whether
#' node (vertex) sizes are determined
#' by a variable. Default is `TRUE`.
#' See `x` below on how size is
#' determined.
#'
#' @param x If not `NULL`, it should be
#' a numeric vector of length equal to
#' the number of models. The node sizes
#' will be proportional to the values
#' of `x`, offset by `min_size`. If
#' `NULL`, the default, the BIC
#' posterior probabilities
#' stored in `object` will be retrieved.
#'
#' @param node_size If `node_size_by_x`
#' is `FALSE`, this is the size for
#' all nodes.
#'
#' @param min_size The minimum size
#' of a node. Default is 5.
#'
#' @param max_size The maximum size
#' of a node. Default is 35.
#'
#' @param color_original The color
#' of node of the original model.
#' Default is `"lightblue"`.
#'
#' @param color_add The color of
#' the nodes of models formed by
#' adding one or more free parameters to
#' the original model.
#' Default is `"burlywood1"`.
#'
#' @param color_drop The color of
#' the nodes of models formed by
#' dropping one or more free parameters
#' from the original model.
#' Default is `"lightgreen"`.
#'
#' @param color_others The color
#' of other models not specified above.
#' Default is `"grey50"`.
#'
#' @param color_label The color of the
#' text labels of the nodes. Default
#' is `"black"`.
#'
#' @param node_label_size The size of
#' the labels of the nodes. Default is
#' 1.
#'
#' @param ... Optional arguments. Not
#' used for now.
#'
#' @examples
#'
#' library(lavaan)
#'
#' mod <-
#' "
#' m1 ~ x
#' y ~ m1
#' "
#'
#' fit <- sem(mod, dat_serial_4, fixed.x = TRUE)
#'
#' out <- model_set(fit)
#' out
#'
#' g <- model_graph(out)
#' plot(g)
#'
#' @export

model_graph <- function(object,
                        node_size_by_x = TRUE,
                        x = NULL,
                        node_size = 5,
                        min_size = 5,
                        max_size = 35,
                        color_original = "lightblue",
                        color_add = "burlywood1",
                        color_drop = "lightgreen",
                        color_others = "lightgrey",
                        color_label = "black",
                        node_label_size = 1,
                        ...) {
    user_models <- sapply(added(object$models), is.null) &
                   sapply(dropped(object$models), is.null)
    # if (sum(user_models, na.rm = TRUE) != 1) {
    #     warning("One or more user models are present. ",
    #         "User model(s) will be plotted separately.")
    #   }
    net_out <- models_network(object)
    out <- igraph::graph_from_adjacency_matrix(net_out,
                                               mode = "directed")
    if (node_size_by_x) {
        if (is.null(x)) {
            if (inherits(object, "model_set")) {
                x <- object$bpp
              } else {
                stop("node_size_by_prob set to TRUE but",
                     "x cannot be extracted or not supplied.")
              }
          }
        node_size <- normalize_size(x,
                                    min_size = min_size,
                                    max_size = max_size)
      } else {
        node_size <- node_size
      }
    igraph::V(out)$size <- node_size
    p <- ncol(net_out)
    m_names <- colnames(net_out)
    i_original <- which(m_names == "original")
    i_add <- grepl("^add: ", m_names)
    i_drop <- grepl("^drop: ", m_names)
    color_tmp <- rep(color_others, p)
    color_tmp[i_original] <- color_original
    color_tmp[i_add] <- color_add
    color_tmp[i_drop] <- color_drop
    igraph::V(out)$color <- color_tmp
    igraph::V(out)$frame.color <- "grey75"
    igraph::V(out)$label <- v_labels(m_names)
    igraph::V(out)$label.color <- color_label
    igraph::V(out)$label.cex <- node_label_size
    igraph::V(out)$label.family <- "sans"
    igraph::E(out)$arrow.size <- .75
    out <- igraph::add_layout_(out, igraph::with_sugiyama())
    class(out) <- c("model_graph", class(out))
    out
  }

#' @title Plot a Network of Models
#'
#' @description Plot a network of models
#' generated by `model_graph()`.
#'
#' @details This function is the plot
#' method of `model_graph` objects,
#' the output of
#' [model_graph()].
#'
#' For now, it simply passes the object
#' to [plot.igraph()]. This function
#' is created for possible customization
#' of the plot in the future.
#'
#' @param x The output of
#' [model_graph()]. (Named `x`
#' because it is required in the naming
#' of arguments of the `plot` generic
#' function.)
#'
#' @param ... Additional arguments,
#' passed to [plot.igraph()].
#'
#' @seealso [model_graph()]
#'
#' @return `NULL`. Called for its
#' side effect.
#'
#' @examples
#'
#' library(lavaan)
#'
#' dat <- dat_path_model
#'
#' mod <-
#' "
#' x3 ~ a*x1 + b*x2
#' x4 ~ a*x1
#' ab := a*b
#' "
#'
#' fit <- sem(mod, dat_path_model, fixed.x = TRUE)
#'
#' out <- model_set(fit)
#' out
#'
#' g <- model_graph(out)
#' plot(g)
#'
#' @export

plot.model_graph <- function(x,
                             ...) {
    NextMethod()
  }
