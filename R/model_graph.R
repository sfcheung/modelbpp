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
#' This is the default way to construct
#' the graph when the model set is
#' automatically by [model_set()].
#'
#' - Each model is connected by an
#'  arrow, pointing from one model to
#'  another model that
#'
#'    a. can be formed by adding one
#'       or more free parameter, or
#'
#'    b. can be formed by releasing one
#'       or more equality constraint
#'       between two
#'       parameters.
#'
#'    c. has nested relation with this
#'       model as determined by the
#'       method proposed by Bentler
#'       and Satorra (2010), if the
#'       models are not generated
#'       internally.
#'
#'  That is, it points to a model with
#'  more degrees of freedom (more
#'  complicated),
#'  and
#'  is nested within that model in either
#'  parameter sense or covariance sense.
#'
#' - By default, the size of the node
#'  for each model is scaled by its
#'  BIC posterior probability, if
#'  available. See *The Size of a Node*
#'  below.
#'
#' - If a model is designated as the
#'  original (target) model,
#'  than he original model, the models
#'  with
#'  more degrees of freedom than the
#'  original model,
#'  and the models with fewer degrees
#'  of freedom than the original models,
#'  are colored differently.
#'
#' - The default layout is the Sugiyama
#'  layout, with simpler models (models
#'  with fewer degrees of freedom) on
#'  the top. The lower a model is in
#'  the network, the more the degrees
#'  of freedom it has. This layout is
#'  suitable for showing the nested
#'  relations of the models. Models on
#'  the same level (layer) horizontally
#'  have the same model *df*.
#'
#' The output is an `igraph` object.
#' Users can customize it in any way
#' they want using functions from
#' the `igraph` package.
#'
#' If a model has no nested relation
#' with all other model, it will not
#' be connected to other models.
#'
#' If no model is named `original`
#' (default is `"original"`), then no
#' model is colored as the original model.
#'
#' ## User-Provided Models
#'
#' If `object` contained one or more
#' user-provided models which are not
#' generated automatically by
#' [model_set()] or similar functions
#' (e.g., [gen_models()]), then the
#' method by Bentler and Satorra (2010)
#' will be used to determine model
#' relations. Models connected by an
#' arrow has a nested relation based on
#' the NET method by Bentler and Satorra
#' (2010). An internal function inspired
#' by [semTools::net()] is used to
#' implement the NET method.
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
#' @param original String. The name
#' of the original model (target model).
#' Default is `"original"`.
#'
#' @param drop_redundant_direct_paths
#' Logical. Whether the redundant direct
#' path between two models. A direct path
#' is redundant if two models are also
#' connected through at least one
#' another model. Default is `TRUE`.
#'
#' @param label_arrow_by_df If `TRUE`,
#' then an arrow (edge) is always labelled
#' by the difference in model *df*s.
#' If `FALSE`, then no arrows are
#' labelled. If `NULL`, then arrows are
#' labelled when not all differences
#' in model *df*s are equal to one.
#' Default is `NULL`.
#'
#' @param arrow_label_size The size of
#' the labels of the arrows (edges), if labelled.
#' Default is 1.
#'
#' @param weight_arrows_by_df String.
#' Use if model *df*
#' differences are stored.
#' If `"inverse"`, larger the
#' difference in model *df*, *narrower*
#' an arrow. That is, more similar two
#' models are, thicker the arrow. If
#' `"normal"`, larger the difference
#' in model *df*, *wider* an arrow.
#' If `"none"`, then arrow width is
#' constant, set to `arrow_max_width`.
#' Default is `"inverse"`.
#'
#' @param arrow_min_width If
#' `weight_arrows_by_df` is not `"none"`,
#' this is the minimum width of an
#' arrow.
#'
#' @param arrow_max_width If
#' `weight_arrows_by_df` is not `"none"`,
#' this is the maximum width of an
#' arrow. If `weight_arrows_by_df` is
#' `"none"`, this is the width of all
#' arrows.
#'
#' @param progress Whether a progress
#' bar will be displayed for some
#' steps (e.g., checking for nested
#' relations). Default
#' is `TRUE`.
#'
#' @param short_names If `TRUE` and
#' short model names are stored,
#' they will be used as model labels.
#' Please print the object with
#' `short_names = TRUE` to find the
#' corresponding full model names.
#'
#' @param min_bpp_labelled If not `NULL`,
#' this is the minimum BPP for a model
#' to be labelled. Models with BPP less
#' than this value will not be labelled.
#' Useful when the number of models
#' is large.
#'
#' @param ... Optional arguments. Not
#' used for now.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#' The internal function for nesting
#' inspired by [semTools::net()],
#' which was developed by
#' Terrence D. Jorgensen.
#'
#' @references
#' Bentler, P. M., & Satorra, A. (2010).
#' Testing model nesting and equivalence.
#' *Psychological Methods, 15*(2),
#' 111--123. \doi{10.1037/a0019625}
#' Asparouhov, T., & Muthén, B. (2019).
#' Nesting and Equivalence Testing for
#' Structural Equation Models.
#' *Structural Equation Modeling: A Multidisciplinary Journal, 26*(2),
#' 302--309. \doi{10.1080/10705511.2018.1513795}
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
                        original = "original",
                        drop_redundant_direct_paths = TRUE,
                        label_arrow_by_df = NULL,
                        arrow_label_size = 1,
                        weight_arrows_by_df = c("inverse", "normal", "none"),
                        arrow_min_width = .5,
                        arrow_max_width = 2,
                        progress = TRUE,
                        short_names = FALSE,
                        min_bpp_labelled = NULL,
                        ...) {
    if (!all(object$converged)) {
        stop("Not all models converged.")
      }
    user_models <- sapply(added(object$models), is.null) &
                   sapply(dropped(object$models), is.null)
    # TODO: Should always use models_network2()?
    if (sum(user_models, na.rm = TRUE) != 1) {
        # warning("One or more user models are present. ",
        #     "User model(s) will be plotted separately.")
        net_out <- models_network2(object,
                                   one_df_only = FALSE,
                                   progress = progress)
      } else {
        net_out <- models_network(object)
      }
    out <- igraph::graph_from_adjacency_matrix(net_out,
                                               mode = "directed",
                                               weight = "df")
    if (drop_redundant_direct_paths) {
        out <- delete_all_redundant_direct(out)
      }
    # Store Basic Info
    out <- doc_model_graph(object,
                           out)
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
    i_original <- which(m_names == original)
    i_add <- grepl("^add: ", m_names)
    i_drop <- grepl("^drop: ", m_names)
    color_tmp <- rep(color_others, p)
    if (length(i_original) > 0) {
        color_tmp[i_original] <- color_original
      }
    color_tmp[i_add] <- color_add
    color_tmp[i_drop] <- color_drop
    igraph::V(out)$color <- color_tmp
    igraph::V(out)$frame.color <- "grey75"
    igraph::V(out)$label <- v_labels(m_names)
    igraph::V(out)$label.color <- color_label
    igraph::V(out)$label.cex <- node_label_size
    igraph::V(out)$label.family <- "sans"
    igraph::E(out)$arrow.size <- .75
    out <- label_by_df(out,
                       mode = label_arrow_by_df)
    if (!is.null(igraph::E(out)$df)) {
        weight_arrows_by_df <- match.arg(weight_arrows_by_df)
        out <- edge_weight(out,
                           mode = weight_arrows_by_df,
                           min_width = arrow_min_width,
                           max_width = arrow_max_width)
      }
    igraph::E(out)$label.cex <- arrow_label_size
    out <- igraph::add_layout_(out, igraph::with_sugiyama())
    out <- layer_by_df(out,
                       model_set_out = object)
    if (short_names) {
        if (is.character(object$short_names)) {
            tmp1 <- object$short_names
            tmp2 <- igraph::V(out)$name
            igraph::V(out)$full_name <- tmp2
            igraph::V(out)$label <- tmp1[tmp2]
          }
      }
    if (is.numeric(min_bpp_labelled)) {
        tmp2 <- igraph::V(out)$label
        tmp3 <- igraph::V(out)$name
        tmp1 <- (object$bpp < min_bpp_labelled)
        tmp1 <- tmp1[tmp3]
        tmp4 <- tmp2
        tmp4[tmp1] <- ""
        igraph::V(out)$label <- tmp4
      }
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
#' to `plot`-method of an `igraph` object.
#' This function
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
#' passed to the `plot`-method of
#' an `igraph` object.
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
