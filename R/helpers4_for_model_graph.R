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
