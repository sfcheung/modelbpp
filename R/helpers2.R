#' @noRd

unique_models <- function(partables) {
    i_added <- added(partables)
    i_dropped <- dropped(partables)
    i_added <- lapply(i_added, sort)
    i_dropped <- lapply(i_dropped, sort)
    i_comb <- mapply(function(x, y) {list(x, y)},
                     x = i_added,
                     y = i_dropped,
                     SIMPLIFY = FALSE)
    j <- !duplicated(i_comb)
    out <- partables[j]
    out
  }

#' @noRd

added <- function(partables) {
    i1 <- sapply(partables, attr,
                 which = "parameters_added")
    i2 <- sapply(partables, attr,
                 which = "constraints_released")
    i <- mapply(c, i1, i2)
    i <- lapply(i, unlist)
    i
  }

#' @noRd

dropped <- function(partables) {
    i <- sapply(partables, attr,
                which = "parameters_dropped")
    i <- lapply(i, unlist)
    i
  }
