#' @title Convert a 'model_set'-Object
#' to an Adjacency Matrix
#'
#' @description Convert a
#' 'model_set'-Object to an adjacency
#' matrix for 'igraph'.
#'
#' @param object A `model_set`-class
#' object or a `partables`-class
#' object.
#'
#' @return
#' A numeric matrix of zeros and ones,
#' to indicate the relations between
#' models.
#'
#' @noRd

models_network <- function(object) {
    if (inherits(object, "model_set")) {
        models <- object$models
      } else {
        if (!inherits(object, "partables")) {
            stop("The object must be either a",
                 "model_set object or a",
                 "partables object.")
          }
      }
    m_added <- added(models)
    m_dropped <- dropped(models)
    chk <- mapply(c, m_added, m_dropped)

    p <- length(models)
    i <- which(names(models) == "original")
    net_out <- matrix(0, p, p)
    colnames(net_out) <- rownames(net_out) <- names(models)

    tmp <- sapply(m_added, length) == 1
    net_out[i, tmp] <- 1

    tmp <- sapply(m_dropped, length) == 1
    net_out[tmp, i] <- 1

    net_out <- mark_added(net_out,
                          m_added = m_added)
    net_out <- mark_dropped(net_out,
                            m_dropped = m_dropped)
    net_out
  }


#' @title Mark Models Formed by Adding Free Parameters
#'
#' @noRd

mark_added <- function(net_out,
                       m_added) {
    for (i in seq_len(length(m_added))) {
        for (j in seq_len(length(m_added))) {
            x <- m_added[[i]]
            y <- m_added[[j]]
            if (identical(x, y)) next
            if (is.null(x)) next
            if (is.null(y)) next
            tmp <- union(x, y)
            tmp1 <- setequal(tmp, x)
            tmp2 <- setequal(tmp, y)
            if (tmp1 && !tmp2) {
                z <- length(setdiff(x, y))
                if (z == 1) {
                    net_out[j, i] <- z
                  }
              }
          }
      }
    net_out
  }

#' @title Mark Models Formed by Dropping Free Parameters
#'
#' @noRd

mark_dropped <- function(net_out,
                       m_dropped) {
    for (i in seq_len(length(m_dropped))) {
        for (j in seq_len(length(m_dropped))) {
            x <- m_dropped[[i]]
            y <- m_dropped[[j]]
            if (identical(x, y)) next
            if (is.null(x)) next
            if (is.null(y)) next
            tmp <- union(x, y)
            tmp1 <- setequal(tmp, x)
            tmp2 <- setequal(tmp, y)
            if (tmp1 && !tmp2) {
                z <- length(setdiff(x, y))
                if (z == 1) {
                    net_out[i, j] <- z
                  }
              }
          }
      }
    net_out
  }

#' @title Add Weights to an Adjacency Matrix
#'
#' @noRd

add_weight <- function(net_out,
                       bpp) {
    for (i in seq_len(nrow(net_out))) {
        for (j in seq_len(ncol(net_out))) {
            if (net_out[i, j] != 0) {
                net_out[i, j] <- bpp[i] - bpp[j]
              }
          }
      }
    net_out
  }

#' @title Normalize Node Size
#'
#' @noRd

normalize_size <- function(bpp,
                           min_size = 1,
                           max_size = 30) {
    bpp_min <- min(bpp)
    bpp_max <- max(bpp)
    bpp_range <- bpp_max - bpp_min
    tmp_size <- max_size * (bpp - bpp_min)/bpp_range +
                min_size
    tmp_size
  }

#' @title Labels for Vertices
#'
#' @noRd

v_labels <- function(x) {
    out0 <- gsub(": ", ":\n", x, fixed = TRUE)
    out0 <- gsub(";", "\n", out0, fixed = TRUE)
    out0
  }