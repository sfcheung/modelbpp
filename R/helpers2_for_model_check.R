#' @noRd

unique_models <- function(partables,
                          original = "original",
                          progress = FALSE) {
    i_added <- added(partables)
    i_dropped <- dropped(partables)
    i_added <- lapply(i_added, sort)
    i_dropped <- lapply(i_dropped, sort)
    i_comb <- mapply(function(x, y) {list(x, y)},
                     x = i_added,
                     y = i_dropped,
                     SIMPLIFY = FALSE)
    i_null <- sapply(i_comb,
                      function(x) {
                          is.null(x[[1]]) && is.null(x[[2]])
                        }
                     )
    if (any(i_null)) {
        i_comb2 <- which(i_null)
      } else {
        i_comb2 <- NULL
      }
    if (!is.null(i_comb2)) {
        i_comb2 <- i_comb2[!duplicated(names(i_comb2))]
      }
    j <- !duplicated(i_comb)
    if (any(j)) {
        j_comb2 <- which(j)
      } else {
        j_comb2 <- NULL
      }
    out <- partables[unique(c(j_comb2, i_comb2))]
    out <- duplicated_by_pt(out,
                            original = original,
                            progress = progress)
    out
  }

#' @noRd

duplicated_by_pt <- function(partables_list,
                             original = "original",
                             progress = FALSE) {
    p <- length(partables_list)
    if (p <= 1) {
        return(partables_list)
      }
    p_names <- names(partables_list)
    chk <- rep(FALSE, p)
    if (progress) {
        pstar <- p * (p - 1) / 2
        cat("\nCheck for duplicated models (",
             p,
             " model[s] to check):\n",
             sep = "")
        pb <- utils::txtProgressBar(min = 0,
                                    max = pstar,
                                    width = 50,
                                    char = "+",
                                    style = 3)
      }
    k <- 0
    for (i in seq_len(p - 1)) {
        for (j in seq(from = i + 1, to = p)) {
            k <- k + 1
            if (progress) {
                utils::setTxtProgressBar(pb,
                                         value = k)
              }
            if (identical_partables(partables_list[[i]],
                                    partables_list[[j]])) {
                if (p_names[j] == original) {
                    chk[i] <- TRUE
                  } else {
                    chk[j] <- TRUE
                  }
              }
          }
      }
    if (progress) {
        close(pb)
      }
    out <- partables_list[!chk]
    out
  }

#' @noRd

added <- function(partables) {
    i1 <- sapply(partables, attr,
                 which = "parameters_added")

    i2 <- sapply(partables, attr,
                 which = "constraints_released")
    # i2 <- sapply(i2, function(x) {
    #           if (length(x) > 0) {
    #               strsplit(x[[1]],
    #                        split = ";",
    #                        fixed = TRUE)[[1]]
    #             } else {
    #               return(NULL)
    #             }
    #         })
    i <- mapply(c, i1, i2)
    i <- lapply(i, unlist)
    i
  }

#' @noRd

dropped <- function(partables) {
    i <- sapply(partables, attr,
                which = "parameters_dropped")
    # i <- sapply(i, function(x) {
    #           if (length(x) > 0) {
    #               strsplit(x[[1]],
    #                        split = ";",
    #                        fixed = TRUE)[[1]]
    #             } else {
    #               return(NULL)
    #             }
    #         })
    i <- lapply(i, unlist)
    i
  }
