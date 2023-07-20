#' @noRd

syntax_to_id <- function(x,
                         ptable) {
    x0 <- paste0(unlist(x), collapse = "\n")
    x_df0 <- lavaan::lavParseModelString(
                        model.syntax = x0,
                        as.data.frame. = TRUE,
                        warn = FALSE)
    x_df1 <- x_df0[, c("lhs", "op", "rhs")]
    x_df3 <- merge(x_df1, ptable)
    x_df3$id
  }

#' @noRd

syntax_to_add_list <- function(x) {
    x0 <- lavaan::lavParseModelString(
                        model.syntax = x,
                        as.data.frame. = TRUE,
                        warn = FALSE)
    x1 <- mapply(c, x0$lhs, x0$op, x0$rhs,
                 USE.NAMES = FALSE,
                 SIMPLIFY = FALSE)
    x1
  }

#' @noRd

add_list_duplicate_cov <- function(x) {
    x0 <- lapply(x, function(y) {
                        if (y[2] == "~~") return(y[3:1])
                        NULL
                      })
    x0 <- x0[!sapply(x0, is.null)]
    x1 <- c(x, x0)
    x1
  }

#' @noRd

add_list_clean_duplicated_cov <- function(x) {
    x0 <- sapply(seq_len(length(x)),
                 function(y) {
                    y0 <- x[[y]]
                    if (y0[2] != "~~") return(TRUE)
                    for (j in seq_len(y)) {
                        if (identical(x[[j]], y0[3:1])) {
                            return(FALSE)
                          }
                      }
                    TRUE
                   })
    x[x0]
  }

#' @noRd

constr_to_lor <- function(x,
                          ptable) {
    rowid1 <- (ptable$plabel == x[1])
    rowid2 <- (ptable$plabel == x[3])
    lor1 <- c(ptable$lhs[rowid1],
              ptable$op[rowid1],
              ptable$rhs[rowid1])
    lor2 <- c(ptable$lhs[rowid2],
              ptable$op[rowid2],
              ptable$rhs[rowid2])
    list(lor1, lor2)
  }
