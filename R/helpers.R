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
    x1 <- mapply(c,
                 lhs = x0$lhs,
                 op = x0$op,
                 rhs = x0$rhs,
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
                          pt) {
    rowid1 <- (pt$plabel == x[1])
    rowid2 <- (pt$plabel == x[3])
    lor1 <- c(lhs = pt$lhs[rowid1],
              op = pt$op[rowid1],
              rhs = pt$rhs[rowid1])
    lor2 <- c(lhs = pt$lhs[rowid2],
              op = pt$op[rowid2],
              rhs = pt$rhs[rowid2])
    list(lor1, lor2)
  }

#' @noRd

constr_pars <- function(constr, pt) {
    out <- sapply(constr, function(z, pt) {
        id_x <- which((pt$lhs == z["lhs"]) &
                      (pt$op == z["op"]) &
                      (pt$rhs == z["rhs"]))
        i_lhs <- which(pt$plabel == pt[id_x, "lhs"])
        i_rhs <- which(pt$plabel == pt[id_x, "rhs"])
        p_lhs <- paste0("(",
                        paste(pt[i_lhs, c("lhs", "op", "rhs")],
                              collapse = ""),
                        ")")
        p_rhs <- paste0("(",
                        paste(pt[i_rhs, c("lhs", "op", "rhs")],
                              collapse = ""),
                        ")")
        paste0(p_lhs, ",", p_rhs)
      }, pt = pt)
    return(out)
  }

#' @noRd

par_names <- function(pars_list) {
    out <- sapply(pars_list, paste, collapse = "")
    out <- paste0(out, collapse = ";")
    return(out)
  }

#' @noRd

release_constr <- function(constr, pt) {
    for (j in constr) {
        i <- which((pt$lhs == j[1]) & (pt$op == j[2]) & (pt$rhs == j[3]))
        dot_p <- c(pt[i, "lhs"], pt[i, "rhs"])
        p_others <- unique(c(pt[-i, "lhs"], pt[-i, "rhs"]))
        dot_p_out <- dot_p[!(dot_p %in% p_others)]
        pt <- pt[-i, ]
        pt[pt$plabel %in% dot_p_out, "label"] <- ""
      }
    return(pt)
  }