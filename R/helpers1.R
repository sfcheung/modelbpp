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
    if (length(x) == 0) {
        return(x)
      }
    x0 <- lapply(x, function(y) {
                        if (y[2] == "~~") {
                            out <- y[3:1]
                            names(out) <- names(y)[3:1]
                            return(out)
                          }
                        NULL
                      })
    x0 <- x0[!sapply(x0, is.null)]
    x1 <- c(x, x0)
    x1
  }

#' @noRd

add_list_clean_duplicated_cov <- function(x) {
    if (length(x) == 0) {
        return(x)
      }
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

#' @noRd

mt_exclude_existing_pars <- function(mt, pt) {
    if (nrow(mt) == 0) {
        return(mt)
      }
    # Remove those already in the parameter tables
    mt_in_pt1 <- mapply(function(x, y) {
                            any((pt$lhs == x) & (pt$rhs == y))
                          },
                        x = mt$lhs,
                        y = mt$rhs,
                        USE.NAMES = FALSE)
    mt_in_pt2 <- mapply(function(x, y) {
                            any((pt$lhs == x) & (pt$rhs == y))
                          },
                        x = mt$rhs,
                        y = mt$lhs,
                        USE.NAMES = FALSE)
    mt_exclude_in_pt2 <- mt_in_pt1 | mt_in_pt2
    out <- mt[!mt_exclude_in_pt2, ]
    return(out)
  }

#' @noRd

mt_exclude_reversed <- function(mt, pt) {
    if (nrow(mt) == 0) {
        return(mt)
      }
    user_v <- unique(c(pt$lhs[pt$user %in% c(1, 0)],
                       pt$rhs[pt$user %in% c(1, 0)]))
    i_iv <- user_v[!user_v %in% pt$lhs[pt$op == "~"]]
    mt_exclude_iv_to_dv <- (mt$op == "~") & (mt$lhs %in% i_iv)
    out <- mt[!mt_exclude_iv_to_dv, ]
    return(out)
  }

#' @noRd

mt_remove_error_cov <- function(mt_list, sem_out) {
    if (length(mt_list) == 0) {
        return(mt_list)
      }
    ind <- lavaan::lavNames(sem_out, "ov.ind")
    ind_cov <- sapply(mt_list, function(x, indnames) {
                        if ((x[1] %in% indnames) &&
                            (x[2] == "~~") &&
                            (x[3] %in% indnames)) {
                              return(FALSE)
                            } else {
                              return(TRUE)
                            }
                      }, indnames = ind)
    out <- mt_list[ind_cov]
    return(out)
  }

#' @noRd

lor_to_list <- function(x) {
    out <- mapply(c,
                  lhs = x[, "lhs"],
                  op = x[, "op"],
                  rhs = x[, "rhs"],
                  SIMPLIFY = FALSE,
                  USE.NAMES = FALSE)
    return(out)
  }

#' @noRd

sets_remove_inadmissible <- function(sets) {
    out <- lapply(sets, function(x) {
        i <- length(x)
        x_to_remove <- rep(FALSE, i)
        x_lr <- lapply(x, function(y) y[c("lhs", "rhs")])
        for (j in seq_len(i)) {
            if (j > 1) {
                for (k in seq_len(j - 1)) {
                  if (all(x_lr[[j]] == x_lr[[k]]) |
                      all(x_lr[[j]] == x_lr[[k]][c("rhs", "lhs")])) {
                      x_to_remove[j] <- TRUE
                    }
                }
              }
          }
        x[!x_to_remove]
      })
    return(out)
  }

#' @noRd

pt_remove_user_defined <- function(pt,
                                   remove_constrained = TRUE,
                                   return_id = FALSE) {
    i1 <- pt$op == ":="
    i1_labels <- pt[i1, "lhs"]
    i2 <- (pt$lhs %in% i1_labels) & (pt$op == "==")
    if (remove_constrained) {
        i0 <- i1 | i2
      } else {
        i0 <- i1 & !i2
      }
    if (return_id) {
        return(!i0)
      } else {
        return(pt[!i0, ])
      }
  }

#' @noRd

pt_remove_constrained_equal <- function(pt,
                                        return_id = FALSE) {
    i1 <- pt$op == "=="
    id_eq <- c(pt$lhs[i1], pt$rhs[i1])
    id_eq <- unique(id_eq)
    i0 <- pt$plabel %in% id_eq
    if (return_id) {
        return(!i0)
      } else {
        return(pt[!i0, ])
      }
  }

#' @noRd

pt_remove_exo_var <- function(pt,
                              return_id = FALSE) {
    i1 <- (pt$exo > 0) & (pt$op == "~~")
    i2 <- (pt$lhs == pt$rhs)
    i0 <- i1 & i2
    if (return_id) {
        return(!i0)
      } else {
        return(pt[!i0, ])
      }
  }

#' @noRd

pt_remove_end_var <- function(pt,
                              return_id = FALSE) {
    i1 <- (pt$exo == 0) & (pt$op == "~~")
    i2 <- (pt$lhs == pt$rhs)
    i0 <- i1 & i2
    if (return_id) {
        return(!i0)
      } else {
        return(pt[!i0, ])
      }
  }

#' @noRd

feedback_and_xy_cov <- function(sem_out) {
    mod_all_paths_org <- suppressWarnings(manymome::all_indirect_paths(sem_out))
    if (length(mod_all_paths_org) == 0) {
        return(character(0))
      }
    mod_all_paths <- manymome::all_paths_to_df(mod_all_paths_org)
    all_feedback <- data.frame(lhs = mod_all_paths$x,
                               op = "~",
                               rhs = mod_all_paths$y)
    all_feedback <- unique(all_feedback)
    all_xy_cov <- data.frame(lhs = mod_all_paths$x,
                             op = "~~",
                             rhs = mod_all_paths$y)
    all_xy_cov <- unique(all_xy_cov)
    out <- list(all_feedback = all_feedback,
                all_xy_cov = all_xy_cov)
    out
  }

#' @noRd

df_to_lor <- function(object) {
    if (nrow(object) == 0) {
        return(character(0))
      }
    out <- mapply(function(a1, a2, a3) {
                      c(lhs = a1,
                        op = a2,
                        rhs = a3)
                    },
                    a1 = object$lhs,
                    a2 = object$op,
                    a3 = object$rhs,
                    SIMPLIFY = FALSE,
                    USE.NAMES = FALSE)
    out
  }
