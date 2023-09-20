#' @noRd

get_diff <- function(x, pt_no_user) {
  x_lor <- x[, c("lhs", "op", "rhs")]
  pt_lor <- pt_no_user[, c("lhs", "op", "rhs")]
  pt_lor$old <- TRUE
  # New parameters
  x_pt <- merge(x_lor, pt_lor, all.x = TRUE)
  x_new <- x_pt[is.na(x_pt$old), c("lhs", "op", "rhs")]
  if (nrow(x_new) > 0) {
      x_new_str <- apply(x_new, 1, paste, collapse = "")
    } else {
      x_new_str <- NULL
    }
  # Constraints released
  x_lor$mod <- FALSE
  x_pt_c <- merge(pt_lor, x_lor, all.x = TRUE)
  x_released <- x_pt_c[is.na(x_pt_c$mod), c("lhs", "op", "rhs")]
  if (nrow(x_released) > 0) {
      x_constr <- lapply(seq_len(nrow(x_released)),
                      function(z) c(x_released[z, 1],
                                    x_released[z, 2],
                                    x_released[z, 3]))
      x_constr_names <- sapply(x_constr, function(z, pt) {
          id_x <- which((pt$lhs == z[1]) & (pt$op == z[2]) & (pt$rhs == z[3]))
          i_lhs <- which(pt$plabel == pt[id_x, "lhs"])
          i_rhs <- which(pt$plabel == pt[id_x, "rhs"])
          p_lhs <- paste0("(", paste(pt[i_lhs, c("lhs", "op", "rhs")], collapse = ""), ")")
          p_rhs <- paste0("(", paste(pt[i_rhs, c("lhs", "op", "rhs")], collapse = ""), ")")
          paste0(p_lhs, ",", p_rhs)
        }, pt = pt_no_user)
    } else {
      x_constr_names <- NULL
    }
  out_str <- paste0(c(x_new_str, x_constr_names), collapse = ";")
  out_ops <- paste("add:", out_str)
  out_ops
}

#' @noRd

get_diff_drop <- function(x, pt_no_user) {
  tmp <- c("lhs", "op", "rhs", "block", "group", "label", "free")
  x0 <- x[, tmp]
  pt_no_user0 <- pt_no_user[, tmp]
  x0$free <- x0$free > 0
  pt_no_user0$free <- pt_no_user0$free > 0
  out <- x0[which(apply(x0 != pt_no_user0, 1, any, na.rm = TRUE)), ]
  out_ops <- sapply(seq_len(nrow(out)), function(x) {
      paste0(out[x, "lhs"], out[x, "op"], out[x, "rhs"], collapse = "")
    })
  out_ops <- paste(out_ops, collapse = ";")
  out_ops <- paste("drop:", out_ops)
}