library(lavaan)

dat <- dat_path_model_p06

mod <- attr(dat, "mod")

fit <- sem(mod, dat_path_model_p06, fixed.x = TRUE)
pt <- parameterTable(fit)
pt_no_user <- pt[pt$op != ":=", ]
pt0 <- parameterTable(fit)
mod_to_add <- get_add(fit)
fit_add <- lapply(mod_to_add, function(x) update(fit, x))
anova_add <- lapply(fit_add, function(x) anova(x, fit))


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



test_that("Parameters to drop as expected", {
    expect_true(
        all(names(mod_to_add) %in%
          c("add: y4~~y6", "add: y4~y6", "add: y5~x2", "add: y6~y4", "add: y6~x1",
            "add: y6~x2", "add: (y4~x1),(y4~x2)"))
      )
  })

test_that("All df differences are one", {
    expect_true(
        all(sapply(anova_add, function(x) x[2, "Df diff"]) == 1)
      )
  })

test_that("Generated difference matches the names", {
    expect_true(
        all(sapply(mod_to_add, get_diff, pt_no_user) == names(mod_to_add))
      )
  })
