#' @title Helper Functions For `partables`-Class Objects
#'
#' @description For tasks such as comparing two parameter
#' tables inside a `partables`-class object.
#'
#' @name partables_helpers
NULL

#' @details
#'
#' The function [identical_partables()] compare two
#' `lavaan` parameter tables and see whether they
#' are identical. (Adapted from a similar function
#' in the package `semhelpinghands`).
#'
#' @param object1 A `lavaan` parameter
#' table or similar object.
#'
#' @param object2 A `lavaan` parameter
#' table or similar object.
#'
#' @return
#' The function [identical_partables()]
#' returns either `TRUE` or `FALSE`.
#'
#' @examples
#'
#' library(lavaan)
#' mod1 <-
#' "
#' x3 ~ x1
#' x2 ~ x4
#' "
#' mod2 <-
#' "
#' x2 ~ x4
#' x3 ~ x1
#' "
#' fit1 <- sem(mod1, dat_path_model)
#' fit2 <- sem(mod2, dat_path_model)
#' pt1 <- parameterTable(fit1)
#' pt2 <- parameterTable(fit2)
#' identical_partables(pt1, pt2)
#'
#' @rdname partables_helpers
#' @export

identical_partables <- function(object1,
                                object2) {
    if (!is_partable(object1) ||
        !is_partable(object2)) {
        stop("At least one of the objects is not a parameter table.")
      }
    # Drop nonessential columns
    object1$est <- NULL
    object2$est <- NULL
    object1$se <- NULL
    object2$se <- NULL
    pt1 <- fix_cov(object1)
    pt2 <- fix_cov(object2)

    tmp1 <- pt1[order(pt1$group,
                      pt1$op, pt1$lhs, pt1$rhs), ]
    tmp2 <- pt2[order(pt2$group,
                      pt2$op, pt2$lhs, pt2$rhs), ]
    rownames(tmp1) <- NULL
    rownames(tmp2) <- NULL
    if (!identical(tmp1[, c("group", "lhs", "op", "rhs")],
                   tmp2[, c("group", "lhs", "op", "rhs")])) {
        return(FALSE)
      }
    if (!identical(tmp1$free > 0,
                   tmp2$free > 0)) {
        return(FALSE)
      }
    if (!is.null(tmp1$label) &&
        !is.null(tmp2$label)) {
        if (!identical(tmp1$label,
                       tmp2$label)) {
            return(FALSE)
          }
      }
    if (!is.null(tmp1$exo) &&
        !is.null(tmp2$exo)) {
        if (!identical(tmp1$exo,
                       tmp2$exo)) {
            return(FALSE)
          }
      }
    if (!is.null(tmp1$ustart) &&
        !is.null(tmp2$ustart)) {
        tmp1u <- tmp1[which(tmp1$ustart > 0), ]
        tmp2u <- tmp2[which(tmp2$ustart > 0), ]
        if (!identical(tmp1u$start,
                       tmp2u$start)) {
            return(FALSE)
          }
      }
    return(TRUE)
  }

#' @param x An object to be checked.
#'
#' @details
#' The function [is_partable()] tries
#' to
#' check whether an object is "likely"
#' to be a parameter table that can be
#' used by [lavaan::lavaan()] and its
#' wrappers, such as [lavaan::sem()].
#'
#' Note that there is no guarantee the
#' the parameter table makes sense or
#' will not lead to error when fitted.
#' It can only check whether it has the
#' required columns.
#'
#' @return
#' The function [is_partable()]
#' returns either `TRUE` or `FALSE`.
#'
#' @examples
#'
#' is_partable(pt1)
#'
#' @rdname partables_helpers
#' @export

is_partable <- function(x) {
    if (!inherits(x, what = "data.frame")) {
        # stop("x is not a data-frame-like object.")
        return(FALSE)
      }
    # Must at least have these columns
    min_names <- c("id",
                   "lhs",
                   "op",
                   "rhs",
                   "free",
                   "ustart")
    if (!all(min_names %in% colnames(x))) {
        # stop("x does not have minimally required columns.")
        return(FALSE)
      }
    return(TRUE)
  }

#' @details
#' The function [same_variables()]
#' check whether all parameter labels
#' in a `partables`-class object use
#' the same observed variables.
#'
#' @return
#' The function [same_variables()]
#' returns either `TRUE` or `FALSE`.
#'
#' @examples
#'
#' out <- model_set(fit1,
#'                  fit_models = FALSE)
#' same_variables(get_partables(out))
#'
#'
#' @rdname partables_helpers
#' @export

same_variables <- function(x) {
    if (length(x) == 1) {
        return(TRUE)
      }
    out0 <- lapply(x, function(xx) {
                out <- tryCatch(lavaan::lavaan(xx))
                out
              })
    if (any(sapply(out0, inherits, what = "error"))) {
        stop("One or more parameter tables result in error.")
      }
    all_names <- lapply(out0,
                        lavaan::lavNames,
                        "ov")
    all_names0 <- all_names[[1]]
    for (xx in all_names[-1]) {
        if (!setequal(all_names0, xx)) {
            return(FALSE)
          }
      }
    return(TRUE)
  }


#' @details
#' The function [get_partables()]
#' extract the `partable` object from
#' a `model_set`-class object.
#'
#' @return
#' The function [get_partables()]
#' returns a `partables`-class
#' object.
#'
#' @examples
#'
#' out <- model_set(fit1,
#'                  fit_models = FALSE)
#' get_partables(out)
#'
#' @rdname partables_helpers
#' @export

get_partables <- function(x) {
    out <- x$models
    if (!inherits(out, what = "partables")) {
        return(NULL)
      }
    out
  }


#' @details
#' The function [to_partables()]
#' combine objects to create a
#' `partables`-class object.
#' The objects to be combined can be
#' a `lavaan`-class object (e.g.,
#' the output of [lavaan::sem()])
#' or a parameter table of `lavaan`.
#'
#' @return
#' The function [to_partables()]
#' returns a `partables`-class
#' object, created from the objects
#' supplied.
#'
#' @param ... The objects to be combined.
#'
#' @examples
#'
#' fit1 <- sem(mod1, dat_path_model)
#' fit2 <- sem(mod2, dat_path_model)
#' pt1 <- parameterTable(fit1)
#' pt2 <- parameterTable(fit2)
#'
#' to_partables(fit1, fit2)
#' to_partables(pt1, pt2)
#'
#' @rdname partables_helpers
#' @export

to_partables <- function(...) {
    ddd <- list(...)
    ddd_names <- sapply(as.list(match.call()[-1]), deparse)
    if (!is.null(names(ddd_names))) {
        i <- which(sapply(names(ddd_names), function(x) {nchar(x) > 0}))
        if (length(i) > 0) {
            ddd_names[i] <- names(ddd_names)[i]
            ddd_names <- unname(ddd_names)
          }
      }
    out <- lapply(ddd,
                  function(x) {
                      if (inherits(x, "lavaan")) {
                          return(lavaan::parameterTable(x))
                        }
                      if (is_partable(x)) {
                          return(x)
                        }
                      stop("At least one object is neither a lavaan object ",
                           "nor a parameter table.")
                    })
    names(out) <- ddd_names
    class(out) <- c("partables", class(out))
    out
  }


#' @noRd

fix_cov <- function(x) {
    i <- which(x$op == "~~")
    if (length(i) == 0) {
        return(x)
      }
    for (j in i) {
        tmp <- sort(unlist(x[j, c("lhs", "rhs")]))
        x[j, "lhs"] <- tmp[1]
        x[j, "rhs"] <- tmp[2]
      }
    return(x)
  }