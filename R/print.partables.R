#' @title Print a `partables`-Class Object
#'
#' @description Print the content of
#' a `partables`-class object.
#'
#' @details
#' The print method for the output
#' of [gen_models()], [get_add()],
#'  and [get_drop()].
#'
#' @return
#' `x` is returned invisibly.
#' Called for its side effect.
#'
#' @param x A `partables`-class object.
#'
#' @param max_tables The maximum number
#' of models to be printed.
#' Default is 10.
#'
#' @param ...  Optional arguments.
#' Ignored.
#'
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [gen_models()], [get_add()],
#'  and [get_drop()].
#'
#' @examples
#'
#' library(lavaan)
#' dat <- dat_path_model
#' mod <-
#' "
#' x3 ~ a*x1 + b*x2
#' x4 ~ a*x1
#' ab := a*b
#' "
#' fit <- sem(mod, dat_path_model, fixed.x = TRUE)
#' mod_to_add <- get_add(fit)
#' mod_to_add
#' print(mod_to_add, max_tables = 1)
#' mod_to_drop <- get_drop(fit)
#' mod_to_drop
#' print(mod_to_drop, max_tables = 1)
#'
#' @export

print.partables <- function(x,
                            max_tables = 10,
                            ...) {
    x_n <- length(x)
    x_names <- names(x)
    x_1 <- x[[1]]
    x_call <- attr(x, "call")
    cat("\n")
    cat("Call:\n")
    print(x_call)
    cat("\n")
    cat("Number of parameter tables: ", x_n, "\n", sep = "")
    cat("\n")
    if (x_n > max_tables) {
        x_tmp <- x_names[seq_len(max_tables)]
        cat("The first", max_tables, "modification(s):\n")
      } else {
        x_tmp <- x_names
        cat("The modifications/models:\n")
      }
    cat(paste(x_tmp, collapse = "\n"), "\n")
    invisible(x)
  }