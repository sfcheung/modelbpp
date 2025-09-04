#' @title Two or More Hypothesized Models
#'
#' @description Combine the 'model_set()'
#' results of two or more hypothesis
#' models.
#'
#' @details
#' There are cases in which users
#' have more than one hypothesized model,
#' each with its own set of neighboring
#' models.
#'
#' The function [model_set_combined()]
#' let users combine the [model_set()]
#' results two or more hypothesized model.
#' Users can then compare the BPPs
#' of these hypothesized models, as well
#' as their neighboring models. Equivalent
#' models will be removed in the process.
#'
#' @return
#' A `model_set`-class object, which is
#' simply an output of [model_set()].
#' All methods and functions for the
#' output of [model_set()] will also
#' work on this object.
#'
#' @param model_set_outputs This must be
#' a named list of the outputs of
#' [model_set()]. The names will be used
#' as prefixes to name the models.
#'
#' @param ... Additional arguments to be
#' passed to [model_set()].
#'
#' @seealso [model_set()]
#'
#' @examples
#'
#' library(lavaan)
#'
#' mod1 <-
#' "
#' x4 ~ x1
#' x7 ~ x4
#' "
#'
#' mod2 <-
#' "
#' x1 ~ x4
#' x7 ~ x4
#' "
#'
#' fit1 <- sem(mod1,
#'             HolzingerSwineford1939,
#'             fixed.x = FALSE)
#' fit2 <- sem(mod2,
#'             HolzingerSwineford1939,
#'             fixed.x = FALSE)
#'
#' out1 <- model_set(fit1)
#' out2 <- model_set(fit2)
#'
#' out1
#' out2
#'
#' outb <- model_set_combined(
#'             list(fit1 = out1,
#'                  fit2 = out2))
#'
#' outb
#'
#' @export
model_set_combined <- function(model_set_outputs,
                               ...) {

  # ---- Sanity Checks ----
  chk_class <- sapply(
                  model_set_outputs,
                  inherits,
                  what = "model_set")
  if (!all(chk_class)) {
    i <- names(chk_class)[!chk_class]
    tmp <- paste0(i,
                  collapse = ", ")
    stop("Some outputs are not from 'model_set()': ",
         tmp)
  }

  fit_names <- names(model_set_outputs)

  if (isTRUE(is.null(fit_names))) {
    stop("model_set_outputs must be a named list")
  }

  if (!isTRUE(all(sapply(fit_names, nchar) > 0))) {
    stop("Some elements have no valid names.")
  }

  # ---- Combine the Models ----

  f <- function(out, fit_name) {
          fits <- out$fit
          names(fits) <- paste0(fit_name, "_", names(fits))
          fits
        }
  all_fits <- mapply(
                f,
                out = model_set_outputs,
                fit_name = fit_names,
                SIMPLIFY = FALSE,
                USE.NAMES = TRUE
              )

  all_fits <- unlist(
                unname(all_fits),
                recursive = FALSE)

  # ---- Do the Analysis ----

  out <- model_set(all_fits,
                   ...)
  out
}