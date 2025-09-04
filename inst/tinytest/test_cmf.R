if (interactive() &&
    length(unclass(packageVersion("modelbpp"))[[1]]) == 4) {

suppressMessages(library(lavaan))

#' @title Two or More Hypothesized Model
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
#' as prefixes for to name the models.
#'
#' @param ... Additional arguments to be
#' passed to [model_set()].
#'
#' @seealso [model_set()]
#'
#' @examples
#' \donttest{
#' }
#'
#' @export
#'
#' @describeIn topic Description of this function
#' @order 1
model_set_combined <- function(model_set_outputs,
                               ...) {
  args <- list(...)

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

  out <- model_set(all_fits,
                   ...)
  out
}

mod1 <-
"
x2 ~ x1 + x4
x3 ~ x2
"

mod2 <-
"
x3 ~ x1
x3 ~ x2
x4 ~ x3
"

mod3 <-
"
x2 ~ x1
x2 ~ x3
x1 ~ x4
x3 ~ x4
"

fit1 <- sem(mod1, HolzingerSwineford1939, fixed.x = FALSE)
fit2 <- sem(mod2, HolzingerSwineford1939, fixed.x = FALSE)
fit3 <- sem(mod3, HolzingerSwineford1939, fixed.x = FALSE)

out1 <- model_set(fit1, progress = FALSE)
out2 <- model_set(fit2, progress = FALSE)
out3 <- model_set(fit3, progress = FALSE)

# Need to rename the fits due to name conflicts.
out1b <- out1
out2b <- out2
out3b <- out3
names(out1b$fit) <- paste0("fit1_", names(out1b$fit))
names(out2b$fit) <- paste0("fit2_", names(out2b$fit))
names(out3b$fit) <- paste0("fit3_", names(out3b$fit))

fit_all <- c(out1b$fit,
             out2b$fit,
             out3b$fit)
names(fit_all)

outa <- model_set(fit_all, progress = FALSE)

outa

outb <- model_set_combined(
            list(fit1 = out1,
                 fit2 = out2,
                 fit3 = out3),
            progress = FALSE)

outb

names(outa)
names(outb)

expect_equal(outa$bpp,
             outb$bpp)

}