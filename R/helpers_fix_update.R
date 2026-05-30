fix_call <- function(
  object,
  env_for_call
) {
  # Evaluate all the arguments and
  # set the call with the values
  call0 <- stats::getCall(object)

  # ==== Any names? ====

  chk_names <- sapply(
    call0[-1],
    is.name
  )
  if (!any(chk_names)) {
    return(object)
  }

  # ==== Evaluate the argument values ====

  tmp <- lapply(
            call0,
            \(x, envir0) eval(x, envir0),
            envir0 = env_for_call
          )
  tmp <- as.call(tmp)
  tmp[[1]] <- call0[[1]]
  object@call <- tmp
  object
}

#' @noRd
auto_ram <- function(
  FUN = lavaan::sem,
  ...
) {
  ddd <- list(...)
  fit <- suppressWarnings(
              tryCatch(do.call(
                FUN,
                ddd
              ),
            error = function(e) e)
          )
  if (inherits(fit, "error")) {
    if (isTRUE(grepl("not defined in the LISREL representation",
                    fit$message))) {
      ddd <- utils::modifyList(
        ddd,
        list(representation = "RAM")
      )
      fit <- suppressWarnings(
                  tryCatch(do.call(
                    FUN,
                    ddd
                  ),
                error = function(e) e)
              )
      if (inherits(fit, "error")) {
        stop(fit)
      }
    }
  }
  fit
}