#' @title Models That Are More Restricted
#'
#' @description Generate a list of
#' models with one or more free parameter
#' dropped (fixed to zero).
#'
#' @details Generate a list of models
#' with one or more free parameters
#' dropped, that is, fixed to zero
#' (with degrees of freedom,
#' *df*, increases by one or more).
#'
#' All free parameters are included in
#' the pool of candidates, except for
#' those explicitly requested to be
#' kept.
#'
#' The models will be checked by `lavaan`
#' to make sure that the increase in
#' model degrees of freedom is of the
#' expected value.
#'
#' @param sem_out The output from an
#' SEM function. Currently support
#' [lavaan::lavaan-class] only.
#' Usually the one used [get_add()] or
#' [get_drop()] to generate the
#' list of models.
#'
#' @param must_drop A character vector
#' of parameters, named in
#' `lavaan::lavaan()` style (e.g.,
#' `"y ~ x"`), that must be included.
#' Default is `NULL`.
#'
#' @param must_not_drop A character
#' vector of parameters, named in
#' [lavaan::lavaan()] style (e.g.,
#' `"x1 ~~ x1"`), that must not be
#' included. Default is `NULL`.
#'
#' @param df_change How many degrees
#' of freedom away in the list. All
#' models with *df* change less than
#' or equal to this number will be
#' included. Default is 1.
#'
#' @param model_id The identification
#' number of the starting model.
#' Default is `NA`, no identification
#' number.
#'
#' @param keep_correct_df_change Keep
#' only tables with actual *df* change
#' equal to expected *df* change.
#'
#' @param remove_duplicated If `TRUE`,
#' the default, duplicated models are
#' removed.
#'
#' @return An object of the class
#' `partables`, a named list of parameter
#' tables, each of them to be used by
#' [lavaan::lavaan()] or [update()]
#' for fitting a model with the added
#' parameters.
#'
#' @seealso [print.partables()]
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#'
#' library(lavaan)
#'
#' dat <- dat_path_model
#' mod <-
#' "
#' x3 ~ a*x1 + b*x2
#' x4 ~ a*x1 + x2
#' ab := a*b
#' "
#'
#' fit <- sem(mod, dat_path_model, fixed.x = TRUE)
#' mod_to_drop <- get_drop(fit)
#' mod_to_drop
#'
#' @export

get_drop <- function(sem_out,
                     must_drop = NULL,
                     must_not_drop = NULL,
                     df_change = 1,
                     model_id = NA,
                     keep_correct_df_change = TRUE,
                     remove_duplicated = TRUE
                    ) {
    if (missing(sem_out)) stop("sem_out is not supplied.")
    if (!inherits(sem_out, "lavaan")) {
        stop("sem_out is not a lavaan-class object.")
      }
    pt <- lavaan::parameterTable(sem_out)
    # Remove all user-defined parameters unless constrained
    pt <- pt_remove_user_defined(pt, remove_constrained = FALSE)
    # Exclude all parameters already constrained to be equal
    id_exclude_eq <- pt_remove_constrained_equal(pt, return_id = TRUE)
    # Exclude the variances of exogenous variables
    id_exclude_exo_var <- pt_remove_exo_var(pt, return_id = TRUE)
    # Exclude error variances of endogenous variables
    id_exclude_end_var <- pt_remove_end_var(pt, return_id = TRUE)
    # Determine the candidate lists
    id_to_drop <- pt$free > 0
    id_to_drop <- id_to_drop & id_exclude_eq
    id_to_drop <- id_to_drop & id_exclude_exo_var
    id_to_drop <- id_to_drop & id_exclude_end_var
    # User specified parameters
    if (!is.null(must_drop)) {
        id_must_drop <- syntax_to_id(must_drop, ptable = pt)
        id_to_drop <- union(id_to_drop, id_must_drop)
      }
    if (!is.null(must_not_drop)) {
        id_must_not_drop <- syntax_to_id(must_not_drop, ptable = pt)
        id_to_drop <- setdiff(id_to_drop, id_must_not_drop)
      }
    # Determine the sets of changes
    sets_to_gen <- lapply(seq_len(df_change),
                function(x) {
                          utils::combn(which(id_to_drop), x, simplify = FALSE)
                        }
              )
    sets_to_gen <- unlist(sets_to_gen, recursive = FALSE)
    df0 <- lavaan::fitMeasures(sem_out, "df")
    out <- lapply(sets_to_gen, gen_pt_drop, pt = pt, to = model_id,
                  source_df = df0, sem_out = sem_out)

    # Keep tables with expected df only?
    if (keep_correct_df_change) {
        chk1 <- sapply(out, attr, which = "df_actual")
        chk2 <- sapply(out, attr, which = "df_expected")
        out <- out[chk1 == chk2]
      }

    out_names <- sapply(out, function(x) {
        paste("drop:",
              paste(attr(x, "parameters_dropped"), collapse = ";"))
      })
    names(out) <- out_names
    attr(out, "call") <- match.call()
    attr(out, "sem_out") <- sem_out
    class(out) <- c("partables", class(out))
    if (remove_duplicated) {
        out <- unique_models(out)
      }
    out
  }

#' @noRd

gen_pt_drop <- function(x, pt, to, source_df = NA, sem_out) {
    # Function to generate pt
    for (i in x) {
        pt[i, "free"] <- 0
        pt[i, "ustart"] <- 0
        pt[i, "est"] <- 0
      }
    p_to_drop <- sapply(x, function(x)
        paste0(pt[x, "lhs"], pt[x, "op"], pt[x, "rhs"])
      )
    p_to_drop_out <- lapply(x, function(x) {
        c(lhs = pt[x, "lhs"], op = pt[x, "op"], rhs = pt[x, "rhs"])
      })
    suppressWarnings(sem_out_update <- lavaan::update(sem_out,
                                     pt,
                                     do.fit = TRUE,
                                     optim.force.converged = TRUE,
                                     warn = FALSE,
                                     se = "none",
                                     baseline = FALSE,
                                     check.start = FALSE,
                                     check.post = FALSE,
                                     check.vcov = FALSE,
                                     control = list(max.iter = 1)))
    pt_update <- lavaan::parameterTable(sem_out_update)
    attr(pt_update, "parameters_dropped") <- p_to_drop
    attr(pt_update, "parameters_dropped_list") <- p_to_drop_out
    attr(pt_update, "ids_dropped") <- x
    attr(pt_update, "to") <- to
    attr(pt_update, "df_expected") <- unname(source_df) +
                               length(x)
    attr(pt_update, "df_actual") <- unname(lavaan::fitMeasures(sem_out_update,
                                                        fit.measures = "df"))
    pt_update
  }