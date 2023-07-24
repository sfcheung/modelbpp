#' @title Models That Are Less Restricted
#'
#' @description Generate the list of
#' models with one more free
#' parameter (and one less degree of
#' freedom).
#'
#' @details Generate the list of models
#' with one more free parameter (and one
#' less degree of freedom, *df*).
#'
#' @param sem_out The output from an
#' structural equation modeling
#' function. Currently support
#' [lavaan::lavaan-class] only. Usually
#' the one used in [get_add()] or
#' [get_drop()] to generate the list of
#' models.
#'
#' @param must_add A character vector
#' of parameters, named in
#' [lavaan::lavaan()] style (e.g.,
#' `"y ~ x"`), that must be added.
#' Default is `NULL``.
#'
#' @param must_not_add A character
#' vector of parameters, named in
#' [lavaan::lavaan()] style (e.g.,
#' `"x1 ~~ x1"`), that must not be
#' added. Default is `NULL`.
#'
#' @param remove_constraints Whether
#' equality constraints will be
#' removed. Default is ``TRUE`.
#'
#' @param exclude_error_cov Exclude
#' error covariances of indicators.
#' Default is `TRUE`.
#'
#' @param df_change How many degrees
#' of freedom (*df*) away in the list.
#' All models with *df* change less than
#' or equal to this number will be
#' included. Default is 1.
#'
#' @param model_id The identification
#' number of the starting model.
#' Default is `NA`, no identification
#' number.
#'
#' @return A named list of parameter
#' tables to be used by
#' [lavaan::lavaan()] or [update()]
#' for fitting a model with the added
#' parameters.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#' # To Do
#'
#' @export

get_add <- function(sem_out,
                     must_add = NULL,
                     must_not_add = NULL,
                     remove_constraints = TRUE,
                     exclude_error_cov = TRUE,
                     df_change = 1,
                     model_id = NA
                    ) {
    if (missing(sem_out)) stop("sem_out is not supplied.")
    if (!inherits(sem_out, "lavaan")) {
        stop("sem_out is not a lavaan-class object.")
      }
    pt <- lavaan::parameterTable(sem_out)

    # Remove all user-defined parameters
    pt <- pt[pt$op != ":=", ]

    # Get the MI table
    mt <- lavaan::modificationIndices(sem_out,
                    standardized = FALSE,
                    power = FALSE)

    # Remove those already in the parameter tables
    mt1 <- mt_exclude_existing_pars(mt = mt, pt = pt)

    # Remove those convert an IV to a DV
    mt1 <- mt_exclude_reversed(mt = mt1, pt = pt)

    # Identify parameters to be added
    mt1_op <- paste0(mt1$lhs, mt1$op, mt1$rhs)
    mt1_op2 <- lor_to_list(mt1)

    # Add must_add
    if (!is.null(must_add)) {
        mt1_must_add <- syntax_to_add_list(must_add)
        mt1_op2 <- union(mt1_op2, mt1_must_add)
      }

    # Remove must_not_add
    if (!is.null(must_not_add)) {
        mt1_must_not_add <- syntax_to_add_list(must_not_add)
        mt1_must_not_add1 <- add_list_duplicate_cov(mt1_must_not_add)
        mt1_op2 <- setdiff(mt1_op2, mt1_must_not_add1)
      }
    # Remove duplicated covariances
    mt1_op2 <- add_list_clean_duplicated_cov(mt1_op2)

    # Remove error covariances between indicators
    if (exclude_error_cov) {
        mt1_op2 <- mt_remove_error_cov(mt_list = mt1_op2,
                                       sem_out = sem_out)
      }

    # Identify parameters constrained to be equal by labels
    i_eq <- pt$op == "=="
    row_eq <- which(i_eq)
    row_eq_op2 <- lor_to_list(pt[row_eq, ])

    # Determine the sets of changes
    sets_to_gen <- lapply(seq_len(df_change),
                function(x) {
                    utils::combn(c(mt1_op, row_eq), x, simplify = FALSE)
                  }
              )
    sets_to_gen <- unlist(sets_to_gen, recursive = FALSE)
    sets_to_gen2 <- lapply(seq_len(df_change),
                function(x) {
                    utils::combn(c(mt1_op2, row_eq_op2), x, simplify = FALSE)
                  }
              )
    sets_to_gen2 <- unlist(sets_to_gen2, recursive = FALSE)

    # Clean up inadmissible modification
    sets_to_gen2_ok <- lapply(sets_to_gen2, function(x) {
        i <- length(x)
        x_to_remove <- rep(FALSE, i)
        x_lr <- lapply(x, function(y) y[c(1, 3)])
        for (j in seq_len(i)) {
            if (j > 1) {
                for (k in seq_len(j - 1)) {
                  if (all(x_lr[[j]] == x_lr[[k]]) |
                      all(x_lr[[j]] == x_lr[[k]][c(2, 1)])) {
                      x_to_remove[j] <- TRUE
                    }
                }
              }
          }
        x[!x_to_remove]
      })
    out <- lapply(sets_to_gen2_ok, gen_pt_add, pt = pt, sem_out = sem_out,
                  from = model_id)
    out_names <- sapply(out, function(x) {
        out <- paste0(c(attr(x, "parameters_added"),
              attr(x, "constraints_released")), collapse = ";")
        paste("add:", out)
      })
    names(out) <- out_names
    out
  }

#' @noRd

gen_pt_add <- function(x, pt, sem_out, from = NA) {
    # Generate pt
    # Collects free parameters to add
    x_free <- x[sapply(x,
                       function(y) {y["op"] != "=="})]
    # Collects constraints to release
    x_constr <- x[sapply(x,
                         function(y) {y["op"] == "=="})]
    # Form constraint in parameter names (lhs-op-rhs)
    x_constr_pars <- constr_pars(constr = x_constr,
                                 pt = pt)
    # Loop and release constraints
    if (length(x_constr) > 0) {
        pt <- release_constr(x_constr, pt = pt)
        x_constr_labels <- paste0(sapply(x_constr,
                                         paste,
                                         collapse = ""),
                                  collapse = ";")
        x_constr_str <- paste(x_constr_pars, collapse = ";")
        x_constr_out <- lapply(x_constr,
                               constr_to_lor,
                               pt = pt)
      } else {
        x_constr_labels <- NULL
        x_constr_str <- NULL
        x_constr_out <- NULL
      }
    # Add free parameters
    if (length(x_free) > 0) {
        x_free_str <- par_names(pars_list = x_free)
        sem_out_update <- lavaan::update(sem_out,
                                         pt,
                                         add = x_free_str,
                                         do.fit = FALSE,
                                         evaluate = FALSE)
        pt_update <- data.frame(sem_out_update$model)
      } else {
        x_free_str <- NULL
        pt_update <- pt
      }
    attr(pt_update, "parameters_added") <- x_free_str
    attr(pt_update, "parameters_added_list") <- x_free
    attr(pt_update, "constraints_released_labels") <- x_constr_labels
    attr(pt_update, "constraints_released") <- x_constr_str
    attr(pt_update, "constraints_released_list") <- x_constr_out
    attr(pt_update, "from") <- from
    attr(pt_update, "df_expected") <- lavaan::fitMeasures(sem_out, "df") -
                                      length(x)
    pt_update
  }
