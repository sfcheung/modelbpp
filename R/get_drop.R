#' @title Models That Are More Restricted
#'
#' @description Generate the list of
#' models with one less free parameter
#' or constraint (and one more
#' degree of freedom).
#'
#' @details Generate the list of models
#' with one less free parameter
#' or constraint (and one more degree
#' of freedom).
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

get_drop <- function(sem_out,
                     must_drop = NULL,
                     must_not_drop = NULL,
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
    # Exclude all parameters already constrained to be equal
    i_eq <- pt$op == "=="
    id_eq <- c(pt$lhs[i_eq], pt$rhs[i_eq])
    id_eq <- unique(id_eq)
    id_exclude_eq <- pt$plabel %in% id_eq
    # Exclude the variances of exogenous variables
    tmp1 <- (pt$exo > 0) & (pt$op == "~~")
    tmp2 <- (pt$lhs == pt$rhs)
    id_exclude_exo_var <- tmp1 & tmp2
    # Exclude error variances of endogenous variables
    tmp1 <- (pt$exo == 0) & (pt$op == "~~")
    tmp2 <- (pt$lhs == pt$rhs)
    id_exclude_end_var <- tmp1 & tmp2
    # Determine the candidate lists
    id_to_drop <- pt$free > 0
    id_to_drop <- id_to_drop & !id_exclude_eq
    id_to_drop <- id_to_drop & !id_exclude_exo_var
    id_to_drop <- id_to_drop & !id_exclude_end_var
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
                  source_df = df0)
    out_names <- sapply(out, function(x) {
        paste("drop:",
              paste(attr(x, "parameters_dropped"), collapse = ";"))
      })
    names(out) <- out_names
    out
  }

#' @noRd

gen_pt_drop <- function(x, pt, to, source_df = NA) {
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
    attr(pt, "parameters_dropped") <- p_to_drop
    attr(pt, "parameters_dropped_list") <- p_to_drop_out
    attr(pt, "ids_dropped") <- x
    attr(pt, "to") <- to
    attr(pt, "df_expected") <- source_df + 1
    pt
  }