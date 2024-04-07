#' @title Measurement Invariance Models
#'
#' @description Generate metric and
#' scalar invariance models and their
#' partial invariance versions.
#'
#' @details
#' This a helper function to generate,
#' based on a multigroup confirmatory
#' factor analysis (CFA) model with
#' no between-group equality constraints,
#' the following models:
#'
#' - A metric invariance model (loadings
#' constrained to be equal across
#' groups).
#'
#' - A scalar invariance model (intercepts
#' and loadings constrained to be equal
#' across groups).
#'
#' - Partial invariance versions of the
#' previous two models, such as a
#' model with the loadings of all items,
#' except for one, constrained to be
#' equal across groups.
#'
#' The models generated can then be used
#' in `model_set()` to compute BPPs.
#'
#' ## Requirements
#'
#' The model used as the input needs to
#' be fitted with no between group
#' constrains, that is, it is a
#' configural invariance model. Although
#' not a must, it is advised to use
#' the default way to identify each
#' factor (that is, fixing a loading
#' to one).
#'
#' ## Implementation
#'
#' This function simply use the
#' `group.partial` and `group.equal`
#' argument of [lavaan::cfa()] to
#' generate the models.
#'
#' @param cfa_out The output of
#' [lavaan::cfa()].
#'
#' @param max_free The maximum number of
#' constraints to be released when
#' generating the partial invariance
#' models. For example, if set to 1,
#' then only the partial metric invariance
#' model only has at most one item allowed
#' to have different loadings across
#' group. Default is 1. If set to zero,
#' then no partial invariance models
#' will be generated.
#'
#' @param metric Logical. If `TRUE`,
#' the default, then metric invariance
#' model and its partial invariance
#' versions are generated.
#'
#' @param scalar Logical. If `TRUE`,
#' the default, then scalar invariance
#' model and its partial invariance
#' versions are generated.
#'
#' @param progress Logical. If `TRUE`,
#' the default, progress bars will be
#' displayed when fitting partial
#' invariance models.
#'
#' @return A list of [lavaan::cfa()]
#' output. The names are automatically
#' generated to indicate whether a model
#' is configural, metric, or scalar
#' invariance, or the item(s) without
#' between-group constraints on the
#' loadings (for partial metric
#' invariance) or intercepts (for
#' partial scalar invariance).
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [model_set()]
#'
#' @examples
#'
#' library(lavaan)
#' data(HolzingerSwineford1939)
#' # For illustration, only two factors are used
#' HSmod <-
#' "
#' spatial =~ x1 + x2 + x3
#' verbal =~ x4 + x5 + x6
#' "
#' fit_config <- cfa(model = HSmod,
#'                   data = HolzingerSwineford1939,
#'                   group = "school")
#' fit_mi <- measurement_invariance_models(fit_config)
#' names(fit_mi)
#' # Need to add 'skip_check_sem_out = TRUE' to use multigroup models.
#' out <- model_set(sem_out = fit_mi,
#'                  skip_check_sem_out = TRUE,
#'                  progress = FALSE,
#'                  parallel = FALSE)
#' print(out)
#'
#' @export
#'

measurement_invariance_models <- function(cfa_out,
                                          max_free = 1,
                                          metric = TRUE,
                                          scalar = TRUE,
                                          progress = TRUE) {
    pt <- lavaan::parameterTable(cfa_out)
    if (!metric && !scalar) {
        return(list(config = cfa_out))
      }
    if (lavaan::lavInspect(cfa_out, "ngroups") == 1) {
        stop("Cannot be used on single-group models.")
      }
    opt <- lavaan::lavInspect(cfa_out, "options")
    if (length(opt$group.equal) != 0) {
        stop("Cannot be used models with 'group.equal' set.")
      }
    if (length(opt$group.partial) != 0) {
        stop("Cannot be used models with 'group.partial' set.")
      }
    if (metric) {
        fit_metric <- lavaan::update(cfa_out,
                                     model = pt,
                                     group.equal = "loadings")
        fit_pi_metric <- partial_invariance(fit_metric,
                                            pars = "loadings",
                                            max_free = max_free,
                                            keep_original = FALSE,
                                            progress = progress)
        fit_metric <- list(metric = fit_metric)
      } else {
        fit_metric <- NULL
        fit_pi_metric <- NULL
      }
    if (scalar) {
        fit_scalar <- lavaan::update(cfa_out,
                                     model = pt,
                                     group.equal = c("loadings", "intercepts"))
        fit_pi_scalar <- partial_invariance(fit_scalar,
                                            pars = "intercepts",
                                            max_free = max_free,
                                            keep_original = FALSE,
                                            progress = progress)
        fit_scalar <- list(scalar = fit_scalar)
      } else {
        fit_scalar <- NULL
        fit_pi_scalar <- NULL
      }
    out <- c(list(config = cfa_out),
                  fit_metric,
                  fit_scalar,
                  fit_pi_metric,
                  fit_pi_scalar)
    out
  }

#' @noRd

partial_invariance <- function(cfa_out,
                                pars = c("loadings", "intercepts"),
                                max_free = 1,
                                keep_original = TRUE,
                                progress = TRUE) {
    if (max_free < 1) {
        return(NULL)
      }
    pars <- match.arg(pars)
    opt <- lavaan::lavInspect(cfa_out, "options")
    if ((pars == "loadings") &&
        opt$group.equal != "loadings") {
        stop("To use pars = 'loadings', group.equal must be 'loadings'.")
      }
    chk <- opt$group.equal
    if ((pars == "intercepts") &&
        !setequal(opt$group.equal, c("loadings", "intercepts"))) {
        stop("To use pars = 'intercepts', group.equal must be c('loadings', 'intecepts').")
      }
    free_pars <- switch(pars,
                        loadings = get_free_loadings(cfa_out),
                        intercepts = get_free_intercepts(cfa_out))
    to_release <- lapply(seq_len(max_free), function(xx) {
          utils::combn(free_pars, m = xx, simplify = FALSE)
        })
    to_release <- unlist(to_release, recursive = FALSE)
    gp_eq <- switch(pars,
                    loadings = c("loadings"),
                    intercepts = c("loadings", "intercepts"))
    pt <- lavaan::parameterTable(cfa_out)
    fit_i <- function(x) {
            sem_out_update <- eval(lavaan::update(cfa_out,
                                             model = pt,
                                             group.equal = gp_eq,
                                             group.partial = x))
            sem_out_update
          }
    if (progress) {
        cat("\nFitting", length(to_release),
            "partial",
            switch(pars, loadings = "metric",
                         intercepts = "scalar"),
            "invariance models:\n")
        pt_out <- pbapply::pblapply(to_release, fit_i)
      } else {
        pt_out <- lapply(to_release, fit_i)
      }
    pt_out_names <- sapply(to_release, paste, collapse = ";")
    names(pt_out) <- pt_out_names
    if (keep_original) {
        if (pars == "loadings") {
            pt_out$metric <- cfa_out
          }
        if (pars == "intercepts") {
            pt_out$scalar <- cfa_out
          }
      }
    pt_out
  }

#' @noRd

get_free_loadings <- function(cfa_out) {
    pt <- lavaan::parameterTable(cfa_out)
    loadings <- pt[(pt$op == "=~") & (pt$free != 0) & (pt$group == 1), ]
    out <- sapply(split(loadings, seq_len(nrow(loadings))), function(xx) {
                paste0(xx$lhs, xx$op, xx$rhs)
              })
    out
  }

#' @noRd

get_free_intercepts <- function(cfa_out) {
    pt <- lavaan::parameterTable(cfa_out)
    intercepts <- pt[(pt$op == "~1") & (pt$free != 0) & (pt$group == 1), ]
    out <- sapply(split(intercepts, seq_len(nrow(intercepts))), function(xx) {
                paste0(xx$lhs, xx$op)
              })
    out
  }
