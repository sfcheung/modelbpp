#' @title Bayesian Posterior Probabilities
#' of Neighboring Models
#'
#' @description Identify neighboring
#' models, fit them, and return the
#' Bayesian posterior probabilities.
#'
#' @details It computes the Bayesian
#' posterior probabilities of a set
#' of models by the method presented
#' in Wu, Cheung, and Leung (2020).
#'
#' First, a list of model is identified
#' based on user-specified criteria.
#' By default, models differ from a fitted
#' model by one degree of freedom,
#' the 1-df-away *neighboring* models,
#' will be found using [get_add()]
#' and [get_drop].
#'
#' Second, these models will be fitted
#' to the sample dataset, and their
#' BICs will be computed.
#'
#' Third, their Bayesian posterior
#' probabilities will be computed
#' using their BICs. Equal prior
#' probabilities for all the models
#' being fitted will be assumed in
#' the current version.
#'
#' The results can then be printed,
#' with the models sorted by descending
#' order of Bayesian posterior
#' probabilities.
#'
#' @param sem_out The output from an
#' SEM function. Currently support
#' [lavaan::lavaan-class] only.
#' Usually the one used [get_add()] or
#' [get_drop()] to generate the
#' list of models.
#'
#' @param prior_sem_out The prior of the
#' model fitted in `sem_out`. Default
#' is `NULL`, and all models will have
#' equal prior probabilities.
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
#' @param df_change_add How many degrees
#' of freedom (*df*) away in the list
#' when adding parameters or releasing
#' constraints. All models with *df*
#' change less than or equal to this
#' number will be included. Default is
#' 1.
#'
#' @param df_change_drop How many *df*
#' away in the list when removing
#' parameters. All models with *df*
#' change less than or equal to this
#' number will be included. Default is
#' 1.
#'
#' @param parallel If `TRUE`, parallel
#' processing will be used to fit the
#' models. Default is `FALSE`.
#'
#' @param ncores Numeric. The number of
#' CPU cores to be used if `parallel`
#' is `TRUE`.
#'
#' @param make_cluster_args A list of
#' named arguments to be passed to
#' `parallel::makeCluster()`. Used by
#' advanced users to configure the
#' cluster if `parallel` is `TRUE`.
#' Default is `list()`.
#'
#' @param progress Whether a progress
#' bar will be displayed, implemented
#' by the `pbapply` package. Default
#' is `TRUE`.
#'
#' @param verbose Whether additional
#' messages will be displayed, such
#' as the expected processing time.
#' Default is `TRUE`.
#'
#' @return An object of the class
#' `model_set`, a list with the following
#' elements:
#'
#' * `model`: A named list of parameter
#'    tables. Each represent the models
#'    identified.
#'
#' * `bic`: A numeric vector, of the
#'    same length as `model`. The BIC of
#'    each model.
#'
#' * `postprob`: A numeric vector, of
#'    the same length as `model`. The
#'    posterior probability of each
#'    model.
#'
#' * `fit`: A named list of
#'    [lavaan::lavaan()] output objects
#'    or [update()] for fitting a model
#'    with the added parameters, of the
#'    same length as `model`.
#'
#' * `change`: A numeric vector, of the
#'    same length as `model`. The change
#'    in model df for each fit. A
#'    positive number denotes one less
#'    free parameter. A negative number
#'    denotes one more free parameter or
#'    one less constraint.
#'
#' * `converged`: A named vector of
#'    boolean values, of the same length
#'    as `model`. Indicates whether each
#'    fit converged or not.
#'
#' * `post_check`: A named vector of
#'    boolean values, of the same length
#'    as `model`. Indicates whether the
#'    solution of each fit is admissible
#'    or not. Checked by
#'    [lavaan::lavInspect()].
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @references
#' Wu, H., Cheung, S. F., & Leung, S. O.
#' (2020). Simple use of BIC to assess
#' model selection uncertainty: An
#' illustration using mediation and
#' moderation models.
#' *Multivariate Behavioral Research*,
#' *55*(1), 1--16.
#' \doi{10.1080/00273171.2019.1574546}
#'
#' @seealso [print.model_set()]
#'
#' @examples
#'
#' library(lavaan)
#'
#' dat <- dat_path_model
#'
#' mod <-
#' "
#' x3 ~ a*x1 + b*x2
#' x4 ~ a*x1
#' ab := a*b
#' "
#'
#' fit <- sem(mod, dat_path_model, fixed.x = TRUE)
#'
#' out <- model_set(fit)
#' out
#'
#' @export

model_set <- function(sem_out,
                      prior_sem_out = NULL,
                      must_add = NULL,
                      must_not_add = NULL,
                      must_drop = NULL,
                      must_not_drop = NULL,
                      remove_constraints = TRUE,
                      exclude_error_cov = TRUE,
                      df_change_add = 1,
                      df_change_drop = 1,
                      parallel = FALSE,
                      ncores = max(parallel::detectCores(logical = FALSE) - 1, 1),
                      make_cluster_args = list(),
                      progress = TRUE,
                      verbose = TRUE) {
  if (missing(sem_out)) stop("sem_out is not supplied.")
  if (!inherits(sem_out, "lavaan")) {
      stop("sem_out is not a lavaan-class object.")
    }
  mod_to_add <- get_add(sem_out,
                        must_add = must_add,
                        must_not_add = must_not_add,
                        remove_constraints = remove_constraints,
                        exclude_error_cov = exclude_error_cov,
                        df_change = df_change_add)
  mod_to_drop <- get_drop(sem_out,
                          must_drop = must_drop,
                          must_not_drop = must_not_drop,
                          df_change = df_change_drop)
  pt0 <- lavaan::parameterTable(sem_out)
  mod_to_fit <- c(mod_to_add, mod_to_drop, list(`original` = pt0))
  out <- fit_many(model_list = mod_to_fit,
                  sem_out = sem_out,
                  parallel = parallel,
                  ncores = ncores,
                  make_cluster_args = make_cluster_args,
                  progress = progress,
                  verbose = verbose)
  out$model <- mod_to_fit
  bic_list <- sapply(out$fit,
        function(x) as.numeric(lavaan::fitMeasures(x, "bic")))
  out$bic <- bic_list
  if (!is.null(prior_sem_out)) {
      p <- length(out$bic)
      i_original <- which(names(out$model) == "original")
      prior_tmp <- rep((1 - prior_sem_out) / (p - 1), p)
      prior_tmp[i_original] <- prior_sem_out
      out$prior <- prior_tmp
    } else {
      # Assume unbiased priors for all models
      out$prior <- rep(1 / length(out$bic), length(out$bic))
    }
  out$postprob <- bpp(bic = out$bic,
                      prior = out$prior)
  out$model_set_call <- match.call()
  class(out) <- c("model_set", class(out))
  out
}

#' @title BIC Posterior Probabilities
#' @noRd

bpp <- function(bic,
                prior = NULL) {
    k <- length(bic)
    if (is.null(prior)) {
        prior <- rep(1 / k, k)
      }
    di <- exp(-.5 * (bic - bic[1])) * prior
    out <- di / sum(di)
    out
  }