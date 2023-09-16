#' @title Minimum Prior
#'
#' @description Find the minimum prior
#' probability required to achieve
#' the desired BIC posterior
#' probability.
#'
#' @details
#' It assumes that all models other than
#' the original model have the same prior
#' probabilities.
#'
#' This function is called by
#' [model_set()] or [print.model_set()]
#' and usually users do
#' not need to call it. It is exported
#' for advanced users.
#'
#' @param bic A named vector of BIC values
#' for a set of models.
#'
#' @param bpp_target A value from zero
#' to 1. The desired BIC posterior
#' probability.
#'
#' @param target_name The name of the
#' original model, as appeared in the
#' names of `bic`.
#'
#' @return A scalar. The required prior
#' probability.
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
#' @seealso [model_set()] and
#'  [print.model_set()]
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
#' min_prior(out$bic, bpp_target = .8)
#'
#' @export

min_prior <- function(bic,
                      bpp_target,
                      target_name = "original") {
    e <- exp(-.5 * (bic - bic[target_name]))
    i <- which(names(bic) == target_name)
    e1 <- e[i]
    estar <- sum(e[-i])
    k <- length(e)
    p1 <- (1 / (k - 1)) * bpp_target * estar /
            (e1 * (1 - bpp_target) + bpp_target * estar / (k - 1))
    unname(p1)
  }