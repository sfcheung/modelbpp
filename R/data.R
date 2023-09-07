#' A sample dataset based on a path model (for testing)
#'
#' Generated from the following path model, n = 100
#'
#' The model used to generate this dataset:
#'
#' ```
#' x1 ~~ x2
#' x3 ~ x1 + x2
#' x4 ~ x3 + x1 + x2
#' ```
#'
#' @format A data frame with four variables:
#' \describe{
#'  \item{x1}{Predictor}
#'  \item{x2}{Predictor}
#'  \item{x3}{Mediator}
#'  \item{x4}{Outcome}
#' }
"dat_path_model"


#' A sample dataset based on a CFA (for testing)
#'
#' Generated from the following path model, n = 200
#'
#' The model used to generate this dataset:
#'
#' ```
#' f1 =~ x1 + x2 + x3 + x5
#' f2 =~ x3 + x4 + x5 + x6
#' f1 ~~ f2
#' ```
#'
#' @format A data frame with six variables:
#' \describe{
#'  \item{x1}{Indicator}
#'  \item{x2}{Indicator}
#'  \item{x3}{Indicator}
#'  \item{x4}{Indicator}
#'  \item{x5}{Indicator}
#'  \item{x6}{Indicator}
#' }
"dat_cfa"

#' A sample dataset based on a complex path model (for testing)
#'
#' Generated from the following path model, n = 200
#'
#' The model used to generate this dataset:
#'
#' ```
#' y4 ~  x1 + x2 + x3
#' y5 ~  y4 + x1 + x2
#' y6 ~  y4 + y5 + x1 + x2 + x3
#' x1 ~~ x2 + x3
#' x2 ~~ x3
#' ```
#'
#' @format A data frame with six variables:
#' \describe{
#'  \item{x1}{Predictor}
#'  \item{x2}{Predictor}
#'  \item{x3}{Predictor}
#'  \item{y4}{Mediator}
#'  \item{y5}{Mediator}
#'  \item{y6}{Outcome}
#' }
"dat_path_model_p06"


#' A sample dataset based on a serial mediation path model (for
#' testing)
#'
#' Generated from the following path model, n = 100
#'
#' The model used to generate this dataset:
#'
#' ```
#' m1 ~ x
#' m2 ~ m1
#' y  ~ m2
#' ```
#'
#' @format A data frame with four variables:
#' \describe{
#'  \item{x}{Predictor}
#'  \item{m1}{Mediator}
#'  \item{m2}{Mediator}
#'  \item{y}{Outcome}
#' }
"dat_serial_4"

#' A sample dataset based on an SEM model (for testing)
#'
#' Generated from an SEM model, n = 250
#'
#' The model to be fitted:
#'
#' ```
#' f1 =~ x1 + x2 + x3 + x4
#' f2 =~ x5 + x6 + x7 + x8
#' f3 =~ x9 + x10 + x11 + x12
#' f4 =~ x13 + x14 + x15 + x16
#' f3 ~ f1 + f2
#' f4 ~ f3
#' ```
#'
"dat_sem"


#' A sample dataset based on a serial mediation path model
#' with weak paths (for
#' testing)
#'
#' Generated from the following path model, n = 100
#'
#' The model used to generate this dataset:
#'
#' ```
#' m1 ~ x
#' m2 ~ m1 + x
#' y  ~ m2 + m1 + x
#' ```
#'
#' @format A data frame with four variables:
#' \describe{
#'  \item{x}{Predictor}
#'  \item{m1}{Mediator}
#'  \item{m2}{Mediator}
#'  \item{y}{Outcome}
#' }
"dat_serial_4_weak"