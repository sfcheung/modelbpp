#' @title A Sample Dataset Based on a
#'  Path Model (For Testing)
#'
#' @description Generated from the
#'  a path model (n = 100).
#'
#' @details  The model used to generate
#' this dataset:
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


#' @title A Sample Dataset Based On a
#' Confirmatory Factor Analysis Model
#' (For Testing)
#'
#' @description Generated from
#' a confirmatory factor analysis
#' model (n = 200).
#'
#' @details The model used to generate
#' this dataset:
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

#' @title A Sample Dataset Based On a
#' Complex Path Model (For Testing)
#'
#' @description Generated from
#' a complex path model (n = 200).
#'
#' @details The model used to generate
#' this dataset:
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

#' @title A Sample Dataset Based On a
#' Serial Mediation Model (For Testing)
#'
#' @description Generated from
#' a serial mediation model (n = 100).
#'
#' @details The model used to generate
#' this dataset:
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

#' @title A Sample Dataset Based On a
#' Structural Model (For Testing)
#'
#' @description Generated from
#' a structural model with
#' latent variables (n = 250).
#'
#' @details The model to be fitted:
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

#' @title A Sample Dataset Based On a
#' Serial Mediation Model
#' With Weak Paths (For Testing)
#'
#' @description Generated from
#' a serial mediation model (n = 100).
#'
#' @details The model to be fitted:
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