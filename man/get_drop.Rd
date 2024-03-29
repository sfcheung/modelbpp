% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_drop.R
\name{get_drop}
\alias{get_drop}
\title{Models That Are More Restricted}
\usage{
get_drop(
  sem_out,
  must_drop = NULL,
  must_not_drop = NULL,
  df_change = 1,
  model_id = NA,
  keep_correct_df_change = TRUE,
  remove_duplicated = TRUE,
  progress = FALSE
)
}
\arguments{
\item{sem_out}{The original model,
which is the output from an
structural equation modeling
function. Currently support
\link[lavaan:lavaan-class]{lavaan::lavaan} objects only.}

\item{must_drop}{A character vector
of parameters, named in
\code{lavaan::lavaan()} style (e.g.,
\code{"y ~ x"}), that must be included.
Default is \code{NULL}.}

\item{must_not_drop}{A character
vector of parameters, named in
\code{\link[lavaan:lavaan]{lavaan::lavaan()}} style (e.g.,
\code{"x1 ~~ x1"}), that must not be
included. Default is \code{NULL}.}

\item{df_change}{How many degrees
of freedom away in the list. All
models with \emph{df} change less than
or equal to this number will be
included, taking into account
requirements set by other arguments.
Default is 1.}

\item{model_id}{The identification
number of the starting model.
Default is \code{NA}, no identification
number.}

\item{keep_correct_df_change}{Keep
only models with actual \emph{df} change
equal to expected \emph{df} change.}

\item{remove_duplicated}{If \code{TRUE},
the default, duplicated models are
removed.}

\item{progress}{Whether a progress
bar will be displayed, implemented
by the \code{pbapply} package. Default
is \code{FALSE}.}
}
\value{
An object of the class
\code{partables}, a named list of parameter
tables, each of them to be used by
\code{\link[lavaan:lavaan]{lavaan::lavaan()}} or \code{\link[=update]{update()}}
for fitting a model with the added
parameters.
}
\description{
Generate a list of
models with one or more free parameter
dropped (fixed to zero).
}
\details{
It generates a list of models
with one or more free parameters
dropped, that is, fixed to zero
(with degrees of freedom,
\emph{df}, increases by one or more).

All free parameters are included in
the pool of candidates, except for
those explicitly requested to be
kept.

The models will be checked by \code{lavaan}
to make sure that the increase in
model degrees of freedom is of the
expected value.

This function is called by
\code{\link[=model_set]{model_set()}} and usually users do
not need to call it. It is exported
for advanced users.
}
\examples{

library(lavaan)

dat <- dat_path_model
mod <-
"
x3 ~ a*x1 + b*x2
x4 ~ a*x1 + x2
ab := a*b
"

fit <- sem(mod, dat_path_model, fixed.x = TRUE)
mod_to_drop <- get_drop(fit)
mod_to_drop

}
\seealso{
\code{\link[=print.partables]{print.partables()}}
}
\author{
Shu Fai Cheung \url{https://orcid.org/0000-0002-9871-9448}
}
