% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.partables.R
\name{print.partables}
\alias{print.partables}
\title{Print a \code{partables}-Class Object}
\usage{
\method{print}{partables}(x, max_tables = 10, ...)
}
\arguments{
\item{x}{A \code{partables}-class object.}

\item{max_tables}{The maximum number
of models to be printed.
Default is 10.}

\item{...}{Optional arguments.
Ignored.}
}
\value{
\code{x} is returned invisibly.
Called for its side effect.
}
\description{
Print the content of
a \code{partables}-class object.
}
\details{
The print method for the output
of \code{\link[=gen_models]{gen_models()}}, \code{\link[=get_add]{get_add()}},
and \code{\link[=get_drop]{get_drop()}}.
}
\examples{

library(lavaan)
dat <- dat_path_model
mod <-
"
x3 ~ a*x1 + b*x2
x4 ~ a*x1
ab := a*b
"
fit <- sem(mod, dat_path_model, fixed.x = TRUE)
mod_to_add <- get_add(fit)
mod_to_add
print(mod_to_add, max_tables = 1)
mod_to_drop <- get_drop(fit)
mod_to_drop
print(mod_to_drop, max_tables = 1)

}
\seealso{
\code{\link[=gen_models]{gen_models()}}, \code{\link[=get_add]{get_add()}},
and \code{\link[=get_drop]{get_drop()}}.
}
\author{
Shu Fai Cheung \url{https://orcid.org/0000-0002-9871-9448}
}
