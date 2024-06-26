% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/c_partables.R
\name{c.partables}
\alias{c.partables}
\alias{c.model_set}
\alias{partables_drop}
\alias{manipulate_partables}
\title{Manipulate Parameter Tables}
\usage{
\method{c}{partables}(...)

\method{c}{model_set}(...)

partables_drop(x, model_names = NULL)
}
\arguments{
\item{...}{An arbitrary number of
objects. All invalid objects (see
details) will be discarded. If an
object is named and is not
\code{partables} object, its name will be
used.}

\item{x}{A \code{partables}-class object.}

\item{model_names}{A character vector
of the names of models in a
\code{partables}-class object.}
}
\value{
A \code{partables}-class objects with
all the objects supplied combined
together. If an object is
a \code{lavaan}-class object, its
parameter table will be retrieved
by \code{\link[lavaan:parTable]{lavaan::parameterTable()}}.
If an object is a \code{model_set}-class
object, the stored \code{partables}-class
object will be retrieved.
}
\description{
Functions to manipulate
a \code{partables}-class object
}
\details{
The \code{partables}-class objects have
a \code{c()} method that can be used to
combine parameter tables.
Each object must be

a. a \code{partables}-class object,
b. a \code{model_set}-class object,
c. a \code{lavaan}-class object, or
d. a parameter table of
the class \code{lavaan.data.frame()},
usually generated by
\code{\link[lavaan:parTable]{lavaan::parameterTable()}}.
Other objects will be discarded.

Names will be used when combining
objects. If two objects have the same
names, then only the first one will be
retained. No warning message will be
issued. Users are encouraged to
explicitly name all objects carefully.

Note that, to invoke this method,
the first object must be a
\code{partables} object.

The \code{model_set} class also has a
\code{c}-method. It
will replace the first object by the stored
partables and then call the \code{c}-method of \code{partables} objects.

The function \code{\link[=partables_drop]{partables_drop()}} is
for dropping models from a
\code{partables}-class object.
}
\examples{

library(lavaan)

moda <-
"
x3 ~ a*x1 + b*x2
x4 ~ a*x1
ab := a*b
"

fita <- sem(moda, dat_path_model, fixed.x = TRUE)

outa <- model_set(fita,
                  progress = FALSE,
                  parallel = FALSE)

modb <-
"
x3 ~ a*x1 + b*x2
x4 ~ a*x2
ab := a*b
"

fitb <- sem(modb, dat_path_model, fixed.x = TRUE)

outb <- model_set(fitb,
                  progress = FALSE,
                  parallel = FALSE)

mod2 <-
"
x2 ~ 0*x3 + 0*x4
x1 ~ 0*x3
"
fit2 <- sem(mod2, dat_path_model)

mod3 <-
"
x2 ~ x3 + 0*x4
x1 ~ x3
"
fit3 <- sem(mod3, dat_path_model)

out <- c(outa$models, user2 = fit2, outb$models, user3 = fit3)
out

out2 <- c(outa, user2 = fit2, outb$models, user3 = fit3)
out2

out3 <- c(outa, user2 = fit2, outb, user3 = fit3)
out3

}
\author{
Shu Fai Cheung \url{https://orcid.org/0000-0002-9871-9448}
}
