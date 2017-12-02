% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds-describe.R
\name{ds_cvar}
\alias{ds_cvar}
\alias{stat_cvar}
\title{Coefficient of Variation}
\usage{
ds_cvar(x)
}
\arguments{
\item{x}{a numeric vector containing the values whose mode is to be computed}
}
\description{
Compute the coefficient of variation
}
\details{
Any NA values are stripped from \code{x} before computation
takes place.
}
\section{Deprecated Function}{

\code{stat_cvar()} has been deprecated. Instead use \code{ds_cvar()}.
}

\examples{
ds_cvar(mtcars$mpg)
}
\seealso{
\code{\link[stats]{var}} \code{\link[stats]{sd}}
}