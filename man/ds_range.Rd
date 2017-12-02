% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds-describe.R
\name{ds_range}
\alias{ds_range}
\alias{stat_range}
\title{Range}
\usage{
ds_range(x)
}
\arguments{
\item{x}{a numeric vector}
}
\value{
Range of \code{x}
}
\description{
Compute the range of a numeric vector
}
\section{Deprecated Function}{

\code{stat_range()} has been deprecated. Instead use \code{ds_range()}.
}

\examples{
ds_range(mtcars$mpg)
}
\seealso{
\code{\link[base]{range}}
}