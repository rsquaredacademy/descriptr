% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds-describe.R
\name{ds_range}
\alias{ds_range}
\title{Range}
\usage{
ds_range(data, x = NULL)
}
\arguments{
\item{data}{A numeric vector or \code{data.frame}.}

\item{x}{Column in \code{data}.}
}
\value{
Range of \code{x}
}
\description{
Compute the range of a numeric vector
}
\examples{
# vector
ds_range(mtcars$mpg)

# data.frame
ds_range(mtcars, mpg)

}
\seealso{
\code{\link[base]{range}}
}
