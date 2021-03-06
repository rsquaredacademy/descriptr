% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds-describe.R
\name{ds_css}
\alias{ds_css}
\title{Corrected Sum of Squares}
\usage{
ds_css(data, x = NULL)
}
\arguments{
\item{data}{A numeric vector or \code{data.frame}.}

\item{x}{Column in \code{data}.}
}
\description{
Compute the corrected sum of squares
}
\examples{
# vector
ds_css(mtcars$mpg)

# data.frame
ds_css(mtcars, mpg)

}
