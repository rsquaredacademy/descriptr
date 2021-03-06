% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds-describe.R
\name{ds_measures_symmetry}
\alias{ds_measures_symmetry}
\title{Measures of symmetry}
\usage{
ds_measures_symmetry(data, ..., decimals = 2)
}
\arguments{
\item{data}{A \code{data.frame} or \code{tibble}.}

\item{...}{Column(s) in \code{data}.}

\item{decimals}{An option to specify the exact number of decimal places to use. The default number of decimal places is 2.}
}
\description{
Returns the measures of symmetry such as skewness and kurtosis.
}
\examples{
# single column
ds_measures_symmetry(mtcarz, mpg)

# multiple columns
ds_measures_symmetry(mtcarz, mpg, disp)

# all columns
ds_measures_symmetry(mtcarz)

# vector
ds_measures_symmetry(mtcarz$mpg)

# vectors of different length
disp <- mtcarz$disp[1:10]
ds_measures_symmetry(mtcarz$mpg, disp)

# decimal places
ds_measures_symmetry(mtcarz, disp, hp, decimals = 3)

}
