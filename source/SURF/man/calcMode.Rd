\name{calcMode}
\alias{calcMode}
\title{Find the Modal Value of a Vector}
\description{
  This function will find the most commonly occurring value (i.e., the
  mode) in a vector.
}
\usage{
calcMode(x, discrete = TRUE)
}
\arguments{
  \item{x}{
    A vector for which to find the mode.
  }
  \item{discrete}{
    A logical switch indicate whether \code{x} is a categorical (i.e.,
    \code{discrete = TRUE}) or continuous (i.e., \code{discrete =
      FALSE}) variable.
  }
}
\details{
  In the event of multiple modes, ties are broken randomly. When
  \code{x} is numeric, the return value will be a length one numeric
  vector, otherwise, the return value will be a length one character
  vector.

  When \code{discrete = FALSE} the modal value is estimated via kernel
  density estimation by applying \code{stats::density} to \code{x}.
}
\value{
  The modal value in \code{x}.
}
\author{
  Kyle M. Lang
}
\examples{
## Find mode of an integer vector:
x1 <- sample(c(1 : 5), 100, TRUE)
calcMode(x1)

## Find mode of a factor:
x2 <- factor(sample(c(1 : 5), 100, TRUE))
calcMode(x2)

## Find mode of a character vector:
x3 <- sample(letters[1 : 5], 100, TRUE)
calcMode(x3)

## Find the mode of a continuous variable:
x4 <- rnorm(1000, 5, 2.5)
calcMode(x4, discrete = FALSE)
}