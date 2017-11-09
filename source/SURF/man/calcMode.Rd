\name{calcMode}
\alias{calcMode}
\title{Find the Modal Value of a Vector}
\description{
  This function will find the most commonly occuring value (i.e., the
  mode) in a vector.
}
\usage{
calcMode(x)
}
\arguments{
  \item{x}{
    A vector for which to find the mode.
  }
}
\details{
  In the event of multiple modes, ties are broken randomly. When
  \code{x} is numeric, the return value will be a length one numeric
  vector, otherwise, the return value will be a length one character
  vector.
}
\value{
  The modal value in \code{x}.
}
\author{
  Kyle M. Lang
}
\examples{
## Find mode of a numeric vector:
x1 <- sample(c(1 : 5), 100, FALSE)
calcMode(x1)

## Find mode of a factor:
x2 <- factor(sample(c(1 : 5), 100, FALSE))
calcMode(x2)

## Find mode of a character vector:
x3 <- sample(letters[1 : 5], 100, FALSE)
calcMode(x3)
}