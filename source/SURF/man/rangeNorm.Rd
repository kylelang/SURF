\name{rangeNorm}
\alias{rangeNorm}
\title{Range Normalize a Vector}
\description{
  This function will standardize a vector so that all transformed values
  exist between user-defined minimum and maximum values.
}
\usage{
rangeNorm(x, oldMin = min(x), oldMax = max(x), newMin = 0.0, newMax = 1.0)
}
\arguments{
  \item{x}{
    A numeric vector to be standardized.
  }
  \item{oldMin}{
    An optional real number giving the minimum possible value of \code{x}.
  }
  \item{oldMax}{
    An optional real number giving the maximum possible value of \code{x}.
  }
  \item{newMin}{
    An optional real number giving the minimum possible value of the
    transformed input vector. 
  }
  \item{newMax}{
    An optional real number giving the maximum possible value of the
    transformed input vector. 
  }
}
\value{
  A numeric vector containing the range-normalized version of \code{x}.
}
\author{
  Kyle M. Lang
}
\examples{
x0 <- runif(100, -3, 5)
x1 <- rangeNorm(x = x0)

range(x1)
}