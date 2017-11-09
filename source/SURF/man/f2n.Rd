\name{f2n}
\alias{f2n}
\title{Safely Cast a Factor to a Numeric Vector}
\description{
  This function will type-cast a factor to a numeric vector without
  mangling the levels when those levels are zero-indexed integers.
}
\usage{
f2n(x)
}
\arguments{
  \item{x}{
    A factor to be converted.
  }
}
\details{
  R indexes factor levels from 1, so type-casting factors that have
  zero-indexed integer labels using \code{as.numeric} can produce
  unanticipated results. This function will cast such factors to
  zero-indexed numeric vectors wherein the original factor \emph{labels}
  are mapped to numeric values (rather than the inherent factor
  \emph{levels} being so mapped).
}
\value{
  A numeric vector containing the type-cast version of \code{x}.
}
\author{
  Kyle M. Lang
}
\examples{
## Generate a factor:
x1 <- factor(c(0 : 5))
x1

## Naive type-casting:
as.numeric(x1)

## Safe type-casting:
f2n(x1)
}