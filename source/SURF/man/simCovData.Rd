\name{simCovData}
\alias{simCovData}
\title{Simulate Data with Known Covariance Structure}
\description{
  This function will simulation simple data with a known covariance
  structure.
}
\usage{
simCovData(nObs,
           sigma,
           nVars  = ncol(sigma),
           means  = 0.0,
           scales = 1.0)
         
}
\arguments{
  \item{nObs}{
    An integer giving the number of rows to simulate.
  }
  \item{sigma}{
    Either a numeric matrix giving the covariance matrix of the
    predictor variables or a length-one numeric vector with value in
    [-1.0, 1.0]. In the latter case, \code{sigma} gives the correlation
    between the predictors (i.e., the degree of collinearity).
  }
  \item{nVars}{
    An integer giving the number of (possibly latent) predictor variables to
    simulate. Defaults to \code{ncol(sigma)}.
  }
  \item{means}{
    A numeric vector of predictor means. Recycled when
    \code{length(means) = 1} to match \code{nVars}, otherwise
    \code{length(means)} must equal \code{nVars}.
  }
  \item{scales}{
    A numeric vector of predictor scales. Recycled when
    \code{length(scales) = 1} to match \code{nVars}, otherwise
    \code{length(scales)} must equal \code{nVars}.
    Ignored when \code{sigma} is a matrix.
  }
}
\details{
  If \code{sigma} is a length-one vector, a covariance matrix is constructed
  from the values provided for \code{sigma} and \code{scales}. In this
  case, \code{sigma} gives the \emph{correlation} between predictors
  (not the covariance) and \code{scales} gives the predictors' standard
  deviations.
}
\value{
  An \code{nObs} by \code{nVars} data.frame of simulated data.
}
\note{
  The column labels of the simulated data will be \code{paste0("x", 1 : nVars)}.
}
\author{
  Kyle M. Lang
}
\seealso{
  \code{\link{imposeMissData}}
  \code{\link{simRegData}}
}
\examples{
## Specify 'sigma' as a full covariance matrix:
sigma       <- matrix(0.3, 10, 10)
diag(sigma) <- 1.0

testData <- simCovData(nObs = 100, sigma = sigma)

## Specify 'sigma' as an inter-predictor correlation:
testData <- simCovData(nObs = 100, nVars = 10, sigma = 0.2)
}