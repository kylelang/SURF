\name{simRegData}
\alias{simRegData}
\title{Simulate Regression Data}
\description{
  This function will simulation regression data with known R-Squared,
  inter-predictor correlation, and latent grouping structure among the
  predictors.
}
\usage{
simRegData(nObs,
           r2,
           sigma,
           beta,
           nVars           = ncol(sigma),
           means           = 0.0,
           scales          = 1.0,
           itemsPerPred    = 1,
           predReliability = 0.8)
}
\arguments{
  \item{nObs}{
    An integer giving the number of rows to simulate.
  }
  \item{r2}{
    A real number in [0, 1]. The R-Squared of the data generating model. That
    is, what proportion of variability in the outcome should be explained by the
    predictors.
  }
  \item{sigma}{
    Either a numeric matrix giving the covariance matrix of the
    predictor variables or a length-one numeric vector with value in
    [-1.0, 1.0]. In the latter case, \code{sigma} gives the correlation
    between the predictors (i.e., the degree of collinearity).
  }
  \item{beta}{
    An numeric matrix of regression coefficients with \code{dim(beta) =
      c(nVars, 1)}. Note that the first element is taken to be the intercept.
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
  \item{itemsPerPred}{
    An integer giving the number of observed items used to define each latent
    predictor. When \code{itemsPerPred = 1}, no latent structure is imposed on
    the predictors.
  }
  \item{predReliability}{
    A real number in [0, 1]. When \code{itemsPerPred > 1},
    \code{predReliability} defines the proportion of reliable variance among the
    indicators of each latent predictor. That is, the proportion of shared
    variance among each latent predictor's observed indicators.  
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
  An \code{nObs} by \code{nVars * itemsPerPred + 1} data.frame of simulated
  data.
}
\note{
  The column labels of the simulated data will be \code{c("y",
    paste0("x", 1 : nVars * itemsPerPred))}.
}
\author{
  Kyle M. Lang
}
\seealso{
  \code{\link{imposeMissData}}
  \code{\link{simCovData}}
}
\examples{
## Specify 'sigma' as a full covariance matrix:
sigma       <- matrix(0.3, 10, 10)
diag(sigma) <- 1.0

testData <- simRegData(nObs  = 100,
                       r2    = 0.5,
                       sigma = sigma,
                       beta  = matrix(c(0.25, rep(0.75, 10)))
                       )

## Specify 'sigma' as an inter-predictor correlation:
testData <- simRegData(nObs  = 100,
                       nVars = 10,
                       r2    = 0.5,
                       sigma = 0.2,
                       beta  = matrix(c(0.25, rep(0.75, 10)))
                       )
}