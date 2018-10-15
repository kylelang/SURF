\name{imposeMissData}
\alias{imposeMissData}
\title{Impose Missing Data}
\description{
  Impose missing data according to MAR, MCAR, and MNAR mechanisms.
}
\usage{
imposeMissData(data,
               targets,
               preds,
               pm,
               snr,
               pattern = "random")
}
\arguments{
  \item{data}{
    A data.frame wherein missing data are to be imposed.
  }
  \item{targets}{
    A named list with slots \dQuote{mar}, \dQuote{mcar}, and
    \dQuote{mnar} containing character vectors giving the column labels
    for variables onto which missing at random, missing completely at
    random, and missing not at random data, respectively, shall be
    imposed.
  }
  \item{preds}{
    A character vector giving the column labels for predictors of the
    MAR missingness.
  }
  \item{pm}{
    A named list with slots \dQuote{mar}, \dQuote{mcar}, and
    \dQuote{mnar} containing real numbers in [0, 1) giving the
    proportions of missing at random, missing completely at random, and
    missing not at random data, respectively, to generate.
  }
  \item{snr}{
    A named list with slots \dQuote{mar} and \dQuote{mnar} containing
    real numbers giving the signal-to-noise ratios of the probit
    regression models used to impose missing at random and missing not
    at random data, respectively. 
  }
  \item{pattern}{
    A (possibly named) character vector indicating in what parts of the missing data
    predictors' distributions MAR and MNAR missing data should be
    imposed. See \emph{Details} section for more information.
  }
}
\details{
  MCAR missing data is imposed by generating a random Bernoulli flag variable
  for each target variable with probability of success equal to
  \code{pm$mcar}.
  
  MAR missing data is imposed via a noisy probit regression model
  wherein the weighted sum of the columns listed in \code{preds} are
  used to predict the response propensity.
  
  MNAR missing data is imposed via the same procedure as MAR missing
  data but the missing data predictor is taken to be the target variable
  itself.

  Legal keywords for the \code{pattern} argument are: \dQuote{low} =
  impose missing in the negative tail of the predictor, \dQuote{high} =
  impose missing in the positive tail of the predictor, \dQuote{center}
  = impose missing in the center of the predictor, \dQuote{tails} =
  impose missing, symmetrically, in both tails of the predictor,
  \dQuote{random} = randomly sample from the previous four keywords.
  
  The \code{pattern} argument can be given as a length-one character
  vector, in which case the specified keyword is applied to all
  applicable variables. Otherwise, the \code{pattern} argument must be a
  named vector with length equal to \code{length(c(targets$mar,
  targets$mnar))} and names matching all elements of
  \code{c(targets$mar, targets$mnar)}. 
}
\value{
  A two-element list with the following slots:
  \describe{
    \item{data: }{
      The incomplete version of \code{data}
    }
    \item{pattern: }{
      A character vector showing which \code{pattern} was used for each
      target variable.
    }
  }
}
\note{
  Due to the stochastic nature of the missing data simulation, the
  actual proportions of missing data will only equal the values provided
  for \code{pm} asymptotically.
}
\author{
  Kyle M. Lang
}
\seealso{
  \code{\link{simRegData}}
}
\examples{
## Simulate some data:
testData <- simRegData(nObs  = 100,
                       nVars = 10,
                       r2    = 0.5,
                       sigma = 0.2,
                       beta  = matrix(c(0.25, rep(0.75, 10)))
                       )

## Impose missing data:
missData <- imposeMissData(data     = testData,
                           targets  = list(mar  = c("y", "x1"),
                                           mcar = c("x2", "x3"),
                                           mnar = c("x4", "x5")
                                           ),
                            preds   = c("x8", "x9", "x10"),
                            pm      = list(mar = 0.2, mcar = 0.1, mnar = 0.1),
                            snr     = list(mar = 5, mnar = 2.5),
                            pattern = "random") 
}