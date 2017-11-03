\name{SURF-package}
\alias{SURF-package}
\alias{SURF}
\docType{package}
\title{
\packageTitle{SURF}
}
\description{
\packageDescription{SURF}
}
\details{
The DESCRIPTION file:
\packageDESCRIPTION{SURF}
\packageIndices{SURF}
}
\author{
\packageAuthor{SURF}

Maintainer: \packageMaintainer{SURF}
}
\keyword{ package }
\examples{
## Simulate regression data:
testData <- simRegData(nObs   = 100,
                       nPreds = 10,
                       r2     = 0.5,
                       collin = 0.2,
                       beta   = matrix(c(0.25, rep(0.75, 10)))
                       )

## Impose missing data
missData <- imposeMissData(data     = testData,
                           targets  = list(mar  = c("y", "x1"),
                                           mcar = c("x2", "x3"),
                                           mnar = c("x4", "x5")
                                           ),
                            preds   = c("x8", "x9", "x10"),
                            pm      = list(mar = 0.2, mcar = 0.1, mnar = 0.1),
                            snr     = list(mar = 5, mnar = 2.5),
                           pattern = "random")

## Plot imputed vs. observed values:
data(testImps)

plotImps(impList = testImps$impList,
         rMat    = testImps$rMat,
         typeVec = testImps$typeVec)
}
