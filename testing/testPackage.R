### Title:    Test the SURF Package
### Author:   Kyle M. Lang
### Created:  2017-NOV-03
### Modified: 2017-NOV-03

library(SURF)

### Test Help Examples ###

## simRegData:
testData <- simRegData(nObs   = 500,
                       nPreds = 10,
                       r2     = 0.5,
                       collin = 0.2,
                       beta   = matrix(c(0.25, rep(0.75, 10)))
                       )

summary(lm(testData[ , 1] ~ as.matrix(testData[ , -1])))

## imposeMissData:
testData <- simRegData(nObs   = 500,
                       nPreds = 10,
                       r2     = 0.5,
                       collin = 0.2,
                       beta   = matrix(c(0.25, rep(0.75, 10)))
                       )

missData <- imposeMissData(data     = testData,
                           targets  = list(mar  = c("y", "x1"),
                                           mcar = c("x2", "x3"),
                                           mnar = c("x4", "x5")
                                           ),
                           preds   = c("x8", "x9", "x10"),
                           pm      = list(mar = 0.2, mcar = 0.1, mnar = 0.1),
                           snr     = list(mar = 5, mnar = 2.5),
                           pattern = "random")

missData$pattern
colMeans(is.na(missData$data))

## plotImps:
data(testImps)

plotImps(impList = testImps$impList,
         rMat    = testImps$rMat,
         typeVec = testImps$typeVec,
         interactive = TRUE)
