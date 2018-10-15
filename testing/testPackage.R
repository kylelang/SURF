### Title:    Test the SURF Package
### Author:   Kyle M. Lang
### Created:  2017-NOV-03
### Modified: 2018-JUN-01

library(SURF)

### TEST HELP EXAMPLES ###

### simRegData ###

testData <- simRegData(nObs  = 50000,
                       nVars = 10,
                       r2    = 0.5,
                       sigma = 0.2,
                       beta  = matrix(c(0.25, rep(0.75, 10)))
                       )

summary(lm(testData[ , 1] ~ as.matrix(testData[ , -1])))
cor(testData[ , -1])

sigma       <- matrix(0.35, 10, 10)
diag(sigma) <- 1.0

testData <- simRegData(nObs  = 50000,
                       r2    = 0.5,
                       sigma = sigma,
                       beta  = matrix(c(0.25, rep(0.75, 10)))
                       )

summary(lm(testData[ , 1] ~ as.matrix(testData[ , -1])))
cor(testData[ , -1])

### simCovData ###
testData <- simCovData(nObs = 50000, sigma = 0.5, nVars = 5, scales = 1.5)
cor(testData)
cov(testData)
sqrt(diag(cov(testData)))

head(testData)

### imposeMissData ###

testData <- simRegData(nObs  = 500,
                       nVars = 10,
                       r2    = 0.5,
                       sigma = 0.2,
                       beta  = matrix(c(0.25, rep(0.75, 10)))
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


missData <- imposeMissData(data     = testData,
                           targets  = list(mar  = c("y", "x1"),
                                           mcar = c("x2", "x3"),
                                           mnar = c("x4", "x5")
                                           ),
                           preds   = c("x8", "x9", "x10"),
                           pm      = 0.2,
                           snr     = 2.5,
                           pattern = "random")

colMeans(is.na(missData$data))

missData <- imposeMissData(data     = testData,
                           targets  = list(mar  = c("y", "x1"),
                                           mcar = c("x2", "x3"),
                                           mnar = c("x4", "x5")
                                           ),
                           preds   = c("x8", "x9", "x10"),
                           pm      = 0.2,
                           snr     = 2.5,
                           pattern = c(y  = "low",
                                       x5 = "random",
                                       x1 = "center",
                                       x4 = "tails")
                           )

colMeans(is.na(missData$data))

### plotImps ###

data(testImps)

plotImps(impList = testImps$impList,
         rMat    = testImps$rMat,
         typeVec = testImps$typeVec,
         interactive = TRUE)

### f2n ###

## Generate a factor:
x1 <- factor(c(0 : 5))
x1

## Naive type-casting:
as.numeric(x1)

## Safe type-casting:
f2n(x1)

### calcMode ###

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

### rangeNorm ###

x0 <- runif(100, -3, 5)
x1 <- rangeNorm(x = x0)

range(x1)
