### Title:    Data Simulation Functions for the SURF Package
### Author:   Kyle M. Lang
### Created:  2017-OCT-25
### Modified: 2018-JUN-01

##-------------------- COPYRIGHT & LICENSING INFORMATION ---------------------##
##  Copyright (C) 2018 Kyle M. Lang <k.m.lang@uvt.nl>                         ##
##                                                                            ##
##  This file is part of SURF.                                                ##
##                                                                            ##
##  This program is free software: you can redistribute it and/or modify it   ##
##  under the terms of the GNU General Public License as published by the     ##
##  Free Software Foundation, either version 3 of the License, or (at you     ##
##  option) any later version.                                                ##
##                                                                            ##
##  This program is distributed in the hope that it will be useful, but       ##
##  WITHOUT ANY WARRANTY; without even the implied warranty of                ##
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General ##
##  Public License for more details.                                          ##
##                                                                            ##
##  You should have received a copy of the GNU General Public License along   ##
##  with this program.  If not, see <http://www.gnu.org/licenses/>.           ##
##----------------------------------------------------------------------------##

simRegData <- function(nObs,
                       nPreds,
                       r2,
                       sigma,
                       beta,
                       means           = 0,
                       scales          = 1,
                       itemsPerPred    = 1,
                       predReliability = 0.8)
{
    if(length(means) == 1) means <- rep(means, nPreds)

    
    if(is.matrix(sigma))
        sigmaX <- sigma
    else if(is.vector(sigma) & length(sigma) == 1) {
        ## Generate a covariance matrix from 'scales' and 'collin':
        w1 <- matrix(scales, nPreds, nPreds)
        w2 <- matrix(scales, nPreds, nPreds, byrow = TRUE)
        
        maxCov <- w1 * w2
        
        sigmaX       <- maxCov * sigma
        diag(sigmaX) <- scales^2
    }
    else
        stop("'sigma' must be a matrix or a length-one vector")
    
    ## Simulate predictor data:
    X <- cbind(1, rmvnorm(nObs, means, sigmaX))

    ## Simulate the outcome:
    eta    <- X %*% beta
    sigmaY <- (var(eta) / r2) - var(eta)
    y      <- eta + rnorm(nObs, 0, sqrt(sigmaY))

    ## Generate a latent structure wherein each predictor is indicated by
    ## 'itemsPerPred' observed variables:
    if(itemsPerPred > 1) {
        nItems   <- nPreds * itemsPerPred
        loadings <- matrix(0, nItems, nPreds)

        ## Populate loading matrix:
        for(m in 1 : nPreds) {
            for(n in 1 : itemsPerPred) {
                offset                  <- (m - 1) * itemsPerPred
                loadings[n + offset, m] <- sqrt(predReliability)
            }
        }

        ## Populate error variance matrix:
        theta <- diag(rep(1 - predReliability, nItems))

        ## Generate observed items:
        X <- X[ , -1] %*% t(loadings) + rmvnorm(nObs, rep(0, nItems), theta)
        
        outDat           <- data.frame(y, X)
        colnames(outDat) <- c("y", paste0("x", c(1 : nItems)))
    }
    else {
        outDat           <- data.frame(y, X[ , -1])
        colnames(outDat) <- c("y", paste0("x", c(1 : nPreds)))
    }
    outDat
}# END simulateData()



makeRVec <- function(linPred, pm, snr, pattern) {
    ## Generate a linear predictor of missingness:
    noise   <- sd(linPred) * (1 / snr)
    linPred <- linPred + rnorm(length(linPred), 0, noise)

    ## Use a probit model to simulate response propensities:
    probs <- pnorm(scale(linPred))

    ## 'pattern' selects which part of the target's distribution is missing:
    if(pattern == "random")
        pattern <- sample(c("low", "high", "tails", "center"), size = 1)

    ## Generate the missignness indicator:
    rVec <- switch(pattern,
                   low    = probs <= pm,
                   high   = probs >= (1 - pm),
                   tails  = probs <= (pm / 2) | probs >= (1 - (pm / 2)),
                   center = probs >= (0.5 - (pm / 2)) &
                       probs <= (0.5 + (pm / 2)),
                   stop("Please provide a valid 'pattern'")
                   )
    list(rVec = rVec, pattern = pattern)
}# END makeRVec()

        

imposeMissData <- function(data, targets, preds, pm, snr, pattern = "random") {
    ## Which mechanisms should be simulated?
    mechFlag <- !is.na(targets)

    if(length(pattern) == 1) {
        pattern        <- rep(pattern, length(c(targets$mar, targets$mnar)))
        names(pattern) <- c(targets$mar, targets$mnar)
    }
    
    ## Create a vector to hold patterns used:
    patOut        <- rep(NA, ncol(data))
    names(patOut) <- colnames(data)
    
    ## Impose MCAR missing:
    if(mechFlag["mcar"]) {
        rMat <- matrix(
            as.logical(
                rbinom(n    = prod(dim(data[ , targets$mcar])),
                       size = 1,
                       prob = pm$mcar)
            ),
            ncol = length(targets$mcar)
        )
        data[ , targets$mcar][rMat] <- NA
        patOut[targets$mcar]        <- "mcar"
    }
    
    ## Impose MAR missing:
    if(mechFlag["mar"]) {
        if(length(preds) == 1) linPred <- data[ , preds]
        else                   linPred <- rowSums(data[ , preds], na.rm = TRUE)
        
        if(any(is.na(linPred)))
            stop("MAR predictor matrix cannot have any entirely missing rows.")
                                              
        for(i in targets$mar) {
            tmp <- makeRVec(linPred = linPred,
                            pm      = pm$mar,
                            snr     = snr$mar,
                            pattern = pattern[i])
            data[tmp$rVec, i] <- NA
            patOut[i]         <- tmp$pattern
        }
    }
    
    ## Impose MNAR missing:
    if(mechFlag["mnar"]) {
        for(i in targets$mnar) {
            tmp <- makeRVec(linPred = data[ , i],
                            pm      = pm$mnar,
                            snr     = snr$mnar,
                            pattern = pattern[i])
            data[tmp$rVec, i] <- NA
            patOut[i]         <- tmp$pattern
        }
    }
    list(data = data, pattern = patOut)
}# END imposeMissing()
