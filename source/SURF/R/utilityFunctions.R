### Title:    Utility Functions for the SURF Package
### Author:   Kyle M. Lang
### Created:  2017-NOV-09
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

## Range normalize a vector:
rangeNorm <- function(x,
                      oldMin = min(x),
                      oldMax = max(x),
                      newMin = 0.0,
                      newMax = 1.0)
{

    r0 <- oldMax - oldMin
    r1 <- newMax - newMin

    (x * r1 - min(x) * r1) / r0 + newMin
}

## Compute/estimate the mode:
calcMode <- function(x, discrete = TRUE) {
    if(discrete) {
        ## Find the modal value(s):
        tab  <- table(x, exclude = NULL)
        mode <- names(tab)[which.max(tab)]
        
        ## Break ties randomly:
        if(length(mode) > 1) mode <- mode[sample(c(1 : length(mode)), 1)]
        
        ## Cast output as numeric, if possible:
        if(is.numeric(x)) out <- as.numeric(mode)
        else              out <- mode
    } else {
        dens <- density(x, na.rm = TRUE)
        out  <- dens$x[which.max(dens$y)]
    }
    out
}

## Safely convert a factor to a numeric vector:
f2n <- function(x) as.numeric(as.character(x))
