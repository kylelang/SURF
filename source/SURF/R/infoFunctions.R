### Title:    License/Warranty Information Functions for SURF
### Author:   Kyle M. Lang
### Created:  2016-MAY-09
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

## Print warranty information:
surfW <- function() {
    lic <- readLines(system.file("LICENSE", package = "SURF"))
    
    start <- grep("15. Disclaimer of Warranty", lic)
    end   <- grep("END OF TERMS AND CONDITIONS", lic) - 1
    
    writeLines(lic[start : end])
}
                    
## Print license:
surfL <- function()
    writeLines(readLines(system.file("LICENSE", package = "SURF")))
