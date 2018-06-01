### Title:    Hidden support Functions for the SURF Package
### Author:   Kyle M. Lang
### Created:  2017-NOV-17
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


## Print startup message:
.onAttach <- function(libname, pkgname) {
    version <- read.dcf(file   = system.file("DESCRIPTION", package = pkgname),
                        fields = "Version")
    
    greet <-
        strwrap(
            paste0("Loading: ",
                   pkgname,
                   " ",
                   version,
                   ", Copyright (C) ",
                   format(Sys.time(), "%Y"),
                   " Kyle M. Lang. ",
                   pkgname,
                   " is distributed under Version 3 of the GNU General Public License (GPL-3); execute 'surfL()' for details. ",
                   pkgname,
                   " comes with ABSOLUTELY NO WARRANTY; execute 'surfW()' for details. ",
                   pkgname,
                   " is beta software. Please report any bugs."),
            width = 81)
    
    for(i in greet) packageStartupMessage(i)
}
