# SURF: Some Useful R Functions
This repository hosts development of the R package `SURF`.

- Licensing information is given in the [LICENSE][] file.
- Built tarballs of the `SURF` package are available in the [builds][] 
  directory.
- Stand-alone documentation is available in the [documentation][docs] directory.
- The source files for the most recent stable version of `mibrr` are available 
  in the [source][src] directory.
  
`SURF` is alpha software created largely for my own personal use (i.e., so that 
I don't have to keep re-implementing these functions). If you choose to use 
`SURF`, please expect frequent---and dramatic---changes to the package's 
functionality and user interface. Please report any bugs that you encounter in 
the issues section of the project page.

## Installation
The best way to install `SURF` is to use the `devtools::install_github` 
function.

1. First, make sure that you have `devtools` installed on your system
2. Next, execute the following lines:

		library(devtools)
		install_github("kylelang/SURF/source/SURF")
    
3. Finally, load `SURF` and enjoy:

		library(SURF)

If the `devtools`-based approach does not work, you can download one of the
built tar-balls from the [builds][] directory and manually install the package
from source by executing the following lines:

	install.packages(pkgs  = "/SAVE_PATH/SURF_VERSION.tar.gz",
	                 repos = NULL,
					 type  = "source")

Where *SAVE_PATH* is replaced by the (relative or absolute) file path to the
location where you saved the tar-ball, and *VERSION* is replaced with the correct
version number for the tar-ball that you downloaded.

[builds]:  https://github.com/kylelang/SURF/tree/master/builds/
[docs]:    https://github.com/kylelang/SURF/tree/master/documentation/
[src]:     https://github.com/kylelang/SURF/tree/master/source/SURF
[LICENSE]: https://github.com/kylelang/SURF/blob/master/LICENSE
