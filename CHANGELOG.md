# Change Log
All notable changes to the **SURF** project will be documented in this file.

The format is based on [Keep a Changelog][kacl], and this project adheres to
[Semantic Versioning][sv].

## 0.0.0.9004 - 2018-10-15 - ACTIVE

### Added
- A new function, `simCovData`, to simulate data according to a basic covariance 
  structure.

### Changed
- Improved implementation of `simRegData` and `imposeMissData` functions.
- The `nPreds` argument in `simRegData` is now `nVars`.

## 0.0.0.9003 - 2018-06-01

### Changed
- Changed the `collin` argument to `sigma` in `simRegData`.
- Added the option to specify the entire covariance matrix of the predictors in 
  `simRegData`.
- Improved the implementations of the `surfL` and `surfW` functions.
  
## 0.0.0.9002 - 2017-11-17

### Added
- `calcMode` now estimates the mode for continous variables when `discrete = 
  FALSE`
- Package startup message

### Changed
- When simulating MAR missing data with `imposeMissData` the linear predictor is 
  now constructed using available cases

    - Allows for incomplete MAR predictor matrices
	- Throws an error if any rows are completely missing

## 0.0.0.9001 - 2017-11-09

### Added
- new function `rangeNorm` and associated doc page
- new function `calcMode` and associated doc page
- new function `f2n` and associated doc page
- doc file for `testImps.RData` example data

### Change
- included licensing header in all source files

## 0.0.0.9000 - 2017-11-03

### Added
- Initialized package
- Populated initial set of functions:

    1. `simRegData`
	1. `imposeMissData`
	1. `plotImps`
	
[kacl]: http://keepachangelog.com/
[sv]:   http://semver.org/
[hw]:   http://r-pkgs.had.co.nz/

