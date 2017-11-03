\name{plotImps}
\alias{plotImps}
\title{Plot Imputed vs. Observed Values}
\description{
  This function will generate plots of imputed versus observed values in
  multiply imputed data. These plots can be examined to
  \dQuote{sanity-check} the imputation procedure.
}
\usage{
plotImps(impList,
         rMat,
         typeVec,
         targetVar   = NULL,
         interactive = FALSE)
}
\arguments{
  \item{impList}{
    A list of multiply imputed datasets.
  }
  \item{rMat}{
    A logical pattern matrix flagging missing values in the orginal data
    used to generate the imputed datasets in \code{impList}. Note that
    \code{TRUE} flags missing cells and \code{FALSE} flags observed
    cells.
  }
  \item{typeVec}{
    A character vector with \code{length(typeVec) = ncol(impList[[1]])}
    giving the measurement levels of the variables in
    \code{impList}. Two values are recognized: \dQuote{cat} = a
    categorical variable (i.e., nominal or ordinal) and \dQuote{con} = a
    continuous variable (i.e., interval or ratio).
  }
  \item{targetVar}{
    An optional character vector giving the column names for variables
    to plot. When \code{targetVar = NULL} all variables with imputed
    values are plotted.
  }
  \item{interactive}{
    A logical flag: Should the cycle through all plotted variables by
    prompting the user to continue after generating each plot?
  }
}
\value{
  Used for its side-effects.
}
\author{
  Kyle M. Lang
}
\examples{
data(testImps)

plotImps(impList = testImps$impList,
         rMat    = testImps$rMat,
         typeVec = testImps$typeVec) 
}