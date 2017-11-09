\name{testImps}
\alias{testImps}
\docType{data}
\title{
List of Example Data Elements
}
\description{
  This data object is a list of data elements meant to demonstrate
  usage of the \code{plotImps} function.
}
\usage{data("testImps")}
\format{
  A list with the following three elements.
  \describe{
    \item{impList: }{
      A list containing 10 multiply imputed datasets.
    }
    \item{rMat: }{
      A logical pattern matrix flagging missing values in the data that
      were imputed to generate \code{impList}. 
    }
    \item{typeVec: }{
      A character vector giving the measurement level of each variable
      in \code{impList}. 
    }
  }
}
\seealso{
  \code{\link{plotImps}}
}
\keyword{datasets}
