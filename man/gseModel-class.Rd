\name{gseModel-class}
\docType{class}
\alias{gseModel-class}
\alias{gseModel}
\title{Class "gseModel": A class to represent a GSE model }
\description{	 
  gseModel is a class to represent a gene set enrichment (GSE) model.  This
  class inherits from the virtual class, gModel.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("gseModel", ...)}.
The initialize method for this class will be created in other packages that
use this package (for example, the initialize method will be created in the
iSNetwork package). 
}
\section{Slots}{
	 \describe{
    \item{\code{modelData}:}{the model data, which is an object of GSE }
    \item{\code{linkData}:}{a list of functions that link this model to its
    parent and child models (if it has any) }
    \item{\code{virtualData}:}{the data that is needed by the views of this
    object }
    \item{\code{modelName}:}{the name of this model }
    \item{\code{modelVar}:}{ a list of variables that refer to the modelData
    (for instance this may be t-test values that were calculated from the
    modelData) }
  }
}
\section{Extends}{
Class \code{"gModel"}, directly.
}
\section{Methods}{
No methods defined with class "gseModel" in the signature.  The methods for
this class will be created in other packages that use this package like
iSNetwork.
}
\author{ Elizabeth Whalen }
\seealso{
  \code{\link{GSE-class}},
  \code{\link{gModel-class}},
  \code{\link{dfModel-class}},
  \code{\link{graphModel-class}},
  \code{\link{exprModel-class}}
}
\examples{
}
\keyword{classes}
