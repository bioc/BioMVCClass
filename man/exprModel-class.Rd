\name{exprModel-class}
\docType{class}
\alias{exprModel-class}
\alias{exprModel}
\title{Class "exprModel": A class to represent an ExpressionSet model }
\description{	 
  exprModel is a class to represent an ExpresssionSet model.  This class inherits
  from the virtual class, gModel.  An object of exprModel is responsible for
  storing and updating the data.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("exprModel", ...)}.
The initialize method for this class will be created in other packages that
use this package (for example, the initialize method will be created in the
iSNetwork package). 
}
\section{Slots}{
	 \describe{
    \item{\code{modelData}:}{the model data, which is an ExpressionSet }
    \item{\code{linkData}:}{a list of functions that link this model to its
    parent and child models (if it has any) }
    \item{\code{virtualData}:}{the data that is needed by the views of this
    object }
    \item{\code{modelName}:}{the name of this model }
    \item{\code{modelVar}:}{a list of variables that refer to the modelData
    (for instance this may be t-test values that were calculated from the
    modelData) }
  }
}
\section{Extends}{
Class \code{"gModel"}, directly.
}
\section{Methods}{
No methods defined with class "exprModel" in the signature.  The methods for
this class will be created in other packages that use this package like
iSNetwork.
}
\author{ Elizabeth Whalen }
\seealso{
  \code{\link{graphModel-class}}
}
\examples{
}
\keyword{classes}
