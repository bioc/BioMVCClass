\name{heatmapView-class}
\docType{class}
\alias{heatmapView-class}
\alias{heatmapView}
\alias{ordering<-,heatmapView-method}
\alias{ordering,heatmapView-method}
\alias{ordering}
\alias{ordering<-}
\alias{rNames<-,heatmapView-method}
\alias{rNames,heatmapView-method}
\alias{rNames<-}
\alias{rNames}

\title{Class "heatmapView": A class to represent a heatmap view }
\description{	 
  heatmapView is a class to represent a view that is a heatmap.  
  heatmapView inherits from the class, plotView, which inherits from the
  virtual class, genView. 
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("heatmapView", ...)}.
The initialize method for this class will be created in other packages that
use this package (for example, the initialize method will be created in the
iSNetwork package). 
}
\section{Slots}{
	 \describe{
    \item{\code{ordering}:}{a list of of information returned from the heatmap
    function }
    \item{\code{plotDevice}:}{the plot device number }
    \item{\code{plotPar}:}{the parameter list for the plot, see par() }
    \item{\code{drArea}:}{an object of class \code{"GtkDrawingArea"} }
    \item{\code{dataName}:}{a character string describing what data are shown
    in the view }
    \item{\code{win}:}{an object of class \code{"GtkWindow"} that holds the
    view }
    \item{\code{winNum}:}{a number that tells what number view this is (for
    example, the first view created will have winNum=1) }
    \item{\code{rNames}:}{the names of the rows to be included in the heatmap
    (this allows the original data to be subset in the view) }
  }
}
\section{Extends}{
Class \code{"plotView"}, directly.
Class \code{"genView"}, by class \code{"plotView"}.
}
\section{Methods}{
  \describe{
    \item{ordering<-}{Sets the \code{ordering} slot }
    \item{ordering}{Returns the \code{ordering} slot }
    \item{rNames<-}{Sets the \code{rNames} slot }
    \item{rNames}{Returns the \code{rNames} slot }
	 }
}
\author{ Elizabeth Whalen }
\examples{
}
\keyword{classes}
