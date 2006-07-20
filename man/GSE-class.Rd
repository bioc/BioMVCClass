\name{GSE-class}
\docType{class}
\alias{GSE-class}
\alias{GSE}
\alias{incidMat<-,GSE-method}
\alias{incidMat,GSE-method}
\alias{incidMat<-}
\alias{incidMat}
\alias{gTestStat<-,GSE-method}
\alias{gTestStat,GSE-method}
\alias{gTestStat<-}
\alias{gTestStat}
\alias{gsTestStat<-,GSE-method}
\alias{gsTestStat,GSE-method}
\alias{gsTestStat<-}
\alias{gsTestStat}
\alias{expData<-,GSE-method}
\alias{expData,GSE-method}
\alias{expData<-}
\alias{expData}
\alias{descr<-,GSE-method}
\alias{descr,GSE-method}
\alias{descr<-}
\alias{descr}
\title{Class "GSE": A class to represent gene set enrichment data }
\description{	 
  GSE is a class to represent gene set enrichment data and will be used in the
  modelData slot in the gseModel object.  This class will store all of the
  information that pertains to performing gene set enrichment.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("GSE", ...)}.
}
\section{Slots}{
	 \describe{
    \item{\code{incidMat}:}{the incidence matrix that shows the relationship
    between the genes and the gene sets }
    \item{\code{gTestStat}:}{the test statistic for the genes relationship
    with the phenotype }
    \item{\code{gsTestStat}:}{the test statistic for the gene set }
    \item{\code{expData}:}{the experimental data (here it will be of class exprSet) }
    \item{\code{descr}:}{a description of the gene set being studied }
  }
}
\section{Methods}{
  \describe{
    \item{incidMat<-}{Sets the \code{incidMat} slot }
    \item{incidMat}{Returns the \code{incidMat} slot }
    \item{gTestStat<-}{Sets the \code{gTestStat} slot }
    \item{gTestStat}{Returns the \code{gTestStat} slot }
    \item{gsTestStat<-}{Sets the \code{gsTestStat} slot }
    \item{gsTestStat}{Returns the \code{gsTestStat} slot }
    \item{expData<-}{Sets the \code{expData} slot }
    \item{expData}{Returns the \code{expData} slot }
    \item{descr<-}{Sets the \code{descr} slot }
    \item{descr}{Returns the \code{descr} slot }
	 }
}
\author{ Elizabeth Whalen }
\seealso{
  \code{\link{gseModel-class}}
}
\examples{
}
\keyword{classes}
