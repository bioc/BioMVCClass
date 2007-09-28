#########
# create a class to hold gene set enrichment data
#########
# not sure if I need to store the ExpressionSet data
# incidMat is the incidence matrix that shows the relationships between
#  genes and gene sets
# gTestStat is the test statistic for the gene with the phenotype
# gsTestStat is the test statistic for the gene set that is obtained by matrix
#  multiplication of incidMat and gTestStat
# expData is the experimental data (here an ExpressionSet)
# descr is for a description of the gene set being studied
setClass("GSE", representation(incidMat="matrix", gTestStat="numeric",
                 gsTestStat="numeric", expData="ExpressionSet", descr="character"))

if (is.null(getGeneric("incidMat")))
  setGeneric("incidMat", function(object)
            standardGeneric("incidMat"))
setMethod("incidMat", "GSE", function(object)
         object@incidMat)

if (is.null(getGeneric("gTestStat")))
  setGeneric("gTestStat", function(object)
            standardGeneric("gTestStat"))
setMethod("gTestStat", "GSE", function(object)
         object@gTestStat)

if (is.null(getGeneric("gsTestStat")))
  setGeneric("gsTestStat", function(object)
            standardGeneric("gsTestStat"))
setMethod("gsTestStat", "GSE", function(object)
         object@gsTestStat)

if (is.null(getGeneric("expData")))
  setGeneric("expData", function(object)
            standardGeneric("expData"))
setMethod("expData", "GSE", function(object)
         object@expData)

if (is.null(getGeneric("descr")))
  setGeneric("descr", function(object)
            standardGeneric("descr"))
setMethod("descr", "GSE", function(object)
         object@descr)

if (is.null(getGeneric("incidMat<-")))
  setGeneric("incidMat<-", function(object, value)
            standardGeneric("incidMat<-"))
setReplaceMethod("incidMat", "GSE", function(object, value)
         {
           object@incidMat<-value
           object
         }
)

if (is.null(getGeneric("gTestStat<-")))
  setGeneric("gTestStat<-", function(object, value)
            standardGeneric("gTestStat<-"))
setReplaceMethod("gTestStat", "GSE", function(object, value)
         {
           object@gTestStat<-value
           object
         }
)

if (is.null(getGeneric("gsTestStat<-")))
  setGeneric("gsTestStat<-", function(object, value)
            standardGeneric("gsTestStat<-"))
setReplaceMethod("gsTestStat", "GSE", function(object, value)
         {
           object@gsTestStat<-value
           object
         }
)

if (is.null(getGeneric("expData<-")))
  setGeneric("expData<-", function(object, value)
            standardGeneric("expData<-"))
setReplaceMethod("expData", "GSE", function(object, value)
         {
           object@expData<-value
           object
         }
)

if (is.null(getGeneric("descr<-")))
  setGeneric("descr<-", function(object, value)
            standardGeneric("descr<-"))
setReplaceMethod("descr", "GSE", function(object, value)
         {
           object@descr<-value
           object
         }
)

##############
# create a class for model objects
##############

# for a model that has graph data
setClass("graphModel", representation(modelData="graph"), contains="gModel")

# for a model that has expression data - 
# the data list should include an ExpressionSet object and maybe a vector of LL ids
setClass("exprModel", representation(modelData="ExpressionSet"), contains="gModel")

# added 6/28/06 to have a gene set enrichment model
setClass("gseModel", representation(modelData="GSE"), contains="gModel")

###########
# create a class for views
###########

# 7/28/05 put the graphLayout info in the graphModel object
setClass("graphView", representation(grLayout="Ragraph"), contains="plotView")

# 9/1/05 not sure if I need to store anything else about a heatmap
# decided to store the list of row and column reorderings returned from the
# heatmap function (just in case I need it later)
setClass("heatmapView", representation(ordering="list", rNames="character"), 
                        contains="plotView")

#####
# accessor functions
#####

if (is.null(getGeneric("ordering")))
  setGeneric("ordering", function(object)
            standardGeneric("ordering"))
setMethod("ordering", "heatmapView", function(object)
         object@ordering)

if (is.null(getGeneric("rNames")))
  setGeneric("rNames", function(object)
            standardGeneric("rNames"))
setMethod("rNames", "heatmapView", function(object)
         object@rNames)

if (is.null(getGeneric("grLayout")))
  setGeneric("grLayout", function(object)
            standardGeneric("grLayout"))
setMethod("grLayout", "graphView", function(object)
         object@grLayout)

#####
# setting the slots
#####

if (is.null(getGeneric("ordering<-")))
  setGeneric("ordering<-", function(object, value)
            standardGeneric("ordering<-"))
setReplaceMethod("ordering", "heatmapView", function(object, value)
         {
           object@ordering<-value
           object
         }
)

if (is.null(getGeneric("rNames<-")))
  setGeneric("rNames<-", function(object, value)
            standardGeneric("rNames<-"))
setReplaceMethod("rNames", "heatmapView", function(object, value)
         {
           object@rNames<-value
           object
         }
)

if (is.null(getGeneric("grLayout<-")))
  setGeneric("grLayout<-", function(object, value)
            standardGeneric("grLayout<-"))
setReplaceMethod("grLayout", "graphView", function(object, value)
         {
           object@grLayout<-value
           object
         }
)


