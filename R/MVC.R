

##############
# create a class for model objects
##############

# for a model that has graph data
setClass("graphModel", representation(modelData="graph"), contains="gModel")

# for a model that has expression data - 
# the data list should include an exprSet object and maybe a vector of LL ids
setClass("exprModel", representation(modelData="exprSet"), contains="gModel")



###########
# create a class for views
###########

# 7/28/05 put the graphLayout info in the graphModel object
setClass("graphView", representation(grLayout="Ragraph"), contains="plotView")

# 9/1/05 not sure if I need to store anything else about a heatmap
# decided to store the list of row and column reorderings returned from the
# heatmap function (just in case I need it later)
setClass("heatmapView", representation(ordering="list"), contains="plotView")

#####
# accessor functions
#####

if (is.null(getGeneric("ordering")))
  setGeneric("ordering", function(object)
            standardGeneric("ordering"))
setMethod("ordering", "heatmapView", function(object)
         object@ordering)

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

if (is.null(getGeneric("grLayout<-")))
  setGeneric("grLayout<-", function(object, value)
            standardGeneric("grLayout<-"))
setReplaceMethod("grLayout", "graphView", function(object, value)
         {
           object@grLayout<-value
           object
         }
)


