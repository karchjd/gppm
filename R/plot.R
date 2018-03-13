
plot.GPPM <- function(gpModel,plotIds){
  theData <- getIntern(gpModel,'data')
  plotLong(theData,plotIds)
}
