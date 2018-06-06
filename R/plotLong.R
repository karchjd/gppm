#' @import ggplot2
#' @export
plot.LongData <- function(myData,plotIds,by,id,dv){
  if (!is.null(attr(myData,'preds')) && missing(by)){
    by <- attr(myData,'preds')
  }
  myData <- as_LongData(myData,id,dv)
  idCol <- attr(myData,'ID')
  dvCol <- attr(myData,'DV')
  if(missing(plotIds)){
    Ids <- unique(myData[,idCol])
    nIds <- length(Ids)
    plotIds <- sample(Ids,min(5,nIds))
  }
  plotData <- myData[myData[,idCol] %in% plotIds,]
  plotData[,idCol] <- as.factor(plotData[,idCol])
  toPlot <- ggplot2::ggplot(plotData,aes_string(x=by,y=dvCol,colour=idCol)) + geom_line() + ggthemes::theme_tufte()
  return(toPlot)
}
