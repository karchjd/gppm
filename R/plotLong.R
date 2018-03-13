#' @import cowplot
#' @export
plotLong <- function(myData,plotIds,by,id,dv){
  if (!is.null(attr(myData,'preds')) && missing(by)){
    by <- attr(myData,'preds')
  }
  myData <- as_LongData(myData,id,dv)
  idCol <- attr(myData,'ID')
  dvCol <- attr(myData,'DV')
  if(missing(plotIds)){
    plotIds <- sample(unique(myData[,idCol]),3)
  }
  plotData <- myData[myData[,idCol] %in% plotIds,]
  plotData[,idCol] <- as.factor(plotData[,idCol])
  toPlot <- ggplot(plotData,aes_string(x=by,y=dvCol,colour=idCol)) + geom_line()
  print(toPlot)
}


# return(ggplot(simDataLong,aes(x=Time,y=value,colour=variable)) + geom_line(size=2)
#        + ylab('Value') + labs(colour = "Person")
#        + theme(axis.text = element_text(size=20),axis.title = element_text(size=22),
#                legend.text = element_text(size=20), legend.title= element_text(size=22)))
