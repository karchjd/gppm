#' @export
plotLong <- function(myData,plotIds,by,id,dv){
  myData <- as_LongData(myData,id,dv)

  idCol <- attr(myData,'id')
  dvCol <- attr(myData,'dv')
  if(missing(plotIds)){
    plotIds <- sample(unique(myData[,idCol]),3)
  }

  counter <- 1
  for(cId in plotIds){
    cData <- myData[myData[,idCol]==cId,]
    forPlot <- data.frame(x=rep(NA,nrow(cData)),y=rep(NA,nrow(cData)))
    for (i in nrow(cData)){
      cData[i,'x'] <- cData[i,by]
      cData[i,'y'] <- cData[i,dv]
    }
    plot(cData$x,cData$y)
  }
}
