#' @export
plotLong <- function(myData,plotIds,by,id,dv){
  myData <- as_LongData(myData,id,dv)
  idCol <- attr(myData,'ID')
  dvCol <- attr(myData,'DV')
  if(missing(plotIds)){
    plotIds <- sample(unique(myData[,idCol]),3)
  }

  colors <- c('red','blue','brown')
  counter <- 1
  nP <- 1
  for(cId in plotIds){
    cData <- myData[myData[,idCol]==cId,]
    forPlot <- data.frame(x=rep(NA,nrow(cData)),y=rep(NA,nrow(cData)))
    for (i in 1:nrow(cData)){
      cData[i,'x'] <- cData[i,by]
      cData[i,'y'] <- cData[i,dv]
    }
    plot(cData$x,cData$y,type='b',add=TRUE,col=colors[nP])
    nP <- nP + 1
  }
  return(plotIds)
}
