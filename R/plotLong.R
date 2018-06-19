#' Plot a Long Data Frame
#'
#' This function is used to plot longitudinal data in data frame.
#'
#' @param myData longitudinal data frame; possibly of class 'LongData'.
#'
#' @param plotIds vector of IDs for which the data should be printed. Can be left empty. Then 5 IDs are picked randomly.
#'
#' @param by label of the variable on the x-axis.
#'
#' @param ID label of the ID column.
#'
#' @param DV label of the variable on the y-axis.
#'
#' @return a fitted Gaussian process panel model, which is an object of class 'GPPM'
#' @examples
#' data("demoLGCM")
#' plot(demoLGCM,plotIds=c(1,2,3),by='t',ID='ID',DV='y')
#' plot(demoLGCM,by='t',ID='ID',DV='y') #five random ids
#' lgcm <- gppm('muI+muS*t','varI+covIS*(t+t#)+varS*t*t#+(t==t#)*sigma',
#'         demoLGCM,'ID','y')
#' lgcmData <- datas(lgcm)
#' plot(lgcmData) #for data sets from a gppm by, ID, and DV are not necessary
#' @import ggplot2
#' @export
plot.LongData <- function(myData,plotIds,by,ID,DV){
  if (!is.null(attr(myData,'preds')) && missing(by)){
    by <- attr(myData,'preds')
  }
  myData <- as_LongData(myData,ID,DV)
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
