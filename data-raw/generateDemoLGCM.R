library(devtools)

trueMuI <- 10;
trueMuS <- 3;
trueVarI <- 4;
trueVarS <- 10;
trueErrVar <- 10

numberPersons <- 250
timePointsPerPerson <- function(){round(rnorm(1,mean=3,sd=1))}
getIntercept <- function(){rnorm(1,mean=trueMuI,sd=sqrt(trueVarI))}
getSlope <- function(){rnorm(1,mean=trueMuS,sd=sqrt(trueVarS))}
getErr <- function(){rnorm(1,mean=0,sd=sqrt(trueErrVar))}
getTime <- function(timePoint){rnorm(1,mean=timePoint,sd=0.5)}
set.seed(249)
demoLGCM <- data.frame(matrix(nrow=1,ncol = 3))
names(demoLGCM) <- c('ID','t','x')
counter <- 1
for (i in 1:numberPersons){
  nTime <- timePointsPerPerson()
  cIntercept <- getIntercept()
  cSlope <- getSlope()
  for (j in 1:nTime){
    demoLGCM[counter,'ID'] <- i
    demoLGCM[counter,'t'] <- getTime(j)
    demoLGCM[counter,'x'] <- cIntercept + cSlope + getErr()
    counter <- counter + 1
  }
}
devtools::use_data(demoLGCM,overwrite = TRUE)

