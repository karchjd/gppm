require(MASS)
require(gppmr)
# tVector <- c(0,10,21)
# yVector <- c(40,30,20)
# names(tVector) <- paste0('t',1:3)
# names(yVector) <- paste0('Y',1:3)
tVector <- c(0,10)
yVector <- c(40,25)
names(tVector) <- paste0('t',1:2)
names(yVector) <- paste0('Y',1:2)
myData <- as.data.frame(t(c(tVector,yVector))) #force R to make dataframe with one row

gpModel <- gppModel('muI+muS*t','varI+covIS*(t+t!)+varS*t*t!+omxApproxEquals(t,t!,0.0001)*sigma',myData)

#fake gppFit with starting values as ML results
gpModel <- gppSetStart(gpModel,c('muI','muS','varI','varS','covIS','sigma'),c(58,-1,258,0.4,0,10))
gpModel<- gppFit(gpModel,useOptimizer=FALSE)

#predictions
tVector <-c(15,20,NA,NA)
names(tVector) <- c('t3','t4','Y3','Y4')
newData <- as.data.frame(t(tVector)) #force R to make dataframe with one row
preds <- predictGPPM(gpModel,1,newData)
