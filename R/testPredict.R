require(MASS)
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
tVector <-15
names(tVector) <- paste('t3')
newData <- as.data.frame(t(tVector)) #force R to make dataframe with one row
preds <- predict.GPPM(gpModel,1,newData)
