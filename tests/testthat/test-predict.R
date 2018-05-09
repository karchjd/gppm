## check with stuff from paper
fakeData <- data.frame(ID=rep(1,5),t=c(0,10,22,15,33),y=c(40,25,88,1,2))
fakeDataTrain <- fakeData[1:2,]
fakeDataTest <- fakeData[3:5,]

lgcm <- gppm('muI+muS*t','sigmaI^2+sigmaS^2*t*t#+covIS*(t+t#)+noise^2*(t==t#)',fakeDataTrain,ID='ID',DV='y')
paras <- c(58,-1,sqrt(258),sqrt(0.4),0,sqrt(10))
names(paras) <- variable.names(lgcm)
lgcmFit <- fit(lgcm,init=paras,useOptimizer=FALSE)
thePreds <- predict(lgcmFit,fakeDataTest)
stopifnot(round(thePreds$predMean[[1]][2])==19)
stopifnot(round(thePreds$predCov[[1]][2,2])==28)
