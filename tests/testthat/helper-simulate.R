data("demoLGCM")
data('trueParas')
demoLGCM <- demoLGCM[sample(1:nrow(demoLGCM),nrow(demoLGCM)),]
lgcm <- gppm('muI+muS*t','varI+covIS*(t+t#)+varS*t*t#+(t==t#)*sigma',
             demoLGCM,'ID','x')
parameterValues <- trueParas
lgcmFit <-fit(lgcm,init=parameterValues)
modelForExtract <- lgcmFit
