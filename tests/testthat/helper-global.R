data("demoLGCM")
data('trueParas')
lgcm <- gppm('muI+muS*t','varI+covIS*(t+t#)+varS*t*t#+(t==t#)*sigma',
             demoLGCM,'ID','x')
parameterValues <- trueParas
lgcmFit <-fit(lgcm,init=parameterValues)
modelForExtract <- lgcmFit

