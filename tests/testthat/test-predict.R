data("demoLGCM")
loIndx <- 1:7
leftOut <- demoLGCM[loIndx,]
demoLGCMWO <- demoLGCM[setdiff(1:nrow(demoLGCM),loIndx),]
simpleModel <- gppm('grandMean','(t==t#)*sigma+constVar',
                    demoLGCMWO,'ID','y')
 simpleModel <- fit(simpleModel)
 preds <- predict(simpleModel,leftOut)
