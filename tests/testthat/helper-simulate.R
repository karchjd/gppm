# data("demoLGCM")
# demoLGCM <- demoLGCM[1:12,]
# demoLGCM <- demoLGCM[sample(1:nrow(demoLGCM),nrow(demoLGCM)),]
# lgcm <- gppm('muI+muS*t','varI+covIS*(t+t#)+varS*t*t#+(t==t#)*sigma',
#              demoLGCM,'ID','x')
# parameterValues <- c(0,-500,0,0,0, 1)
# names(parameterValues) <-c('muI','muS','varI','varS','covIS','sigma')
