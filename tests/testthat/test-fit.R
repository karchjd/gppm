context('fit')
test_that("useOptimizerFalse", {
  gpModel <- gppm('b0+b1*t','(t==t#)*sigma',myDataLong)
  startValues <- c(10,3,1)
  names(startValues) <- c('b0','b1','sigma')
  gpModel <- fit(gpModel,useOptimizer=FALSE,init=startValues)
  coef(gpModel)
})

test_that("useOptimizerTrue", {
  # data("demoLGCM")
  # lgcm <- gppm('muI+muS*t','varI+covIS*(t+t#)+varS*t*t#+(t==t#)*sigma',
  #              demoLGCM,'ID','x')
  # lgcm <- fit(lgcm)
})



