context('fit')
test_that("useOptimizerFalse", {
  gpModel <- gppm('b0+b1*t','(t==t#)*sigma',myDataLong)
  startValues <- c(10,3,1)
  names(startValues) <- c('b0','b1','sigma')
  gpModel <- fit(gpModel,useOptimizer=FALSE,init=startValues)
  coef(gpModel)
})
