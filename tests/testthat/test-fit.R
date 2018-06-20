context('fit')
test_that("useOptimizerFalse", {
  gpModel <- gppm('b0+b1*t','(t==t#)*sigma',myDataLong)
  startValues <- c(10,3,1)
  names(startValues) <- c('b0','b1','sigma')
  gpModel <- fit(gpModel,useOptimizer=FALSE,init=startValues)
  coef(gpModel)
  expect_equal(coef(gpModel),startValues)
})

test_that("useOptimizerTrue", {
  #for this seed all true paras are in data set
  confInters <- confint(lgcmFit)
  for (cPar in names(trueParas)){
    expect_true(confInters[cPar,1] < trueParas[cPar] && confInters[cPar,2] > trueParas[cPar])
  }
})



