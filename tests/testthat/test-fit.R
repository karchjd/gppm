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
  data("demoLGCM")
  data('trueParas')
  lgcm <- gppm('muI+muS*t','varI+covIS*(t+t#)+varS*t*t#+(t==t#)*sigma',
               demoLGCM,'ID','x')
  try(lgcm <- fit(lgcm,verbose = FALSE),silent=TRUE)

  #for this seed all true paras are in data set
  confInters <- confint(lgcm)
  for (cPar in names(trueParas)){
    expect_true(confInters[cPar,1] < trueParas[cPar] && confInters[cPar,2] > trueParas[cPar])
  }
})



