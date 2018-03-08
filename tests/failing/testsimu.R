context("simulate")
test_that("working", {
  data("demoLGCM")
  demo
  lgcm <- gppm('muI+muS*t','varI+covIS*(t+t#)+varS*t*t#+(t==t#)*sigma',
               demoLGCM,'ID','x')
  parameterValues <- c(0,-500,0,0,0, 1)
  names(parameterValues) <-c('muI','muS','varI','varS','covIS','sigma')
  simData <- simulate(lgcm,parameterValues)
  simDatad

})
data("demoLGCM")
lgcm <- gppm('muI+muS*t','varI+covIS*(t+t#)+varS*t*t#+(t==t#)*sigma',
        demoLGCM,'ID','x')

parameterValues <- c(0,-5,0,0,0, 0.001)
names(parameterValues) <-c('muI','muS','varI','varS','covIS','sigma')
simData <- simulate(lgcm,parameterValues)
