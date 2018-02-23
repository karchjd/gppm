context('fit')
test_that("linear regression", {
  #generate data according to the linear change model
  b0 <- 3
  b1 <- 2
  sigma <- 1 #abbreviation for \sigma_\epsilon^2
  nTime <- 3
  trueModel <- function(t){b0+b1*t+rnorm(n=1,mean=0,sd=sqrt(sigma))}
  tVector <- 1:nTime
  yVector <- vapply(tVector, trueModel, 1)

  #get results using lm
  fittedLM <- lm(yVector ~ tVector)
  parasLM <- coefficients(fittedLM)
  parasLM[3] <-summary(fittedLM)$sigma
  names(parasLM) <- c("LM.b0",'LM.b1','LM.sigma')

  #data format for GPPM
  myData <- as.data.frame(list(tVector,yVector,rep(1,3)))
  names(myData) <- c('t','Y','ID')
  print(names(myData))
  #get results using GPPM
  gpModel <- gppModel('b0+b1*t','(t==t#)*sigma',myData,ID='ID',DV='Y')
  gpModel <- fit(gpModel)

  #compare results
  expect_equal(parasLM[1:2],coef(gpModel)[1:2],check.attributes = FALSE,tolerance = 0.0001)
})
