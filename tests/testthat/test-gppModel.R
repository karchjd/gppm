context("gppModel-easychecks")
test_that("wrong variable names", {
  tmpData <- myDataLong
  names(tmpData)[3] <- 't '
  expect_error(gpModel <- gppModel('b0+b1*t','(t==t#)*sigma',tmpData,stanModel=FALSE),'Invalid variable name')
  names(tmpData)[3] <- 't!#'
  expect_error(gpModel <- gppModel('b0+b1*t','(t==t#)*sigma',tmpData,stanModel=FALSE),'Invalid variable name')
  names(tmpData)[3] <- 'Ha ha'
  expect_error(gpModel <- gppModel('b0+b1*t','(t==t#)*sigma',tmpData,stanModel=FALSE),'Invalid variable name')
  names(tmpData)[3] <- '3D5'
  expect_error(gpModel <- gppModel('b0+b1*t','(t==t#)*sigma',tmpData,stanModel=FALSE),'Invalid variable name')
})

test_that("ID not in", {
  expect_error(gpModel <- gppModel('b0+b1*t','(t==t#)*sigma',myData,ID='ID2',DV='y',stanModel=FALSE),'ID variable')
})

test_that("DV not in", {
  expect_error(gpModel <- gppModel('b0+b1*t','(t==t#)*sigma',myData,ID='ID',DV='y2',stanModel=FALSE),'DV variable')
})




context("gppModel-meanCovariance")
test_that("linear regression", {
  gpModel <- gppModel('b0+b1*t','(t==t#)*sigma',myDataLong,stanModel=FALSE)

  mFormula <- getIntern(gpModel,'parsedmFormula')
  cFormula <- getIntern(gpModel,'parsedcFormula')

  expect_equal(mFormula,"b0+b1*X[i,j,1]")
  expect_equal(cFormula,"(X[i,j,1]==X[i,k,1])*sigma")
})

test_that("Bayesian Linear regression", {
  gpModel <- gppModel('0','(t*t#+1)*sigmab+(t==t#)*sigma',myDataLong,stanModel=FALSE)

  mFormula <- getIntern(gpModel,'parsedmFormula')
  cFormula <- getIntern(gpModel,'parsedcFormula')

  expect_equal(mFormula,"0")
  expect_equal(cFormula,"(X[i,j,1]*X[i,k,1]+1)*sigmab+(X[i,j,1]==X[i,k,1])*sigma")
})

test_that("squared exponential", {
  gpModel <- gppm('c','sigmaf*exp(-(t-t#)^2/rho)+(t==t#)*sigma',myDataLong,stanModel=FALSE)

  mFormula <- getIntern(gpModel,'parsedmFormula')
  cFormula <- getIntern(gpModel,'parsedcFormula')

  expect_equal(mFormula,"c")
  expect_equal(cFormula,"sigmaf*exp(-(X[i,j,1]-X[i,k,1])^2/rho)+(X[i,j,1]==X[i,k,1])*sigma")
})

test_that("hard names", {
  tmp <- myData
  names(tmp) <- c('ID45','t83','y13')
  gpModel <- gppm('b0+b1*t83','(t83==t83#)*sigma',tmp,ID='ID45',DV='y13',stanModel=FALSE)

  mFormula <- getIntern(gpModel,'parsedmFormula')
  cFormula <- getIntern(gpModel,'parsedcFormula')

  expect_equal(mFormula,"b0+b1*X[i,j,1]")
  expect_equal(cFormula,"(X[i,j,1]==X[i,k,1])*sigma")
})

test_that("typo", {
  gpModel <- gppm('b0+b1*t2','(t==t#)*sigma',myDataLong,stanModel=FALSE)

  mFormula <- getIntern(gpModel,'parsedmFormula')
  cFormula <- getIntern(gpModel,'parsedcFormula')

  expect_equal(mFormula,"b0+b1*X[i,j,1]")
  expect_equal(cFormula,"(X[i,j,1]==X[i,k,1])*sigma")
})




