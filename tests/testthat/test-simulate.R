context("simulate")
test_that("working", {
   simData <- simulate(lgcm,parameterValues)
   yHat <- simData[,'t']*parameterValues[2]
   expect_true(all.equal(yHat,simData[,'y'],tolerance=6,scale=1))
})

test_that("no GPPM model as input", {
  expect_error(simData <- simulate('muI',parameterValues),'no applicable method')
})

test_that("missing parameter values", {
  expect_error(simData <- simulate(lgcm,parameterValues[1:3]), 'The parameters')
})

test_that("invalid parameter name", {
  names(parameterValues)[3] <- 'Haha'
  expect_error(simData <- simulate(lgcm,parameterValues), 'The parameters')
})






