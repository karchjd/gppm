context("gppm-summary")
test_that("unfitted", {
  print(summary(lgcm))
  expect_equal(1,1)
})

test_that("fitted", {
  print(summary(lgcmFit))
  expect_equal(1,1)
})



