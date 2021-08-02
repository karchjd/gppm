context('all')
test_that("all-1", {
  data("demoLGCM")
  data('trueParas')
  lgcm <- gppm('muI+muS*t','varI+covIS*(t+t#)+varS*t*t#+(t==t#)*sigma',
               demoLGCM,'ID','x')
  expect(TRUE,TRUE)
})
