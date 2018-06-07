context('cv')
test_that("Each person one Fold", {
  theFolds <- createLeavePersonsOutFolds(lgcm)
  ids <- datas(lgcm)[,gppm:::getID(datas(lgcm))]
  uIds <- sort(unique(ids))
  folds <- vector(mode='integer',length(uIds))
  for (i in 1:length(ids)){
    idIdx = uIds==ids[i]
    if (folds[idIdx]==0){
      folds[idIdx]<-theFolds[i]
    }else{
      expect_identical(folds[idIdx],theFolds[i])
    }
  }
})

test_that("Folds roughly same size", {
  theFolds <- createLeavePersonsOutFolds(lgcm)
  proportion <- table(theFolds)/sum(table(theFolds))
  expect(all(proportion>0.09 & proportion<0.11))
})

