##splitters
#' @export
createLeavePersonsOutFolds <- function(gpModel,k=10){
  checkGPPM(gpModel)

  #get all person ids
  theData <- datas(gpModel)
  idCol <- getID(theData)
  theIDs <- theData[,idCol]
  uniqueIDs <- sort(unique(theIDs))
  nPers <- length(uniqueIDs)

  #decide which person to put in which fold
  if(k<=nPers){
    min_reps <- nPers%/%k
    if (min_reps > 0) {
      spares <- nPers%%k
      seqVector <- rep(1:k, min_reps)
      if (spares > 0)
        seqVector <- c(seqVector, sample(1:k, spares))
      foldVector <- sample(seqVector)
    }
    else {
      foldVector <- sample(1:k,size = nPers)
    }
  }else{
    stop('Fewer Persons than folds requested. Consider using lower k.')
  }

  #build fold vector for each observation of each person
  foldVectorLong <- vector(mode = "integer", nrow(theData))
  for (i in 1:nrow(theData)){
    personIdx <- theData[i,idCol]==uniqueIDs
    stopifnot(sum(personIdx)==1)
    foldVectorLong[i] <- foldVector[personIdx]
  }
  return(foldVectorLong)
}

validate_cross <- function(gpModel,foldVector,loss){
  #gpModel
  checkGPPM(gpModel)

  #foldVector
  n <- nrow(datas(gpModel))
  if (length(foldVector)!=n){
    stop('foldVector invalid length')
  }
  if (!identical(sort(unique(foldVector)),1:max(foldVector))){
    stop('foldVector invalid format')
  }

  #loss
  if (!(loss %in% c('lpp'))){
    stop('Invalid loss function')
  }
}

#' @export
crossvalidate <- function(gpModel,foldVector,loss='lpp'){
  validate_cross(gpModel,foldVector,loss)


  nFolds <- max(foldVector)
  theData <- datas(gpModel)
  rowsRes <- vector(mode='double',length=nrow(theData))
  res <- 0
  for (cFold in 1:1){
    trainRows <- foldVector != cFold
    testRows <- foldVector == cFold
    #train model
    tmpModel <- subsetData(gpModel,trainRows)
    tmpModel <- fit(tmpModel)
    #get predictions
    thePreds <- predict(tmpModel,theData[testRows,])
    res <- res + accuracy(thePreds)$nLPP
  }
  return(res)
}


