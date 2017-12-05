getObsMoments <- function(model,i){
  mxGetExpected(model$omx,c('means','covariance'),defvar.row = i)
}

predict.GPPM<- function(model,i,myData){
  ##get mean and covariance matrices
  #auto covariance observed

  #covariance observed
  tmp <- getObsMoments(model,i)
  covObs <- tmp$covariances

  #mean observed
  meanObs <- tmp$means

  #auto covariance new
  covNew <- getCovNew(model,myData,mode='auto')
  #mean new
  meanNew <- getMeanNew(model,myData)

  #cross covariance
  covObsNew <- getCovNew(model,myData,mode='cross')


  ##get predictions
}
