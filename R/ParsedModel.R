new_ParsedModel <- function(params,mFormula,kFormula){
  stopifnot(is.character(params)) # character vector of parameters
  stopifnot(is.character(mFormula)&&length(mFormula)==1) #string containing the mean function
  stopifnot(is.character(kFormula)&&length(kFormula)==1) #string containing the covariance function

  structure(list(
    params=params,
    mFormula=mFormula,
    kFormula=kFormula),
  class='ParsedModel'
  )
}

validate_input <- function(mFormula,kFormula,myData){
  stopifnot(is.character(mFormula)&&length(kFormula)==1) #string containing the mean function
  stopifnot(is.character(mFormula)&&length(kFormula)==1) #string containing the covariance function
  stopifnot(class(myData)=='StanData') #string containing the covariance function
}

betterRegMatches <- function(modString,resGreg,value=' '){
  stopifnot(length(resGreg)==1)
  resGreg <- resGreg[[1]]
  for (i in 1:length(resGreg)){
    substr(modString,resGreg[i],resGreg[i]+attributes(resGreg)$match.length[i]-1) <- paste0(rep(value,attributes(resGreg)$match.length[i]),collapse = '')
  }
  return(modString)
}

extractParamsPreds <- function(myFormula,myData){
  ##constants
  specialChar <- '!'
  splitters <- c('\\^','%\\^%','\\+','-','%\\*%','\\*','/','%x%','%&%','\\(','\\)',',','[[:alnum:]]*\\(')

  ##detect and remove reserved characters used for functions, operators etc.
  #detect
  regExp <- paste0(splitters,'|',collapse = '')
  regExp <- substr(regExp,1,nchar(regExp)-1)
  grepRes <- gregexpr(regExp,myFormula)

  #remove
  newFormula <- myFormula
  if(grepRes[[1]][1] !=-1){ #any match
    newFormula <- betterRegMatches(newFormula, grepRes)
  }

  ##detect and remove all variables that are in the predictor matrix and thus not parameters
  #detect
  dataNames <- colnames(myData$X[[1]])
  dataNames <- c(dataNames,paste0(dataNames,specialChar)) #add special char
  regExp <- paste0(dataNames,'|',collapse = '') #looking for all chars
  regExp <- substr(regExp,1,nchar(regExp)-1)
  regExp <- paste0('(?<=^| )(',regExp,')(?=$| )',collapse = '') #only look for stuff with whitespace before and after
  grepRes <- gregexpr(regExp,newFormula,perl = TRUE)
  vars <- regmatches(newFormula,grepRes)[[1]]

  #remove
  if (length(vars)>0){
    vars <- gsub(specialChar,'',vars)
    vars <- unique(vars)
    newFormula <- betterRegMatches(newFormula, grepRes)
  }

  ##extract parameters
  newFormula <- gsub('^[[:space:]]+',"",newFormula) #remove spaces at the beginning
  params <- strsplit(newFormula,'[[:space:]]+')[[1]]
  isnotNumber <- suppressWarnings(is.na(as.double(params)))
  params <- params[isnotNumber]
  params <- unique(params)
  list(params=params,preds=grepRes[[1]])
}

createFormula <- function(preds,myData){

  toReplace <- preds
  meanReturn <- meanFunction
  dollarPosition <- c(toReplace,toReplace+attributes(toReplace)$capture.length)
  dollarPosition <- sort(dollarPosition)
  for (i in 1:length(dollarPosition)){
    meanReturn <- paste0(substr(meanReturn,1,dollarPosition[i]-1),'$',substr(meanReturn,dollarPosition[i],nchar(meanReturn)),collapse='')
    dollarPosition <- dollarPosition+1
  }
  return(list(params=params,modelF=meanReturn,vars=vars))
}

parseFormula <- function(myFormula,myData){
  paramsPreds <- extractParamsPreds(myFormula,myData)
  newFormula <- createFormula(paramsPreds$preds,myData)
}



parseModel <- function(mFormula,kFormula,myData){
  validate_input(mFormula,kFormula,myData)
  parseFormula(mFormula,myData)
  parseFormula(kFormula,myData)
  #constants
}
