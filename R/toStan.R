validate_toStan <- function(parsedModel,myData){
  stopifnot(is(parsedModel,'ParsedModel'))
  stopifnot(is(myData,'StanData'))
}


toStan <-function(parsedModel,myData,control){
  ##constants
  templateLocation <- '~/mystuff/projects/GPPMSoftware/R/gppm/R/stanTemplate.stan'

  validate_toStan(parsedModel,myData)


  ##
  theTemplate <- readChar(templateLocation, file.info(templateLocation)$size)


  paramSect <- paste0('real ', parsedModel$params,';',collapse = '\n ')

  theCode <- theTemplate;
  theCode <- gsub('<parameters>',paramSect,theCode)
  theCode <- gsub('<meanfunction>',parsedModel$mFormula,theCode)
  theCode <- gsub('<covfunction>',parsedModel$kFormula,theCode)
  if(control$stanModel){
    theModel <- stan_model(model_code = theCode)
  }
}


