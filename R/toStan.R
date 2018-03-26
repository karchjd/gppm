validate_toStan <- function(parsedModel,myData){
  stopifnot(is(parsedModel,'ParsedModel'))
  stopifnot(is(myData,'StanData'))
}


toStan <-function(parsedModel,myData,control){
  validate_toStan(parsedModel,myData)

  templateLocation <- file.path(system.file(package = 'gppm'),'stanTemplate.stan')
  theTemplate <- readChar(templateLocation, file.info(templateLocation)$size)

  theCode <- theTemplate;
  paramSect <- paste0('real ', parsedModel$params,';',collapse = '\n ')
  theCode <- gsub('<parameters>',paramSect,theCode)
  theCode <- gsub('<meanfunction>',parsedModel$mFormula,theCode)
  theCode <- gsub('<covfunction>',parsedModel$kFormula,theCode)
  if(control$stanModel){
    theModel <- stan_model(model_code = theCode)
  }
}


