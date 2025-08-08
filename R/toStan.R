validate_toStan <- function(parsedModel, myData) {
  stopifnot(is(parsedModel, "ParsedModel"))
}

.pkgglobalenv <- new.env(parent = emptyenv())

toStan <- function(parsedModel, control) {
  validate_toStan(parsedModel)

  templateLocation <- file.path(system.file(package = "gppm"), "stanTemplate.stan")
  theTemplate <- readChar(templateLocation, file.info(templateLocation)$size)

  theCode <- theTemplate
  paramSect <- paste0("real ", parsedModel$params, ";", collapse = "\n ")
  theCode <- gsub("<parameters>", paramSect, theCode)
  theCode <- gsub("<meanfunction>", parsedModel$mFormula, theCode)
  theCode <- gsub("<covfunction>", parsedModel$kFormula, theCode)
  theModel <- NULL
  if (control$stanModel) {
    theOut <- utils::capture.output(tryCatch(
      {
        theOutOut <- utils::capture.output(theModel <- rstan::stan_model(model_code = theCode, auto_write = TRUE), type = "output")
        print(theOutOut)
      }), type = "message")

    if (is.null(theModel)) {
      parseErrorOuttoStan(theOut)
      for (i in seq_along(theOut)) {
        message(theOut[i])
      }
      stop("Stan error. See above")
    }
  } else {
    theModel <- NA
  }
  return(theModel)
}

parseErrorOuttoStan <- function(errorOut) {
  if (any("Variable identifier (name) may not be reserved word" %in% errorOut)) {
    tmp <- stringr::str_extract(errorOut, "(?<=found identifier=).+")
    varName <- tmp[!is.na(tmp)]
    stop(sprintf("Change parameter name '%s'. It corresponds to a reserved word in stan.", varName))
  }
}
