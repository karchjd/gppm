new_GPPMControl <- function(stanModel,verbose){
  stopifnot(is.logical(stanModel))
  stopifnot(is.logical(verbose))


  structure(list(
    stanModel=stanModel, #generate stanModel Object or not
    verbose=verbose #verbose or not
  ),class='GPPMControl')
}

#' @export
gppmControl <- function(stanModel=TRUE,verbose=TRUE){
  new_GPPMControl(stanModel,verbose)
}
