#' Simulated Data From a Latent Growth Curve Model.
#'
#' @format A data frame with 1998 rows and 3 variables:
#' \describe{
#'   \item{ID}{Subject ID}
#'   \item{t}{Time index}
#'   \item{y}{Generic measurement}
#' }
"demoLGCM"

#' Parameters used for generating \code{\link{demoLGCM}}.
#'
#' @format A parameter vector.
"trueParas"


#' Example Gaussian Process Panel model
#'
#' @format An object of class GPPM. A fitted Gaussian process panel model. Used to speed up the example code for the numerous extractor functions. Result of
#'
#' ```
#' data('demoLGCM')
#' exampleModel <- fit(gppm('muI+muS*t','varI+covIS*(t+t#)+varS*t*t#+(t==t#)*sigma',demoLGCM,'ID','y'))
#' ```
"exampleModel"

#' Example Gaussian Process Panel model
#'
#' @format An object of class GPPM. An unfitted Gaussian process panel model. Used for testing only. Result of
#'
#' ```
#' data('demoLGCM')
#' exampleModelNotFit <- fit(gppm('muI+muS*t','varI+covIS*(t+t#)+varS*t*t#+(t==t#)*sigma',demoLGCM,'ID','y'))
#' ```
"exampleModelNotFit"



