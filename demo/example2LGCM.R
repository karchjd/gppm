##setup
require(gppmr)
require(OpenMx)
require(MASS)

##settings
nP <- 100
nT <- 3

##define latent growth curve model using SEM software
lgcModel <- generateLGCM(nT)
trueModel <- omxSetParameters(lgcModel,labels=c('muI','muS','varI','varS','covIS','sigma'),values=c(10,3,1,2,0.5,0.25))

##simulate data
yVector <- simulateData(trueModel,N=nP)
tVector <- matrix(rep(0:(nT-1),each=nP),nrow = nP,ncol = nT)

##fit data using SEM
semModel <- mxModel(lgcModel,mxData(yVector,type = "raw"))
semModel <- mxRun(semModel,silent = TRUE)

##fit data using GPPM
gpModel <- gppModel(tVector,yVector,'$muI$+$muS$*$t$','$varI$+$covIS$*($s$+$t$)+$varS$*$s$*$t$+omxApproxEquals($s$,$t$,0.0001)*$sigma$')
gpModel <- omxSetParameters(gpModel, labels=c("GPPM.muI[1,1]","GPPM.muS[1,1]","GPPM.varI[1,1]","GPPM.varS[1,1]","GPPM.covIS[1,1]","GPPM.sigma[1,1]"),
                            values=omxGetParameters(lgcModel)[c('muI','muS','varI','varS','covIS','sigma')]) #same starting values
gpModelFit <- mxRun(gpModel,silent=TRUE)

##compare results
lgcmSame <- all.equal(omxGetParameters(gpModelFit),omxGetParameters(semModel)[c('muI','muS','varI','covIS','varS','sigma')],check.attributes=FALSE,tolerance=0.0001)
message(sprintf('Estimated parameters for the LGCM model are the same: %s',lgcmSame))
