## Test environments
* local OS X install, R 3.5.0 
* ubuntu XXX (on travis-ci), R XXX
* win-builder (devel and release) 

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

checking installed package size ... NOTE
  installed size is  6.4Mb
  sub-directories of 1Mb or more:
    data   6.1Mb

## Downstream dependencies
I have also run R CMD check locally on downstream dependencies of gppm using devtools::revdep_check()
All packages that I could install passed except:

* Ecoengine: this appears to be a failure related to config on 
  that machine. I couldn't reproduce it locally, and it doesn't 
  seem to be related to changes in httr (the same problem exists 
  with httr 0.4).