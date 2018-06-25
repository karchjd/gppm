## Resubmission
This is a resubmission. To address the maintainer's comments, I have

* Changed gppm to 'gppm' in the DESCRIPTION file

* Changed all \dontrun tags to \donttest

## Test environments
* local mac OS 10.12.6, R 3.5.0 
* ubuntu 14.04 (on travis-ci), R 3.5.0 
* win-builder (devel) 

## R CMD check results
There were no ERRORs, WARNINGs and one NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Julian D. Karch <j.d.karch@fsw.leidenuniv.nl>’

New submission

## Downstream dependencies
I have also run R CMD check locally on downstream dependencies of gppm using devtools::revdep_check(). No ERRORs or WARNINGs were found.
