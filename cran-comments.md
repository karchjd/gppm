## Resubmission
This is a resubmission. To address the maintainer's comments, I have

* decreased the test time from ~465s  to ~170s by skiping a few long tests on cran, and writting some tests more efficiently.

* added DOI references about the method in the description file.

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
