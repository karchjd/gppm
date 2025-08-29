# R CMD Check results
── R CMD check results ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── gppm 0.3.0 ────
Duration: 1m 15.6s

❯ checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Julian D. Karch <j.d.karch@fsw.leidenuniv.nl>’
  
  New submission
  
  Package was archived on CRAN
  
  CRAN repository db overrides:
    X-CRAN-Comment: Archived on 2023-05-15 as issues were not corrected
      in time.

❯ checking for future file timestamps ... NOTE
  unable to verify current time

❯ checking DESCRIPTION meta-information ... NOTE
  'LinkingTo' field is unused: package has no 'src' directory

0 errors ✔ | 0 warnings ✔ | 3 notes ✖

## Comments

The package does not work without the LinkingTo field. The package relies on rstan and I used the guidance contained provided by the package authors (https://cran.r-project.org/web/packages/rstantools/vignettes/minimal-rstan-package.html) to setup my package, which includes the LinkingTo field. I tried removing the LinkingTo field but then my calls to the rstan functions fail with error message that headers are missing.
