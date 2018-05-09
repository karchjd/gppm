#make sure that devtools are installed
if (!requireNamespace('devtools')){
  install.packages('devtools')
}
devtools::install_github('karchjd/gppmr@develop')
