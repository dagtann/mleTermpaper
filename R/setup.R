rm(list = ls())                                    ## start clean
if(Sys.info()['user']=='dag'){
  pathCode = '/home/dag/gitreps/mleTermpaper/R' ;
  pathData = '/home/dag/Dropbox/Buero/Dissertation/2015/duke/classes/mle/termPaper/dta' ;
  pathOut = '/home/dag/Dropbox/Buero/Dissertation/2015/duke/classes/mle/termPaper/out'
}

## --- Call standard libraries ----------------------------------
packs <- c('MASS', 'ggplot2', 'ggthemes')
invisible(lapply(packs, library, character.only = TRUE))

## --- Load user defined functions ------------------------------
source(file.path(pathCode, 'userFunc', 'meanArray.R'))

## --- declare global options -----------------------------------
options(help_type = 'html')
set.seed(8590)

## --- source builds --------------------------------------------
# Uncomment for rerun
source(file.path(pathCode, 'dataBuild.R'))
# source(file.path(pathCode, 'executeCrossValidation.R'))
# source(file.path(pathCode, 'outOfSamplePerformance.R'))
# source(file.path(pathCode, 'simulation1stDifferences.R'), echo = TRUE)
## END