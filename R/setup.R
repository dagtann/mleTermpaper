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
source(file.path(pathCode, 'userFunc', 'clusterRobustSe.R'))
## --- declare global options -----------------------------------
options(help_type = 'html')
set.seed(8590)

## --- source builds --------------------------------------------
# source(file.path(pathCode, 'dataBuild.R'))

## --- Published Results ----------------------------------------
source(file.path(pathCode, 'original', 'publishedAnalysis.R'))
source(file.path(pathCode, 'original', 'outputRegressionTablesErRepression.R'))
source(file.path(pathCode, 'original', 'outputRegressionTablesPiRepression.R'))
source(file.path(pathCode, 'original', 'differencesSePi.R'))
source(file.path(pathCode, 'original', 'differencesSeEr.R'))
source(file.path(pathCode, 'original', 'parallelRegressionsPi.R'))
source(file.path(pathCode, 'original', 'parallelRegressionsEr.R'))

## --- Replication steps ----------------------------------------
# source(file.path(pathCode, 'executeCrossValidation.R'))
# source(file.path(pathCode, 'outOfSamplePerformance.R'))
# source(file.path(pathCode, 'simulation1stDifferences.R'), echo = TRUE)
## END