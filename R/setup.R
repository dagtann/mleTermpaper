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

## END