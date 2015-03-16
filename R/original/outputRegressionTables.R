## --- Preliminaries --------------------------------------------
load(file.path(pathOut, 'omodels.RData'))
library('stargazer')

## --- Coefficient matrix ---------------------------------------
## empty matrix
erCoefMat <- matrix(NA, 
  nrow = length(coef(er.replicate[[1]][[1]][[1]])) + 
  length(er.replicate[[1]][[1]][[1]][['zeta']]),
  ncol = length(er.replicate),
  dimnames = list(
    c(

      names(coef(er.replicate[[1]][[1]][[1]])),            ## covars
      names(er.replicate[[1]][[1]][[1]][['zeta']])     ## cut points
    ),
    c('t1', 't2', 't3', 't4', 't5')
  )
)
## fill coefficients and cutpoints
for(i in 1:length(erDvs)){ 
  erCoefMat[1:23, i] <- rowMeans(
    sapply(er.replicate[[i]], function(x) { coef(x[[1]]) })
  )
  erCoefMat[24:28, i] <- rowMeans(
    sapply(er.replicate[[i]], function(x) { x[[1]][['zeta']] })
  )
  erCoefMat[, i] <- round(erCoefMat[, i], digits = 2)
}
## fill in cluster robust se
for(i in 1:length(erDvs)){
  seRobust <- round(
    rowMeans(
      sapply(er.replicate[[i]], function(x) {sqrt(diag(x[[2]]))})
    ),
    digits = 2
  )
  erCoefMat[, i] <- paste0(
     erCoefMat[, i], ' (', seRobust, ')'
  )
}

## --- Summary stats --------------------------------------------
N <- vector(length = 6)                      ## obs in regression
N[1] <- 'N'
for(i in 1:length(erDvs)){
  N[i+1] <- mean(
    sapply(er.replicate[[i]], function(x){ 
      dim(model.matrix(x[[1]]))[1] 
    }
    )
  )
}
ctry <- vector(length = 6)                   ## countries in data
ctry[1] <- 'Countries'
for(i in 1:length(erDvs)){
  ctry[i+1] <- mean(
    sapply(er.replicate[[i]], function(x){ 
      length(unique(x[[1]][['model']][, 'cowcode'])) 
    }
    )
  )
}
logLikelihood <- vector(length = 6)      ## Log likelihood values
logLikelihood[1] <- 'Log likelihood'
for(i in 1:length(erDvs)){
  logLikelihood[i+1] <- round(
    mean(
      sapply(er.replicate[[i]], function(x){ logLik(x[[1]]) } )
    ),
    digits = 2
  )
}

## --- Table setup ----------------------------------------------
covar.labs <- c('Current ER Repression', 'Co-optation', # row lab
  'Civil war', 'Interstate war', 'log(Population)', 
  'log(GDP/capita)', 'Personalist Regime', 'Monarchy', 
  'Dominant party', 'Trade in Percent of GDP', 'Cold War',
  'Growth in GDP/capita', 
  'Leader Duration', 'Squared Leader Duration', 
  'Cubed Leader Duration', 'Past leader failures', 
  'Past coups', 'log(Oil rents)', 'Election year', 
  'General strikes', 'Riots', 'Antigovernment protests',
  'PIR repression',
  '2|3', '3|4', '4|5', '5|6', '6|7', 'N', 'Countries',
  'Log likelihood'
)
column.labs <- c(                                  ## model title
  '~', 'ER$_{t+1}$', 'ER$_{t+2}$', 'ER$_{t+3}$', 'ER$_{t+4}$', 
  'ER$_{t+5}$'
)

## --- table output ---------------------------------------------
stargazer(
  rbind(cbind(covar.labs, erCoefMat), N, ctry, logLikelihood),
  title = 'Effect of co-optation on empowerment rights repression',
  label = 'tbl:erRepressionBton',
  covariate.labels = column.labs,
  summary = FALSE, 
  out = file.path(pathOut, 'erBTON.tex'),
  style = 'apsr',  rownames = FALSE, align = TRUE
)
rm(
  covar.labs, column.labs, i, N, ctry, logLikelihood, 
  seRobust, erCoefMat
)
## END