## --- Preliminaries --------------------------------------------
## This script generates regression output tables for
## physical integrity rights violations.
load(file.path(pathOut, 'omodels.RData'))
library('stargazer')

## --- Coefficient matrix ---------------------------------------
ncoef <- length(coef(pi.replicate[[1]][[1]][[1]]))
ntau <- length(
  pi.replicate[[1]][[1]][[1]][['zeta']]
  )
## empty matrix
piCoefMat <- matrix(NA, 
  nrow = ncoef + ntau,
  ncol = length(pi.replicate),
  dimnames = list(
    c(
      names(coef(pi.replicate[[1]][[1]][[1]])),            ## covars
      names(pi.replicate[[1]][[1]][[1]][['zeta']])     ## cut points
    ),
    c('t1', 't2', 't3', 't4', 't5')
  )
)
## fill coefficients and cutpoints
for(i in 1:length(piDvs)){ 
  piCoefMat[1:ncoef, i] <- rowMeans(
    sapply(pi.replicate[[i]], function(x) { coef(x[[1]]) })
  )
  piCoefMat[(ncoef+1):(ncoef+ntau), i] <- rowMeans(
    sapply(pi.replicate[[i]], function(x) { x[[1]][['zeta']] })
  )
  piCoefMat[, i] <- round(piCoefMat[, i], digits = 2)
}
## fill in cluster robust se
for(i in 1:length(piDvs)){
  seRobust <- round(
    rowMeans(
      sapply(pi.replicate[[i]], function(x) {sqrt(diag(x[[2]]))})
    ),
    digits = 2
  )
  piCoefMat[, i] <- paste0(
     piCoefMat[, i], ' (', seRobust, ')'
  )
}

## --- Summary stats --------------------------------------------
N <- vector(length = 6)                      ## obs in regression
N[1] <- 'N'
for(i in 1:length(piDvs)){
  N[i+1] <- mean(
    sapply(pi.replicate[[i]], function(x){ 
      dim(model.matrix(x[[1]]))[1] 
    }
    )
  )
}
ctry <- vector(length = 6)                   ## countries in data
ctry[1] <- 'Countries'
for(i in 1:length(piDvs)){
  ctry[i+1] <- mean(
    sapply(pi.replicate[[i]], function(x){ 
      length(unique(x[[1]][['model']][, 'cowcode'])) 
    }
    )
  )
}
logLikelihood <- vector(length = 6)      ## Log likelihood values
logLikelihood[1] <- 'Log likelihood'
for(i in 1:length(piDvs)){
  logLikelihood[i+1] <- round(
    mean(
      sapply(pi.replicate[[i]], function(x){ logLik(x[[1]]) } )
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
  'Current PIR repression',
  '0|1', '1|2', '2|3', '3|4', '4|5', '5|6', '6|7', '7|8', 'N', 
  'Countries', 'Log likelihood'
)
column.labs <- c(                                  ## model title
  '~', 'PI$_{t+1}$', 'PI$_{t+2}$', 'PI$_{t+3}$', 'PI$_{t+4}$', 
  'PI$_{t+5}$'
)

## --- table output ---------------------------------------------
stargazer(
  rbind(cbind(covar.labs, piCoefMat), N, ctry, logLikelihood),
  title = 'Effect of co-optation on physical integrity rights repression',
  label = 'tbl:piRepressionBton',
  covariate.labels = column.labs,
  summary = FALSE, 
  out = file.path(pathOut, 'piBTON.tex'),
  style = 'apsr',  rownames = FALSE, align = FALSE
)
rm(
  covar.labs, column.labs, i, N, ctry, logLikelihood, 
  seRobust, piCoefMat
)
## END