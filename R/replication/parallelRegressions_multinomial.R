load(file.path(pathOut, 'omodels.RData'))
packs <- c('nnet', 'parallel')
invisible(lapply(packs, library, character.only = TRUE))

## --- set up parallel workspace --------------------------------
numWorkers <- 4
mnomFit <- function(dv, ivs, dta){    ## multinomial fit function
  form <- as.formula(paste0(dv,'~',ivs))
  fit <- multinom(form, data = dta, Hess = TRUE, maxit = 500)
  return(fit)
}

## --- run multinomial alternative models -----------------------
mnom.er <- vector('list', length = length(erDvs))
names(mnom.er) <- erDvs
for(dv in erDvs) {
  mnom.er[[dv]] <- mclapply(
    dta.list, 
    function(x) { mnomFit(dv = dv, ivs = ivTerms, dta = x) },
    mc.cores = numWorkers, mc.cleanup = TRUE
  ) 
}
summary(mnom.er)                                   ## debug check
lapply(mnom.er, summary)
mnom.pi <- vector('list', length = length(piDvs))
names(mnom.pi) <- piDvs
for(dv in piDvs) {
  mnom.pi[[dv]] <- mclapply(
    dta.list, 
    function(x) { 
      mnomFit(dv = dv, ivs = ivTerms, dta = x) 
    },
    mc.cores = numWorkers, mc.cleanup = TRUE
  ) 
}
summary(mnom.pi)                                   ## debug check
lapply(mnom.pi, summary)

## --- generate data.frame for point comparisons ----------------
assum.dta <- expand.grid(dv = c(erDvs, piDvs), imp = 1:5)
assum.dta <- assum.dta[with(assum.dta, order(dv, imp)), ]

pull <- function(from, what) {             ## extraction function
  data.matrix <- sapply(from, 
    function(dv){ 
      sapply(dv, 
        function(imp){ imp[[1]][[what]] }
      )
    }
  )
  data.matrix <- reshape2::melt(
    data.matrix, 
    id.vars = row.names(data.matrix), 
    measure.vars = 1:ncol(data.matrix),
    value.name = what
  )
  return(data.matrix[, 3])
}
assum.dta[, 'deviance.polr'] <- pull(
  from = c(er.replicate, pi.replicate), what = 'deviance'
)
assum.dta[, 'edf.polr'] <- pull(
  from = c(er.replicate, pi.replicate), what = 'edf'
)

pull <- function(from, what) {    ## redefine for [[1]] component
  data.matrix <- sapply(from, 
    function(dv){ 
      sapply(dv, 
        function(imp){ imp[[what]] }
      )
    }
  )
  data.matrix <- reshape2::melt(
    data.matrix, 
    id.vars = row.names(data.matrix), 
    measure.vars = 1:ncol(data.matrix),
    value.name = what
  )
  return(data.matrix[, 3])
}
assum.dta[, 'edf.mnom'] <- pull(
  from = c(mnom.er, mnom.pi), what = 'edf'
)
assum.dta[, 'deviance.mnom'] <- pull(
  from = c(mnom.er, mnom.pi), what = 'deviance'
)
rm(pull)

assum.dta[, 'pval'] <- with(assum.dta, ## generate comparions dta
  pchisq( 
    deviance.polr-deviance.mnom, 
    edf.mnom-edf.polr, 
    lower.tail = FALSE
  )
)
plot(x = 1:nrow(assum.dta), y = assum.dta[, 'pval'])
abline(h = .05)
## Probably everything following the first model violates parallel
## regressions

## --- finishing up ---------------------------------------------
rm(numWorkers, mnomFit, mnom.er, mnom.pi, assum.dta)