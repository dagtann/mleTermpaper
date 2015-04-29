load(file.path(pathOut, 'base.RData'))

## --- replicate ER results -------------------------------------
ivTerms <- paste(         ## generic formula for independent vars
  c('fh_ordinal', 'cooptation', 'prio_conflict_intra', 
    'prio_conflict_inter', 'gled_lpop', 'gled_lgdppc', 
    'geddes_personal', 'geddes_monarch', 'geddes_party', 
    'wdi_tradegdp', 'coldwar', 'wdi_gdppcgrowth', 
    'archigos_durationLin', 'archigos_durationSqu', 
    'archigos_durationCub', 'archigos_pastleaderfail', 
    'powthy_pastattempts', 'pseudologross', 'election', 
    'banks_genstrike', 'banks_riot', 'banks_antigovdem',
    'flip_ciri_phys'
  ), collapse = '+'
)

## --- analysis run in parallel ---------------------------------
library(parallel)
numWorkers <- detectCores()
mcpolr <- function(data){
  ## Compute model
  mod <- polr(
    as.formula(paste0(dv, '~', ivTerms)),
    data = data,
    method = 'logistic', model = TRUE, Hess = TRUE,
    control = list(maxit = 500)        ## increase max iterations
  )
  ## Compute cluster robus standard errors
  mod[['model']][, 'cowcode'] <- data[-mod[['na.action']], 'cowcode']
  varCluster <- clusterRobustSe(
    fm = mod, cluster = mod[['model']][, 'cowcode']
  )
  return(list(model = mod, varCluster = varCluster))
}

## --- Empowerment rights ---------------------------------------
er.replicate <- vector('list', length = length(erDvs))
names(er.replicate) <- erDvs
for(dv in erDvs){          ## implicit looping over dv in formula
  er.replicate[[dv]] <- mclapply(
    dta.list, mcpolr, 
    mc.cores = numWorkers, mc.cleanup = TRUE
  )
}
summary(er.replicate)
lapply(er.replicate, summary)

## --- Physical integrity rights ---------------------------------
pi.replicate <- vector('list', length = length(piDvs))
names(pi.replicate) <- piDvs
for(dv in piDvs){
  pi.replicate[[dv]] <- mclapply(
    dta.list, mcpolr, 
    mc.cores = numWorkers, mc.cleanup = TRUE
  ) 
}
summary(pi.replicate)
lapply(pi.replicate, summary)

detach(package:parallel)

rm(dv, numWorkers)
save.image(file.path(pathOut, 'omodels.RData'))
## END