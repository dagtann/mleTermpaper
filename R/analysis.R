load(file.path(pathOut, 'base.Rdata'))

## --- replicate ER results -------------------------------------
addIvs <- paste(               ## generic formula required coding
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

er.replicate <- vector('list', length = length(erDvs))
names(er.replicate) <- erDvs

fail <- 0                          ## rudimentary quality control
counter <- 0
address <- vector(length = 25)

for( dv in erDvs ){              ## for all empowerment right dvs
  for( i in 1:length(dta.files) ){       ## for all imputations
    counter <- counter+1                        ## step counter
    print(counter)                              
    er.replicate[[dv]][[paste0("imp", i)]] <- polr(
        as.formula(paste(dv, '~', addIvs, sep = ' ')),
        data = dta.list[[i]],
        method = 'logistic', Hess = TRUE, model = TRUE,
        control = list(maxit = 500),  ## increase max iterations,
        na.action = na.omit
    )
    if(           ## "Quality control": if model did not converge
      er.replicate[[dv]][[paste0("imp", i)]][['convergence']] != 0) {
      fail <- fail+1
      address[counter] <- 1
    }
  }
}
rm(i, dv)

counter
table(fail)
table(address)

coefList <- lapply(
  er.replicate, 
  function(x) { rowMeans(sapply(x, coef)) }
)
coefList

tauList <- lapply(
  er.replicate, 
  function(x) { rowMeans(sapply(x, '[[', 'zeta')) }
)
tauList

seList <- lapply(
  er.replicate, 
  function(x) { lapply(x, vcov) }
)
packs <- c(packs, 'lmtest', 'sandwich', 'multiwayvcov')
lapply(packs, library, character.only = TRUE)

vcovHAC(
  er.replicate[[1]][[1]]
)
seList <- lapply(
  seList, function(x) { 
    meanArray( lapply(x, function(y) sqrt(diag(y)))) 
  }
)
mod <- er.replicate[[1]][[1]]
estfun(mod)

vcovHC(mod)