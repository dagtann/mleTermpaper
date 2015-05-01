## This script loads and prepares the unimputed data for
## analysis
## --- Load data ------------------------------------------------
load(file.path(pathOut, 'base.RData'))
library(foreign)
org.dta <- read.dta(
  file.path(pathData, 'amelia_data_input_version12.dta'),
  convert.factors = TRUE,
  convert.underscore = FALSE, 
  convert.dates = TRUE
)
detach(package:foreign)

## --- create factor variables ----------------------------------
org.dta[, 'geddes_party'] <- factor(org.dta[, 'geddes_party'], 
  levels = 0:1, labels = c('No', 'Yes')
)
org.dta[, 'geddes_personal'] <- factor(
  org.dta[, 'geddes_personal'], 
  levels = 0:1, labels = c('No', 'Yes')
)
org.dta[, 'geddes_monarch'] <- factor(org.dta[, 'geddes_monarch'], 
  levels = 0:1, labels = c('No', 'Yes')
)
org.dta[, 'geddes_military'] <- factor(
  org.dta[, 'geddes_military'], 
  levels = 0:1, labels = c('No', 'Yes')
)
org.dta[, 'coldwar'] <- factor(org.dta[, 'coldwar'],   ## cold war
  levels = 0:1, labels = c('No', 'Yes')
)
org.dta[, 'election'] <- factor(org.dta[, 'election'], ## election
  levels = 0:1, labels = c('No', 'Yes')
)
org.dta <- org.dta[with(org.dta, order(cowcode, year)), ] # sort

## --- Impute missing data --------------------------------------
set.seed(8969)
library('Amelia')
# which(
#   names(org.dta) %in% c('fh_ordinal', 'flip_ciri_phys', 'cooptation')
# )
## positions 24, 30 & 39
bds <- matrix(                                  ## logical bounds
  c(
    24, 0, 3,
    30, 1, 7, 
    39, 0, 8
  ), nrow = 3, ncol = 3, byrow = TRUE
)
amelia.out <- amelia(
  subset(org.dta, year >= 1981), m = 10, p2s = 2,
  cs = 'cowcode', ts = 'year', polytime = 3, # intercs = TRUE,
  idvars = c('geddes_casename', 'geddes_military', 'banks_instability'),
  noms = c(
    'geddes_personal', 'geddes_monarch', 
    'geddes_party', 'coldwar', 'election'
  ),
  sqrts = c(
    'powthy_pastsuccesses', 'archigos_pastleaderfail',
    'prio_conflict_intra', 'prio_conflict_inter'
  ),
  ords = c('cooptation', 'fh_ordinal', 'flip_ciri_phys'),
  bounds = bds,
  parallel = 'multicore'
)
## Imputation diagnostics
# amelia.out                                ## chain lengths stable
# plot(amelia.out, which.vars = 'cooptation')     ## 2 few imp on 3
# plot(amelia.out, which.vars = 'flip_ciri_phys')    ## skewed left
# plot(amelia.out, which.vars = 'fh_ordinal')            ## perfect

# overimpute(amelia.out, var = 'cooptation')                ## fine
# overimpute(amelia.out, var = 'flip_ciri_phys') ## prob lower tail
# overimpute(amelia.out, var = 'fh_ordinal')       ## prob low tail

# disperse(amelia.out, dims = 1, m = 10)               ## converged
# disperse(amelia.out, dims = 2, m = 10)               ## converged

summary(amelia.out[['imputations']][[1]])

## --- create time lead variables ------------------------------- 
org.dta[, 'cooptation_discrete'] <- factor(       ## discrete iv
  org.dta[, 'cooptation'],
  levels = 0:3, 
  labels = c('NoLegPar', 'NoLegLeast1Par', 'Leg1Par', 
    'legMore1Par'
  )
)

for(i in 1:length(erDvs)) {       ## loop over empowerment rights
  org.dta[, erDvs[i]] <- ave(
    org.dta[, 'fh_ordinal'], org.dta[, 'cowcode'],
    FUN = function(x) { Hmisc::Lag(x, shift = -i) }
  )
  org.dta[, erDvs[i]] <- factor(
    org.dta[, erDvs[i]], levels = 2:7, ordered = TRUE
  )
}
for(i in 1:length(piDvs)) {       ## loop over physical integrity
  org.dta[, piDvs[i]] <- ave(
    org.dta[, 'flip_ciri_phys'], org.dta[, 'cowcode'],
    FUN = function(x) { Hmisc::Lag(x, shift = -i) }
  )
  org.dta[, piDvs[i]] <- factor(
    org.dta[, piDvs[i]], levels = 0:8, ordered = TRUE
  )
}
rm(i)

## --- create time lead in imputations --------------------------
for(i in 1:length(names(amelia.out[['imputations']]))){
  amelia.out[['imputations']][[i]][, 'lead1_fh'] <- ave(
    amelia.out[['imputations']][[i]][, 'fh_ordinal'], 
    amelia.out[['imputations']][[i]][, 'cowcode'],
    FUN = function(x) { Hmisc::Lag(x, shift = -1) }
  )
  amelia.out[['imputations']][[i]][, 't'] <- amelia.out[['imputations']][[i]][, 'year']-1989
  amelia.out[['imputations']][[i]][, 'tsqu'] <- amelia.out[['imputations']][[i]][, 't']^2
}
summary(amelia.out[['imputations']][[1]])
## --- finishing ------------------------------------------------
save(org.dta, file = file.path(pathOut, 'orgData.RData'))
## END