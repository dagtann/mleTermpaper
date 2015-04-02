## This script loads and prepares the unimputed data for
## analysis
## --- Load data ------------------------------------------------
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
org.dta[, 'cooptation_discrete'] <- factor(       ## discrete iv
  org.dta[, 'cooptation'],
  levels = 0:3, 
  labels = c('NoLegPar', 'NoLegLeast1Par', 'Leg1Par', 
    'legMore1Par'
  )
)
org.dta[, 'coldwar'] <- factor(org.dta[, 'coldwar'],   ## cold war
  levels = 0:1, labels = c('No', 'Yes')
)
org.dta[, 'election'] <- factor(org.dta[, 'election'], ## election
  levels = 0:1, labels = c('No', 'Yes')
)

## --- create time lead variables ------------------------------- 
org.dta <- org.dta[with(org.dta, order(cowcode, year)), ] # sort
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
## --- finishing ------------------------------------------------
save(org.dta, file = file.path(pathOut, 'orgData.RData'))
## END