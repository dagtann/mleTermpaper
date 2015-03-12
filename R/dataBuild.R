## --- Load data ------------------------------------------------
dta.files <- dir(pathData)[                 ## list of data files
  grep(x = dir(pathData), pattern = "old", fixed = TRUE)
]
dta.list <- list()

library('foreign')
for(i in 1:length(dta.files)) {
  dta.list[[i]] <- read.dta(
    file = file.path(pathData, dta.files[i]),
    convert.factors = FALSE,
    convert.dates = TRUE,
    convert.underscore = FALSE
  )
  dta.list[[i]] <- with(dta.list[[i]], 
    dta.list[[i]][ order(cowcode, year), ]
  )
  names(dta.list)[i] <- dta.files[i]
}
detach(package:foreign)

erDvs <- paste0('lead_fh', 1:5)         ## empowerment rights dvs

## --- clean original data --------------------------------------
dta.list[[2]] <- dta.list[[2]][  ## Remove single row with all NA
  -which(is.na(dta.list[[2]][, 'archigos_duration'])), 
]

for(i in 1:length(dta.files)){
  for(j in erDvs){                   ## dep var to ordered factor
    dta.list[[i]][, j] <- factor(
      dta.list[[i]][, j], levels = 2:7, ordered = TRUE
    )
  }
  dta.list[[i]][, 'geddes_party'] <- factor( ## reg types to fact
    dta.list[[i]][, 'geddes_party'], 
    levels = 0:1, labels = c('No', 'Yes')
  )
  dta.list[[i]][, 'geddes_personal'] <- factor(
    dta.list[[i]][, 'geddes_personal'], 
    levels = 0:1, labels = c('No', 'Yes')
  )
  dta.list[[i]][, 'geddes_monarch'] <- factor(
    dta.list[[i]][, 'geddes_monarch'], 
    levels = 0:1, labels = c('No', 'Yes')
  )
  dta.list[[i]][, 'geddes_military'] <- factor(
    dta.list[[i]][, 'geddes_military'], 
    levels = 0:1, labels = c('No', 'Yes')
  )
  dta.list[[i]][, 'cooptation_discrete'] <- factor( ## discrete iv
    dta.list[[i]][, 'cooptation'],
    levels = 0:3, 
    labels = c('NoLegPar', 'NoLegLeast1Par', 'Leg1Par', 
      'legMore1Par'
    )
  )
  dta.list[[i]][, 'coldwar'] <- factor(         ## cold war dummy
    dta.list[[i]][, 'coldwar'], 
    levels = 0:1, labels = c('No', 'Yes')
  )
  dta.list[[i]][, 'election'] <- factor(   ## election year dummy
    dta.list[[i]][, 'election'], 
    levels = 0:1, labels = c('No', 'Yes')
  )
  agePoly <- poly(             ## orthogonal duration polynomials
    dta.list[[i]][, 'archigos_duration'], degree = 3
  )
  colnames(agePoly) <- paste0(
    'archigos_duration', c('Lin', 'Squ', 'Cub')
  )
     ## NOTE: Models won't converge if not uncorrelated age terms
  dta.list[[i]] <- data.frame(dta.list[[i]], agePoly)
}
rm(i, j, agePoly)

## --- Note ------------------------------------------------------
table(dta.list[[1]][, 'prio_conflict_intra']) 
table(dta.list[[1]][, 'prio_conflict_inter']) 
## These count conflicts per year! Description slightly ambiguous
## different in publication.

## --- Save results ---------------------------------------------
save.image(file.path(pathOut, 'base.RData'))
## END