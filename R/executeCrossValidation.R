## --- Define fold objects and containers -----------------------
n.folds <- 10                       ## folds for cross-validation
## Data imputed, original data cannot be restored --> Define 
## folds once and brute force into each data set
set.seed(8590)
fold <- sample(
  1:n.folds, size = nrow(dta.list[[1]]), replace = TRUE
)
dta.list <- lapply(dta.list, function(x) {    ## order to be safe
  x[with(x, order(cowcode, year)), ]
  }
)
for(i in 1:length(dta.files)){           ## brute force add folds
  dta.list[[i]][, 'fold'] <- fold
}

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

## --- Run cross validation -------------------------------------
er.train <- vector('list', length = length(erDvs))  ## list train
names(er.train) <- erDvs
er.test <- vector('list', length = length(erDvs))    ## list test 
names(er.test) <- erDvs

fail <- 0                          ## rudimentary quality control
counter <- 0
address <- vector(length = 250)

            ## Note: You are about to run a bottleneck procedure.
for( dv in erDvs ){              ## for all empowerment right dvs
  for( j in 1:n.folds ){                         ## for all folds
    for( i in 1:length(dta.files) ){       ## for all imputations
      counter <- counter+1                        ## step counter
      print(counter)                              
      er.train[[dv]][[paste0('fold', j)]][[paste0("imp", i)]] <- 
        polr(
          as.formula(paste(dv, '~', addIvs, sep = ' ')),
          data = dta.list[[i]][dta.list[[i]][, 'fold'] != j, ],
          method = 'logistic', Hess = TRUE, model = TRUE,
          control = list(maxit = 500)  ## increase max iterations
      )
      if(         ## "Quality control": if model did not converge
        er.train[[dv]][[paste0('fold', j)]][[paste0("imp", i)]][['convergence']] != 0) {
        fail <- fail+1
        address[counter] <- 1
      }
      er.test[[dv]][[paste0('fold', j)]][[paste0("imp", i)]] <- 
        predict(         ## predicted probabilities for test data
          er.train[[dv]][[paste0('fold', j)]][[paste0("imp", i)]],
          newdata = dta.list[[i]][dta.list[[i]][, 'fold'] == j, ],
          type = 'prob'
      )
    }
  }
}
rm(i, j, dv)

counter                             ## how many models are there?
table(fail)                                ## did any model fail?
table(address)                  ## what was the iteration number?
rm(address, fail, counter)
## --- save results ---------------------------------------------
save.image(file.path(pathOut, 'models.Rdata'))
## END