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
