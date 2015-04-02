## Simulation of ordered logit data
 load(file.path(pathOut, 'omodels.RData'))

## --- setup scenario data -----------------------------------------
scenario.dta <- expand.grid(
  fh_ordinal = mean(dta.list[[1]][, 'fh_ordinal'], na.rm = TRUE),
  cooptation = c(0, 3),  ##           
  prio_conflict_intra = mean(dta.list[[1]][, 'prio_conflict_intra'], na.rm = TRUE),
  prio_conflict_inter = mean(dta.list[[1]][, 'prio_conflict_inter'], na.rm = TRUE),
  gled_lpop = mean(dta.list[[1]][, 'gled_lpop'], na.rm = TRUE),
  gled_lgdppc = mean(dta.list[[1]][, 'gled_lgdppc'], na.rm = TRUE),
  geddes_personal = 1,
  geddes_monarch = 0,
  geddes_party = 0,
  wdi_tradegdp = mean(dta.list[[1]][, 'wdi_tradegdp'], na.rm = TRUE),
  coldwar = 1,
  wdi_gdppcgrowth = mean(dta.list[[1]][, 'wdi_gdppcgrowth'], na.rm = TRUE),
  archigos_durationLin = mean(dta.list[[1]][, 'archigos_durationLin'], na.rm = TRUE),
  archigos_durationSqu = mean(dta.list[[1]][, 'archigos_durationSqu'], na.rm = TRUE),
  archigos_durationCub = mean(dta.list[[1]][, 'archigos_durationCub'], na.rm = TRUE),
  archigos_pastleaderfail = mean(dta.list[[1]][, 'archigos_pastleaderfail'], na.rm = TRUE),
  powthy_pastattempts = mean(dta.list[[1]][, 'powthy_pastattempts'], na.rm = TRUE),
  pseudologross = mean(dta.list[[1]][, 'pseudologross'], na.rm = TRUE),
  election = 0,
  banks_genstrike = mean(dta.list[[1]][, 'banks_genstrike'], na.rm = TRUE),
  banks_riot = mean(dta.list[[1]][, 'banks_riot'], na.rm = TRUE),
  banks_antigovdem = mean(dta.list[[1]][, 'banks_antigovdem'], na.rm = TRUE),
  flip_ciri_phys = mean(dta.list[[1]][, 'flip_ciri_phys'], na.rm = TRUE)
)
scenario.dta <- as.matrix(scenario.dta)

## --- simluate coefficients ---------------------------------------
coef.list <- lapply(         ## pull and average all coefficients
  pi.replicate, function(dv) { 
    rowMeans(
      sapply(dv, function(imp) { coef(imp[[1]]) } ) 
    )
  }
)
tau.list <- lapply(                  ## pull and average all taus
  pi.replicate, function(dv) { 
    rowMeans(
      sapply(dv, function(imp) { imp[[1]][['zeta']] } ) 
    )
  }
)
sigma.list <- lapply(              ## pull and average all sigmas
  pi.replicate, function(dv) { 
    meanArray( 
      lapply(dv, function(imp) {solve(imp[[1]][['Hessian']])}) 
    ) 
  }
)

N <- 100000                                ## number of iterations
dta <- expand.grid(             ## data.frame storing predictions
  dv = piDvs,                                    ## what dv am I?
  id = 1:N,                       ## what draw from mvrnorm am I?
  c0 = NA, c1 = NA, c2 = NA, c3 = NA, c4 = NA, c5 = NA, c6 = NA, 
  c7 = NA, c8 = NA ## dv cats
)
dta <- dta[with(dta, order(dv, id)), ]
rownames(dta) <- NULL

#i <- 'lead_flip1'
for(i in piDvs){                   ## for all dependent variables
  preds <- mvrnorm(                          ## draw coefficients
    N, c(coef.list[[i]], tau.list[[i]]), sigma.list[[i]]
  )
  ## generate predictions for either scenario
  pred.0 <- preds[, 1:length(coef.list[[i]])] %*% scenario.dta[1, ]
  pred.4 <- preds[, 1:length(coef.list[[i]])] %*% scenario.dta[2, ]
  ## convenience object
  tau <- preds[, (length(coef.list[[i]])+1):dim(preds)[2]]  
  ## generate per cent point difference in predicted probability
  dta[dta[, 'dv'] == i, 'c0'] <- plogis(tau[, 1] - pred.4) - plogis(tau[, 1] - pred.0)
  dta[dta[, 'dv'] == i, 'c1'] <- plogis(tau[, 2] - pred.4) - plogis(tau[, 1] - pred.4) - (plogis(tau[, 2] - pred.0) - plogis(tau[, 1] - pred.0))
  dta[dta[, 'dv'] == i, 'c2'] <- plogis(tau[, 3] - pred.4) - plogis(tau[, 2] - pred.4) - (plogis(tau[, 3] - pred.0) - plogis(tau[, 2] - pred.0))
  dta[dta[, 'dv'] == i, 'c3'] <- plogis(tau[, 4] - pred.4) - plogis(tau[, 3] - pred.4) - (plogis(tau[, 4] - pred.0) - plogis(tau[, 3] - pred.0))
  dta[dta[, 'dv'] == i, 'c4'] <- plogis(tau[, 5] - pred.4) - plogis(tau[, 4] - pred.4) - (plogis(tau[, 5] - pred.0) - plogis(tau[, 4] - pred.0))
  dta[dta[, 'dv'] == i, 'c5'] <- plogis(tau[, 6] - pred.4) - plogis(tau[, 5] - pred.4) - (plogis(tau[, 6] - pred.0) - plogis(tau[, 5] - pred.0))
  dta[dta[, 'dv'] == i, 'c6'] <- plogis(tau[, 7] - pred.4) - plogis(tau[, 6] - pred.4) - (plogis(tau[, 7] - pred.0) - plogis(tau[, 6] - pred.0))
  dta[dta[, 'dv'] == i, 'c7'] <- plogis(tau[, 8] - pred.4) - plogis(tau[, 7] - pred.4) - (plogis(tau[, 8] - pred.0) - plogis(tau[, 7] - pred.0))
  dta[dta[, 'dv'] == i, 'c8'] <- (1 - plogis(tau[, 8] - pred.4)) - (1 - plogis(tau[, 8] - pred.0))
}
rm(tau, pred.0, pred.4, preds, N, coef.list, sigma.list, tau.list)
## --- proceed to plotting --------------------------------------
dta.md <- reshape2::melt(                         ## reshape data
  dta, id.vars = c('dv', 'id'), measure.vars = paste0('c', 0:8)
)
dta.md <- within(dta.md, {            ## generate speaking labels
  dv <- factor(dv, levels = piDvs, labels = paste0('PI at t+', 1:5))
  variable <- factor(variable,
    levels = paste0('c', 0:8), labels = paste(0:8)
  )
  }
)

p <- ggplot(dta.md, aes(x = value)) +
geom_line(stat = 'density') +
geom_vline(xintercept = 0, linetype = 'longdash') +
labs(y = 'Density', x = expression(
    paste(Delta, plain(Pr)(plain(PI)[t+i]==x))
  )
) +
theme_minimal() +
theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
facet_wrap(dv ~ variable, scales = 'free_y', nrow = 5)

library('gridExtra')               ## Add labeling note at bottom
sub.label <- textGrob(                 
  'Per cent point change in probability of category given maximal change in co-optation; 100,000 iterations.', 
  gp = gpar(fontsize = 10), x = unit(1, "npc"), hjust = 1, vjust = 0
)
ggsave(                                    ## save merged figures
  filename = file.path(pathOut, 'simulationMaximumDifferencePI.pdf'),
  plot = arrangeGrob(p, sub = sub.label, clip = FALSE),
  scale = 1, width = 14, height = 14/1.618, dpi = 1200,
  family = 'serif'
)
detach(package:gridExtra)

## --- finishing ------------------------------------------------
dev.off()
rm(p, sub.label, dta, scenario.dta)
## END