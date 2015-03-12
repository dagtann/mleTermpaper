## === coefficient plot across all models =======================
load(file.path(pathOut, 'models.Rdata'))

## --- Extract coefficients -------------------------------------
## coef & se containers
ncoef <- length(coef(er.train[[1]][[1]][[1]]))
ntau <- length(er.train[[1]][[1]][[1]][['zeta']])
varnames <- c(
  names(coef(er.train[[1]][[1]][[1]])),
  names(er.train[[1]][[1]][[1]][['zeta']])
)
coefList <- lapply(erDvs, function(x) {
  matrix(
    NA, 
    ncol = n.folds, nrow = ncoef + ntau, 
    dimnames = list(varnames, 1:n.folds)
  )
  }
)
names(coefList) <- erDvs
seList <- coefList

for(dv in 1:5){                 ## extract & average coefficients
  for(j in 1:10){
  coefList[[dv]][, j]  <- rowMeans(
    sapply(
      er.train[[dv]][[j]], function(x){ c(coef(x), x[['zeta']]) }
    )
  )
  }
}

for(dv in 1:5){              ## extract & average standard errors
  for(j in 1:10){
  seList[[dv]][, j]  <- rowMeans(
    sapply(
      er.train[[dv]][[j]], 
      FUN = function(x){ sqrt(diag(solve(x[['Hessian']]))) } 
    )
  )
  }
}
rm(dv, j)

## --- plot effects ----------------------------------------
pdta <- do.call(rbind.data.frame, coefList)
listNames <- row.names(pdta)
listNames <- strsplit(listNames, '.', fixed = TRUE)
pdta <- within(pdta, {
  depVar <- sapply(listNames, "[", 1)
  varNames <- sapply(listNames, "[", 2)
  }
)
pdta <- reshape2::melt(
  data = pdta,
  id.vars = c('depVar', 'varNames'),
  measure.vars = as.character(1:10),
  value.name = 'coef'
)
sedta <- do.call(rbind.data.frame, seList)
sedta <- within(sedta, {
  depVar <- sapply(listNames, "[", 1)
  varNames <- sapply(listNames, "[", 2)
  }
)
sedta <- reshape2::melt(
  data = sedta,
  id.vars = c('depVar', 'varNames'),
  measure.vars = as.character(1:10),
  value.name = 'se'
)
pdta <- merge(
  x = pdta, y = sedta,
  by = c('depVar', 'varNames', 'variable')
)
pdta[, 'varNames'] <- factor(pdta[, 'varNames'],
  levels = c('fh_ordinal', 'cooptation', 'prio_conflict_intra', 
  'prio_conflict_inter', 'gled_lpop', 'gled_lgdppc', 
  'geddes_personalYes', 'geddes_monarchYes', 'geddes_partyYes', 
  'wdi_tradegdp', 'coldwarYes', 'wdi_gdppcgrowth', 
  'archigos_durationLin', 'archigos_durationSqu', 
  'archigos_durationCub', 'archigos_pastleaderfail', 
  'powthy_pastattempts', 'pseudologross', 'electionYes', 
  'banks_genstrike', 'banks_riot', 'banks_antigovdem', 
  'flip_ciri_phys', '2|3', '3|4', '4|5', '5|6', '6|7'
),
labels = c(
  'ER repression at t0', 'Co-optation', 'Civil wars', 
  'Interstate wars',
  'log(Population size)', 'log(GDP/capita)', 'Personal regime',
  'Monarchy', 'Dominant party regime', 'Trade (% GDP)', 'Cold War',
  'Growth in GDP/capita', 'Leader tenure', 'Squared leader tenure',
  'Cubed leader tenure', 'Past leadership fails', 'Past coups',
  'log(Oil rents)', 'Election year', 'General strikes', 'Riots',
  'Anti-gov. Demonstr.', 'PIR repression', 'Cut 2|3',
  'Cut 3|4', 'Cut 4|5', 'Cut 5|6', 'Cut 6|7'
  )
)
for(i in unique(pdta[, 'depVar'])){
  p <- ggplot(
    data = subset(pdta, depVar == i), 
    aes(x = variable, y = coef)
  ) +      
  geom_point() +
  geom_linerange(
    aes(ymin = coef-qnorm(.025)*se, ymax = coef+qnorm(.025)*se),
    size = .3, linetype = 'longdash'
  ) +
  scale_shape_calc() +
  labs(
    title = i,
    x = 'k-Fold',
    y = 'Estimated coefficient'
  ) +
  theme_bw() +
  facet_wrap(~ varNames, scale = 'free_y') +
  theme(
    legend.position = 'top',
    legend.direction = 'horizontal',
    panel.border = element_blank()
  )
  ggsave(
    plot = p, 
    file = paste0(pathOut, '/figCoefPlotKfold_', i, '.pdf'),
    width = 12, height = 12/1.618, family = 'serif'
  )
}
rm(p, pdta, sedta, coefList, seList)
## END