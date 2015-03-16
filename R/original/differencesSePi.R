## --- Problem spotting #1 --------------------------------------
## --- Physical Integrity Rights --------------------------------
## --- Big differences between classical and robust SE? ---------

## --- Preliminaries --------------------------------------------
load(file.path(pathOut, 'omodels.RData'))
covar.labs <- c('Current ER Repression', 'Co-optation', # row lab
  'Civil war', 'Interstate war', 'log(Population)', 
  'log(GDP/capita)', 'Personalist Regime', 'Monarchy', 
  'Dominant party', 'Trade in Percent of GDP', 'Cold War',
  'Growth in GDP/capita', 
  'Leader Duration', 'Squared Leader Duration', 
  'Cubed Leader Duration', 'Past leader failures', 
  'Past coups', 'log(Oil rents)', 'Election year', 
  'General strikes', 'Riots', 'Antigovernment protests',
  'Current PIR repression', 'Cut 0|1', 'Cut 1|2', 'Cut 2|3', 
  'Cut 3|4', 'Cut 4|5', 'Cut 5|6', 'Cut 6|7', 'Cut 7|8'
)

## --- Coefficient matrix ---------------------------------------
## empty matrix
ncoef <- length(coef(pi.replicate[[1]][[1]][[1]]))
ntau <- length(
  pi.replicate[[1]][[1]][[1]][['zeta']]
  )
piSeMat <- matrix(NA, 
  nrow = ncoef+ntau,
  ncol = length(pi.replicate),
  dimnames = list(
    names(diag(vcov(pi.replicate[[1]][[1]][[1]]))),
    c('t1', 't2', 't3', 't4', 't5')
  )
)
piSeRobMat <- piSeMat

## fill coefficients and cutpoints
for(i in 1:length(piDvs)){ 
  piSeMat[, i] <- rowMeans(
    sapply(
      pi.replicate[[i]], function(x) { sqrt(diag(vcov(x[[1]]))) }
    )
  )
}
## fill in cluster robust se
for(i in 1:length(piDvs)){
  piSeRobMat <- rowMeans(
    sapply(
      pi.replicate[[i]], function(x) {sqrt(diag(x[[2]]))}
    )
  )
}
test.df <- as.data.frame(piSeRobMat/piSeMat)     ## rel dif in se

## --- Prepare plotting data.frame ------------------------------
test.df[, 'vars'] <- rownames(test.df)
test.df <- within(test.df, {
  vars <- factor(vars, 
    levels = c(
      'fh_ordinal', 'cooptation', 'prio_conflict_intra', 
      'prio_conflict_inter', 'gled_lpop', 'gled_lgdppc', 
      'geddes_personalYes', 'geddes_monarchYes', 'geddes_partyYes', 
      'wdi_tradegdp', 'coldwarYes', 'wdi_gdppcgrowth', 
      'archigos_durationLin', 'archigos_durationSqu', 
      'archigos_durationCub', 'archigos_pastleaderfail', 
      'powthy_pastattempts', 'pseudologross', 'electionYes', 
      'banks_genstrike', 'banks_riot', 'banks_antigovdem', 
      'flip_ciri_phys', '0|1', '1|2', '2|3', '3|4', '4|5', '5|6', 
      '6|7', '7|8'
    ),
  labels = covar.labs
  )
  orderLabs <- nrow(test.df):1
  }
)
test.df <- reshape2::melt(
  test.df, 
  id.vars = c('vars', 'orderLabs'),
  measure.vars = paste0('t', 1:5),
  value.name = 'relDif'
)

## --- proceed to plotting --------------------------------------
p <- ggplot(
  data = test.df,
  aes(
    x = reorder(vars, orderLabs, decrease = TRUE), 
    y = relDif, shape = variable,
  )
) +
geom_point() +
geom_hline(yintercept = 1, linetype = 2) +
scale_shape_manual(values = c(15, 17, 18, 19, 25),
  labels = paste('t', 1:5, sep = '+') 
) +
scale_y_continuous(
  limits = c(0.5, 2.75), breaks = seq(.5, 2.5, .5)
) +
labs(
  x = '', 
  y = expression(bar('SE')['Robust']%.%bar('SE')['Classic']^-1),
  shape = 'Physical Integrity Violations at'
) +
theme_bw() +
theme(
  legend.position = 'top',
  legend.direction = 'horizontal',
  legend.key = element_blank(),
  axis.ticks.y = element_blank(),
  panel.border = element_blank()
) +
coord_flip()
ggsave(plot = p, file = file.path(pathOut, 'difSePi.pdf'),
  width = 9, height = 9/1.618, dpi = 1200, family = 'serif'
)

## --- finish ---------------------------------------------------
rm(
  p, test.df, ncoef, ntau, piSeMat, piSeRobMat, covar.labs, i
)
## END