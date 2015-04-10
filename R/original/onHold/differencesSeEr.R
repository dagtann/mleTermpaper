## --- Problem spotting #1 --------------------------------------
## --- Big differences between classical and robust SE? ---------
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
  'Current PIR repression', 'Cut 2|3', 'Cut 3|4', 'Cut 4|5', 
  'Cut 5|6', 'Cut 6|7'
)
## --- Coefficient matrix ---------------------------------------
## empty matrix
ncoef <- length(coef(er.replicate[[1]][[1]][[1]]))
ntau <- length(er.replicate[[1]][[1]][[1]][['zeta']])
erSeMat <- matrix(NA, 
  nrow = ncoef+ntau,
  ncol = length(er.replicate),
  dimnames = list(
    names(diag(vcov(er.replicate[[1]][[1]][[1]]))),
    c('t1', 't2', 't3', 't4', 't5')
  )
)
erSeRobMat <- erSeMat

## fill coefficients and cutpoints
for(i in 1:length(erDvs)){ 
  erSeMat[, i] <- rowMeans(
    sapply(
      er.replicate[[i]], function(x) { sqrt(diag(vcov(x[[1]]))) }
    )
  )
}
## fill in cluster robust se
for(i in 1:length(erDvs)){
  erSeRobMat <- rowMeans(
    sapply(
      er.replicate[[i]], function(x) {sqrt(diag(x[[2]]))}
    )
  )
}
test.df <- as.data.frame(erSeRobMat/erSeMat)    ## rel dif in ses

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
      'flip_ciri_phys', '2|3', '3|4', '4|5', '5|6', '6|7'
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

## --- Proceed to plotting --------------------------------------
p <- ggplot(
  data = test.df,
  aes(
    x = reorder(vars, orderLabs, decrease = TRUE), 
    y = relDif, shape = variable,
  )
) +
geom_point() +
geom_hline(yintercept = 1, linetype = 2) +
scale_y_continuous(limits = c(0.5, 2.75), breaks = seq(.5, 2.5, .5)) +
scale_shape_manual(values = c(15, 17, 18, 19, 25),
  labels = paste('t', 1:5, sep = '+') 
) +
labs(
  x = '', 
  y = expression(bar('SE')['Robust']%.%bar('SE')['Classic']^-1),
  shape = 'Empowerment Rights Repression at'
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
ggsave(plot = p, file = file.path(pathOut, 'difSeEr.pdf'),
  width = 9, height = 9/1.618, dpi = 1200, family = 'serif'
)

## --- finish ---------------------------------------------------
rm(p, test.df, ncoef, ntau, erSeMat, erSeRobMat, covar.labs, i)
## END