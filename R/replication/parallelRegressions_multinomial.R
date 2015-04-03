load(file.path(pathOut, 'omodels.RData'))
packs <- c('nnet', 'parallel')
invisible(lapply(packs, library, character.only = TRUE))

## --- set up parallel workspace --------------------------------
numWorkers <- 4
mnomFit <- function(dv, ivs, dta){    ## multinomial fit function
  form <- as.formula(paste0(dv,'~',ivs))
  fit <- multinom(form, data = dta, Hess = TRUE, maxit = 500)
  return(fit)
}

## --- run multinomial alternative models -----------------------
## Attention: You are about to run bottleneck procedures.
mnom.er <- vector('list', length = length(erDvs))
names(mnom.er) <- erDvs
for(dv in erDvs) {
  mnom.er[[dv]] <- mclapply(
    dta.list, 
    function(x) { mnomFit(dv = dv, ivs = ivTerms, dta = x) },
    mc.cores = numWorkers, mc.cleanup = TRUE
  ) 
}
summary(mnom.er)                                   ## debug check
lapply(mnom.er, summary)
mnom.pi <- vector('list', length = length(piDvs))
names(mnom.pi) <- piDvs
for(dv in piDvs) {
  mnom.pi[[dv]] <- mclapply(
    dta.list, 
    function(x) { 
      mnomFit(dv = dv, ivs = ivTerms, dta = x) 
    },
    mc.cores = numWorkers, mc.cleanup = TRUE
  ) 
}
summary(mnom.pi)                                   ## debug check
lapply(mnom.pi, summary)

## --- generate data.frame for point comparisons ----------------
assum.dta <- expand.grid(dv = c(erDvs, piDvs), imp = 1:5)
assum.dta <- assum.dta[with(assum.dta, order(dv, imp)), ]

pull <- function(from, what) {             ## extraction function
  data.matrix <- sapply(from, 
    function(dv){ 
      sapply(dv, 
        function(imp){ imp[[1]][[what]] }
      )
    }
  )
  data.matrix <- reshape2::melt(
    data.matrix, 
    id.vars = row.names(data.matrix), 
    measure.vars = 1:ncol(data.matrix),
    value.name = what
  )
  return(data.matrix[, 3])
}
assum.dta[, 'deviance.polr'] <- pull(
  from = c(er.replicate, pi.replicate), what = 'deviance'
)
assum.dta[, 'edf.polr'] <- pull(
  from = c(er.replicate, pi.replicate), what = 'edf'
)

pull <- function(from, what) {    ## redefine for [[1]] component
  data.matrix <- sapply(from, 
    function(dv){ 
      sapply(dv, 
        function(imp){ imp[[what]] }
      )
    }
  )
  data.matrix <- reshape2::melt(
    data.matrix, 
    id.vars = row.names(data.matrix), 
    measure.vars = 1:ncol(data.matrix),
    value.name = what
  )
  return(data.matrix[, 3])
}
assum.dta[, 'edf.mnom'] <- pull(
  from = c(mnom.er, mnom.pi), what = 'edf'
)
assum.dta[, 'deviance.mnom'] <- pull(
  from = c(mnom.er, mnom.pi), what = 'deviance'
)
rm(pull)                                       ## pull not needed

assum.dta <- within(assum.dta, {## generate comparions dta
  pval <- pchisq( # bonferroni adjustment for multiple testing
    deviance.polr-deviance.mnom, 
    edf.mnom-edf.polr, 
    lower.tail = FALSE
  )
  type <- factor(ifelse(dv %in% erDvs, 'er', 'pi'))
  }
)
assum.dta[, 'pcorr'] <- ave(
  assum.dta[, 'pval'], assum.dta[, 'type'], 
  FUN = function(x) { p.adjust(x, method = 'bonferroni') }
)
head(assum.dta)
with(assum.dta, plot(density(pcorr-pval)))

correction <- function(pval){
  p.adjust(pval, method = p.adjust.methods, n = length(p))
}


pdta <- aggregate(
  assum.dta[, c('pval', 'pcorr')], 
  by = list(dv = assum.dta[, 'dv']), 
  FUN = mean
)
pdta <- within(pdta, {
  type <- ifelse(dv %in% erDvs, 'er', 'pi')
  type <- factor(type, 
    levels = c('er', 'pi'), 
    labels = c('Empowerment rights', 'Physical integrity rights')
  )
  lead <- as.character(dv)
  lead <- substr(lead, nchar(lead), nchar(lead))
  lead <- factor(
    lead, levels = 1:5, labels = paste0('t+', 1:5)
  )
  }
)

library('grid')
p <- ggplot(
  data = pdta, 
  aes(x = lead, y = pcorr, shape = type)
) + 
geom_hline(yintercept = .05, linetype = 'longdash') +
geom_point(position = position_dodge(.3)) + 
annotate(
  label = paste(expression(alpha==0.05)), x = 5, y = .05,
  vjust = -.2, hjust = 0.2, geom = 'text', parse = TRUE, family = 'serif',
  size = 3.5
) +
scale_y_continuous(breaks = c(0, .25, .5, .75, 1)) +
labs(
  y = 'P-Value',
  #expression(plain(P-Value)[plain(Bonferroni)]), 
  x = '', shape = ''
) +
theme_minimal(base_family = 'serif') +
theme(
  legend.position = c(.415, 1.06),
  legend.direction = 'horizontal',
  legend.title = element_blank(),
  axis.title.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  plot.margin = unit(c(1, 0, 0, 0)+.1, units = 'lines')
)
ggsave(plot = p, 
  file = file.path(pathOut, 'parallelRegrDevianceBonfP.pdf'),
  width = 4, height = 4/1.618, dpi = 1200
)
detach(package:grid)
## --- finishing up ---------------------------------------------
# rm(numWorkers, mnomFit, mnom.er, mnom.pi, assum.dta)