## This script compares BIC scores of full and bare bones models
## Outputs a graph
## ==============================================================
load(file.path(pathOut, 'omodels.RData'))

library('parallel')                ## prepare parallel processing
numWorkers <- 4
ivTerms.org <- ivTerms

## --- physical integrity rights --------------------------------
bic.pi <- lapply(                       ## extract full model bic
  pi.replicate, function(dv) {
    sapply(dv, function(imp) { BIC(imp[[1]])})
  }
)
bic.pi <- do.call(data.frame, bic.pi)
bic.pi[, 'imp'] <- row.names(bic.pi)
bic.pi <- reshape2::melt(
  bic.pi, id.vars = 'imp', measure.vars = 1:5, value.name = 'bic.full'
)

ivTerms <- 'flip_ciri_phys'         ## compute lagged dv only bic
pi.bare <- vector('list', length = length(piDvs))
names(pi.bare) <- piDvs
for(dv in piDvs){          ## implicit looping over dv in formula
  pi.bare[[dv]] <- mclapply(
    dta.list, mcpolr, 
    mc.cores = numWorkers, mc.cleanup = TRUE
  )
}
rm(dv)
summary(pi.bare[[1]][[1]][[1]])             ## check model output

bic.pi.bare <- lapply(pi.bare, function(dv) {      ## extract bic
    sapply(dv, function(imp) { BIC(imp[[1]])})
  }
)
bic.pi.bare <- do.call(data.frame, bic.pi.bare) ## create plot dta
bic.pi.bare[, 'imp'] <- row.names(bic.pi.bare)
bic.pi.bare <- reshape2::melt(
  bic.pi.bare, id.vars = 'imp', 
  measure.vars = 1:5, value.name = 'bic.bare'
)
bic.pi.dta <- merge(bic.pi, bic.pi.bare)
rm(bic.pi.bare, bic.pi, pi.bare)

## --- empowerment rights ---------------------------------------
bic.er <- lapply(er.replicate, function(dv) { ## extract full bic
  sapply(dv, function(imp) { BIC(imp[[1]])})
  }
)
bic.er <- do.call(data.frame, bic.er)
bic.er[, 'imp'] <- row.names(bic.er)
bic.er <- reshape2::melt(
  bic.er, id.vars = 'imp', measure.vars = 1:5, value.name = 'bic.full'
)

ivTerms <- 'fh_ordinal'                     ## compute bare model
er.bare <- vector('list', length = length(erDvs))
names(er.bare) <- erDvs
for(dv in erDvs){          ## implicit looping over dv in formula
  er.bare[[dv]] <- mclapply(
    dta.list, mcpolr, 
    mc.cores = numWorkers, mc.cleanup = TRUE
  )
}
summary(er.bare[[1]][[1]][[1]])             ## output model check
rm(dv)

bic.er.bare <- lapply(er.bare, function(dv) { ## extract bare bic
    sapply(dv, function(imp) { BIC(imp[[1]])})
  }
)
bic.er.bare <- do.call(data.frame, bic.er.bare)  ## create plot dta
bic.er.bare[, 'imp'] <- row.names(bic.er.bare)
bic.er.bare <- reshape2::melt(
  bic.er.bare, id.vars = 'imp', 
  measure.vars = 1:5, value.name = 'bic.bare'
)
bic.er.dta <- merge(bic.er, bic.er.bare)
rm(bic.er, bic.er.bare, er.bare)

## --- proceed to plotting --------------------------------------
bic.pdta <- rbind(bic.pi.dta, bic.er.dta) ## combine data for plot
rm(bic.er.dta, bic.pi.dta)

bic.pdta <- with(bic.pdta, aggregate( 
    bic.pdta[, c('bic.full', 'bic.bare')], 
    by = list(variable = variable),
    FUN = mean
  )
)
bic.pdta <- within( bic.pdta, {
  lead <- as.character(variable)
  lead <- substr(lead, nchar(lead), nchar(lead))
  type <- ifelse(variable %in% erDvs, 'er', 'pi')
  type <- factor(type, 
    levels = c('er', 'pi'), 
    labels = c('Empowerment rights', 'Physical integrity rights')
  )
  }
)
bic.pdta[, 'delta'] <- with(bic.pdta, bic.bare-bic.full)

library('grid'); library('RColorBrewer')
colors <- brewer.pal(n = 5, name = 'Dark2')

p <- ggplot(data = bic.pdta, 
  aes(x = bic.full, y = delta, color = as.factor(lead))
) +
geom_hline(yintercept = 0, linetype = 'longdash') +
geom_point(size = 3) +

scale_y_continuous(limits = c(-125, 125)) +
labs(
  x = expression(plain(BIC)[Full]), 
  y = expression(
    plain(BIC)[plain(Lagged~Response)]-plain(BIC)[plain(Full)]
  ),
  colour = 'Lead', shape = 'Repression of'
) +
scale_colour_manual(
  values = colors,
  labels = paste('t', 1:5, sep = '+')
) +
facet_wrap(~type, scale = 'free_x') +
theme_minimal() +
theme(
  legend.position = c(.19, 1.09),
  legend.direction = 'horizontal',
  legend.background = element_rect(fill = 'transparent', colour = 'transparent'),
  legend.key = element_blank(),
  legend.title = element_blank(),
  plot.margin = unit(c(1,.7,0,0)+.1, units = 'lines')
)
ggsave(
  plot = p, file = file.path(pathOut, 'aicDifferences.pdf'),
  width = 7, height = 7/1.618, dpi = 1200, family = 'sans'
)

## --- Finishing ------------------------------------------------
detach(package:grid)
detach(package:parallel)
ivTerms <- ivTerms.org
rm(numWorkers, ivTerms.org, p, colors)
## END