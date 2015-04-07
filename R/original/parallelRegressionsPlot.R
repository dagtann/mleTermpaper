## --- Preliminaries --------------------------------------------
load(file.path(pathOut, 'omodels.RData'))
glm.refit <- function(data){
  ## Compute model
  mod <- glm(as.formula(paste0(paste0('ybin', i), '~', ivTerms)),
    data = data,
    family = 'binomial',
    control = list(maxit = 500)        ## increase max iterations
  )
  ## Compute cluster robus standard errors
  ## mod[['model']][, 'cowcode'] 
  cluster.code <- data[-mod[['na.action']], 'cowcode']
  varCluster <- clusterRobustSe(
    fm = mod, cluster = cluster.code
  )
  return(list(model = mod, varCluster = varCluster))
}

## --- Define reference/storage for loop ------------------------
ylevels <- sapply(
  dta.list[[1]][, c(erDvs, piDvs)], 
  function(x) { 
    na.omit(sort(unique(as.numeric(as.character(x))))) 
  }
)
coef.dta <- expand.grid(
  dv = c(erDvs, piDvs),  imp = paste0('imp', 1:5), ylevel = 0:8, 
  coef = NA, se = NA, perfect = NA
)
drop <- with(coef.dta, which(dv %in% erDvs & (ylevel <= 1 | ylevel > 7)))
coef.dta <- coef.dta[-drop, ]
coef.dta <- coef.dta[with(coef.dta, order(dv, imp, ylevel)), ]
row.names(coef.dta) <- NULL

## --- run parallel regression ----------------------------------
## You are about to run a bottleneck procedure with 225 regressions
count <- 0                        ## counter for rows in coef.dta
options(warn = 1)       ## immediately output warnings if occured
for(dv in c(erDvs, piDvs)){                       ## for every DV
  for(j in 1:length(dta.list)){       ## go through each imp data
    dta <- dta.list[[j]]                          ## pull out dta
                ## slows loop, but avoids modifying original data       
    for(i in ylevels[[dv]][-1]){          ## exclude lowest level
      ## i <- 1 ## debug perfect separation
      ## dv <- 'lead_flip2' ## debug perfect separation
      count <- count+1     ## step + 1, id's row in results frame
      na.count <- which(is.na(dta[, dv])) ## record NA case address
      dta[, paste0('ybin', i)] <- ifelse(     ## gen binary for all  
        i <= as.numeric(as.character(dta[, dv])), 1, 0
      )
      # Test recoding
      # print(paste0('I am here:', i))
      # print(table(dta[, dv], dta[, paste0('ybin', i)]))
      dta[na.count, paste0('ybin', i)] <- NA     ## restore missing
      ## Debug print
      print(count); print(dv); print(paste0('dta', j)); print(paste0('ybin', i))
      fit <- glm.refit(dta)
      slot <- which(
        coef.dta[, 'dv'] == dv & 
        coef.dta[, 'imp'] == names(dta.list)[j] & 
        coef.dta[, 'ylevel'] == i
      )
      coef.dta[slot, 'coef'] <- coef(fit[['model']])['cooptation']  ## coef
      coef.dta[slot, 'se'] <- sqrt(diag(fit[['varCluster']]))[['cooptation']] ## se
      coef.dta[slot, 'perfect'] <- ifelse(
        0.00000000000001 > min(fitted(fit[['model']])) |
        0.99999999999999 < max(fitted(fit[['model']])),
        1, 0
      ) 
    }
  }
}
rm(fit, count, ylevels)
summary(coef.dta)
with(coef.dta, table(ylevel, perfect))

## --- prepare plotting -----------------------------------------
coef.dta <- within(coef.dta, {
  lo95 <- coef - qnorm(.975)*se
  up95 <- coef + qnorm(.975)*se
  }
)
tmp <- subset(coef.dta, !is.na(coef))
tmp <- tmp[with(tmp, order(dv, ylevel, imp)), ]
drop <- with(tmp, 
  which(ave(perfect, dv, ylevel, FUN = function(x){ any(x==1) }) == 1)
)
tmp <- tmp[-drop, ]
tmp <- with(tmp,
  aggregate(
    tmp[, c('coef', 'se', 'up95', 'lo95')],
    by = list(dv = dv, ylevel = ylevel),
    FUN = mean
  )
)
tmp <- within(tmp, {
  lead <- as.character(dv)
  lead <- substr(lead, nchar(lead), nchar(lead))
  type <- ifelse(as.character(dv) %in% erDvs, 'er', 'pi')
  type <- factor(
    type, levels = c('er', 'pi'), 
    labels = c('Empowerment rights', 'Physical integrity rights')
  )
  }
)
drop <- with(tmp, which(dv == 'lead_fh2' & ylevel == 3))
tmp <- tmp[-drop, ]

dodge <- .65
lab <- c('NULL>=2', 'NULL>=3', 'NULL>=4', 'NULL>=5', 'NULL>=6', 
  'NULL>=7', 'NULL>=8'
) 

library('grid')
p <- ggplot(
  data = subset(tmp, dv %in% c(erDvs[1:2], piDvs[1:2])),
  aes(x = as.factor(ylevel), y = coef, colour = lead, ymin = lo95, ymax = up95)
)
p <- p + 
geom_hline(yintercept = 0, linetype = 'longdash') +
geom_point(position = position_dodge(dodge)) +
geom_errorbar(
  position = position_dodge(dodge), width = 0, size = .3, 
  show_guide = FALSE
) + 
scale_x_discrete(labels = parse(text = lab)) +
scale_y_continuous(limits = c(-1, 1)) +
scale_colour_manual(values = c('#0380B5', '#9E3173')) +
labs(
  x = 'Discretized response categories', 
  y = expression(beta[plain(Co-optation)]),
  colour = 't+'
) +
facet_grid(~type, scales = 'free_x') +
theme_minimal(base_size = .8*12) +
theme(
  text = element_text(family = 'serif'),
  legend.position = c(.17, 1.2),
  legend.direction = 'horizontal',
  #legend.key.width = unit(0, units = 'mm'),
  panel.grid.minor = element_blank(),
  plot.margin = unit(c(.7, 0, 0, 0)+.1, units = 'lines')
)
ggsave(
  file = file.path(pathOut, 'parallelRegressionsPosterPlot.pdf'),
  width = 3, height = 3/1.6178, dpi = 1200
)