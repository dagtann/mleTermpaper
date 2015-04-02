## --- Preliminaries --------------------------------------------
load(file.path(pathOut, 'omodels.RData'))

## --- Containers for results -----------------------------------
ylevels <- sapply(                         ## unique levels in dv
  dta.list[[1]][, erDvs], 
  function(x) { 
    na.omit(sort(unique(as.numeric(as.character(x))))) 
  }
)
ylevels <- ylevels[-c(1, 2), ]
coef.dta <- expand.grid(
  erDvs = erDvs,  imp = paste0('imp', 1:5),
  ylevel = 4:7, coef = NA, se = NA
)
coef.dta <- coef.dta[with(coef.dta, order(erDvs, imp, ylevel)), ]
row.names(coef.dta) <- NULL

## --- discrete case estimation ---------------------------------
options(warn = 1)       ## immediately output warnings if occured
count <- 0
for(dv in erDvs){               ## for all empowerment rights dvs
  for(j in 1:length(dta.list)){                      ## pull data
    dta <- dta.list[[j]]
    for(i in ylevels[, dv]){              ## for all levels in dv
      count <- count+1      ## step +1 == row address in coef.dta
      na.count <- which(is.na(dta[, dv])) ## record NA address
      dta[, paste0('ybin', i)] <- ifelse(   ## gen binary for all  
        i <= as.numeric(dta[, dv])+1, 1, 0
      )
      # Test recoding
      print(paste0('I am here:', i))
      print(table(dta[, dv], dta[, paste0('ybin', i)]))
      dta[na.count, paste0('ybin', i)] <- NA   ## restore missing
      print(count)
      fit <- glm(                        ## run binary regression
        as.formula(paste0(paste0('ybin', i), '~', ivTerms)),
        family = 'binomial', data = dta
      )
      coef.dta[count, 'coef'] <- coef(fit)['cooptation']
      coef.dta[count, 'se'] <- sqrt(diag(vcov(fit)))['cooptation']
      rm(fit)
    }
  }
}
rm(count, dv, i, j, na.count)
summary(coef.dta)
options(warn = 0) # with(dta, table(lead_fh1))
## Perfect or quasi-perfect separation occurs if FH == 2, dropped
## as not necessary to make the point.

## --- simplify data for plotting -------------------------------
coef.dta <- aggregate(
  coef.dta[, c('coef', 'se')],
  by = list(erDvs = coef.dta[, 'erDvs'], ylevel = coef.dta[, 'ylevel']),
  FUN = mean
)
coef.dta <- within(coef.dta, {
  erDvs <- as.character(erDvs)
  erDvs <- ifelse(erDvs == 'lead_fh1', 't+1', erDvs)
  erDvs <- ifelse(erDvs == 'lead_fh2', 't+2', erDvs)
  erDvs <- ifelse(erDvs == 'lead_fh3', 't+3', erDvs)
  erDvs <- ifelse(erDvs == 'lead_fh4', 't+4', erDvs)
  erDvs <- ifelse(erDvs == 'lead_fh5', 't+5', erDvs)
  }
)
## --- proceed to plotting --------------------------------------
p <- ggplot(
  data = coef.dta, 
  aes(x = factor(ylevel), y = coef)
) +
geom_point() +
geom_linerange(
  aes(ymin = coef-qnorm(.975)*se, ymax = coef+qnorm(.975)*se),
  size = .3
) +
geom_hline(
  yintercept = 0, colour = 'black', linetype = 'longdash', size = .3
) +
scale_x_discrete(limits = as.character(3:7), labels = c(
  '3' = expression(NULL>=3), '4' = expression(NULL>=4), 
  '5' = expression(NULL>=5), '6' = expression(NULL>=6), 
  '7' = expression(NULL>=7)
  )
) +
facet_grid(.~erDvs) +
labs(
  y = 'Estimated coefficient on co-optation',
  x = 'Discrete levels of empowerment rights restrictions'
) + 
theme_minimal()

library('gridExtra')               ## Add labeling note at bottom
sub.label <- textGrob(                 
  'Confidence intervals at the .95 level added. First inequality omitted because perfect separation occured.', 
  gp = gpar(fontsize = 8), x = unit(1, "npc"), hjust = 1, vjust = 0
)
ggsave(                                    ## save merged figures
  filename = file.path(pathOut, 'parallelRegressionsEr_modified.pdf'),
  plot = arrangeGrob(p, sub = sub.label, clip = FALSE),
  scale = 1, width = 7, height = 7/1.618, dpi = 1200,
  family = 'serif'
)
detach(package:gridExtra)
rm(p, sub.label, coef.dta, ylevels)
## END