## --- Preliminaries --------------------------------------------
load(file.path(pathOut, 'omodels.RData'))
ylevels <- lapply(       ## Check if all levels of response exist
  dta.list, 
  function(x) { 
    apply(dta.list[[1]][, piDvs], 2, 
      function(x) {
        table(as.numeric(as.character(x)))
      }
    )
  }
)

## --- Define reference/storage for loop ------------------------
ylevels <- sapply(
  dta.list[[1]][, piDvs], 
  function(x) { 
    na.omit(sort(unique(as.numeric(as.character(x))))) 
  }
)
ylevels <- ylevels[-c(1,2), ]
coef.dta <- expand.grid(
  piDvs = piDvs,  imp = paste0('imp', 1:5), ylevel = 2:8, coef = NA,
  se = NA
)
coef.dta <- coef.dta[with(coef.dta, order(piDvs, imp, ylevel)), ]
row.names(coef.dta) <- NULL

## --- run parallel regression ----------------------------------
## You are about to run a bottleneck procedure with 225 regressions
count <- 0                        ## counter for rows in coef.dta
options(warn = 1)       ## immediately output warnings if occured
for(dv in piDvs){                                 ## for every DV
  for(j in 1:length(dta.list)){       ## go through each imp data
    dta <- dta.list[[j]]                          ## pull out dta
                ## slows loop, but avoids modifying original data
    for(i in ylevels[, dv]){ 
    count <- count+1     ## step + 1, id's row in results frame
    na.count <- which(is.na(dta[, dv])) ## record NA case address
    dta[, paste0('ybin', i)] <- ifelse(     ## gen binary for all  
      i <= as.numeric(dta[, dv])-1, 1, 0
    )
    # Test recoding
    # print(paste0('I am here:', i))
    # print(table(dta[, dv], dta[, paste0('ybin', i)]))
    dta[na.count, paste0('ybin', i)] <- NA     ## restore missing
    print(count); print(dv); print(paste0('dta', j)); print(paste0('ybin', i))
    fit <- glm(                              ## run logit model
      as.formula(paste0(paste0('ybin', i), '~', ivTerms)),
      family = 'binomial', control = list(maxit = 500), 
      data = dta
    )
    coef.dta[count, 'coef'] <- coef(fit)['cooptation']  ## coef
                                          ## store standard error
    coef.dta[count, 'se'] <- sqrt(diag(vcov(fit)))['cooptation']
    }
  }
}
rm(fit, count, ylevels)
summary(coef.dta)
options(warn = 0)
## level 0 dropped from analysis b/c perfect separation occured
## --- simplify data for plotting -------------------------------
coef.dta <- aggregate(
  coef.dta[, c('coef', 'se')],
  by = list(erDvs = coef.dta[, 'piDvs'], ylevel = coef.dta[, 'ylevel']),
  FUN = mean
)
coef.dta <- within(coef.dta, {
  piDvs <- as.character(piDvs)
  piDvs <- ifelse(piDvs == 'lead_flip1', 't+1', piDvs)
  piDvs <- ifelse(piDvs == 'lead_flip2', 't+2', piDvs)
  piDvs <- ifelse(piDvs == 'lead_flip3', 't+3', piDvs)
  piDvs <- ifelse(piDvs == 'lead_flip4', 't+4', piDvs)
  piDvs <- ifelse(piDvs == 'lead_flip5', 't+5', piDvs)
  }
)

## --- proceed to plotting --------------------------------------
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
scale_x_discrete(limits = as.character(1:8),
  labels = c(
    # '1' = expression(NULL>=1), '2' = expression(NULL>=2),
    # '3' = expression(NULL>=3), '4' = expression(NULL>=4), 
    # '5' = expression(NULL>=5), '6' = expression(NULL>=6), 
    # '7' = expression(NULL>=7), '8' = expression(NULL>=8)
    '1' = expression(NULL>=1), '2' = "",
    '3' = expression(NULL>=3), '4' = "", 
    '5' = expression(NULL>=5), '6' = "", 
    '7' = expression(NULL>=7), '8' = ""
  )
) +
scale_y_continuous(breaks = seq(-.5, .75, .25)) +
facet_grid(.~piDvs) +
labs(
  y = 'Estimated coefficient on co-optation',
  x = 'Discrete levels of physical integrity violations'
) + 
theme_minimal()

library('gridExtra')               ## Add labeling note at bottom
sub.label <- textGrob(                 
  'Confidence intervals at the .95 level added. First inequality omitted because perfect separation occured.', 
  gp = gpar(fontsize = 8), x = unit(1, "npc"), hjust = 1, vjust = 0
)
ggsave(                                    ## save merged figures
  filename = file.path(pathOut, 'parallelRegressionsPi_modified.pdf'),
  plot = arrangeGrob(p, sub = sub.label, clip = FALSE),
  scale = 1, width = 7, height = 7/1.618, dpi = 1200,
  family = 'serif'
)
detach(package:gridExtra)
## END