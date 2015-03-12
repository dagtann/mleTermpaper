## --- preliminaries --------------------------------------------
load(file.path(pathOut, 'models.Rdata')) ## if not loaded already

pred.er.train <- vector('list', length = length(erDvs))
names(pred.er.train) <- erDvs
for(dv in erDvs){            ## extract training data predictions
  for(j in 1:n.folds){         
    summary(
      pred.er.train[[dv]][[paste0('fold', j)]] <- lapply(
        er.train[[dv]][[j]], 
        FUN = function(x){ fitted(x, type = 'probs') }
    )
    )
  }
}
ylevels <- as.character(2:7)                  ## dv reference levels
absLossTrain <- vector('list', length = length(erDvs)) ## train loss
names(absLossTrain) <- erDvs
absLossTest <- vector('list', length = length(erDvs))   ## test loss  
names(absLossTest) <- erDvs

## --- Training absolute loss -----------------------------------
for(dv in erDvs){
  for(j in 1:n.folds){                      ## consider all folds
    for(i in 1:length(dta.files)){  ## for each fold extract data
      y.train <- er.train[[dv]][[j]][[i]][['model']][, dv]
      y.train <- as.numeric(y.train)     ## and setup a matrix of 
                              ## binary indicators for comparison
      y.mat <- matrix(
        NA, nrow = length(y.train), ncol = length(ylevels)
      )
      colnames(y.mat) <- ylevels
      for(k in ylevels){
        y.mat[, k] <- ifelse(y.train == k, 1, 0)
      }
       ## absolute loss for predictions averaged over imputations
      absLossTrain[[dv]][[paste0('fold', j)]] <- sum(
        abs(meanArray(pred.er.train[[dv]][[j]])-y.mat)
      )
    }
  }
}
absLossTrain

## --- Test absolute loss ---------------------------------------
for(dv in erDvs){
  for(j in 1:n.folds){                      ## consider all folds
    for(i in 1:length(dta.files)){  ## for each fold extract data
      y.test <- dta.list[[i]][ dta.list[[i]][, 'fold'] == j, dv] 
      y.test <- as.numeric(y.test)       ## and setup a matrix of 
                              ## binary indicators for comparison
      y.mat <- matrix(
        NA, nrow = length(y.test), ncol = length(ylevels)
      )
      colnames(y.mat) <- ylevels
      for(k in ylevels){
        y.mat[, k] <- ifelse(y.test == k, 1, 0)
      }
      absLossTest[[dv]][[paste0('fold', j)]] <- sum(
        abs(meanArray(er.test[[dv]][[j]])-y.mat), na.rm = TRUE
      )
    }
  }
}
absLossTest
rm(i, j, k, dv, y.mat, y.test, y.train)

## --- Proceed to plotting --------------------------------------
## data prep --> from list to data frame and merge
pdta <- do.call(rbind.data.frame, absLossTrain)
names(pdta) <- paste0('fold', 1:n.folds)
pdta[, 'depVar'] <- row.names(pdta)
pdta <- reshape2::melt(
  pdta,
  id.vars = 'depVar',
  measure.vars = paste0('fold', 1:10),
  value.name = 'absLossTrain'
)

test.dta <- do.call(rbind.data.frame, absLossTest)
names(test.dta) <- paste0('fold', 1:n.folds)
test.dta[, 'depVar'] <- row.names(test.dta)
test.dta <- reshape2::melt(
  test.dta,
  id.vars = 'depVar',
  measure.vars = paste0('fold', 1:10),
  value.name = 'absLossTest'
)

pdta <- merge(
  x = pdta, y = test.dta, by = c('depVar', 'variable')
)
rm(test.dta)

## Standardization of loss: code sample sizes into data
sample.size <- apply(dta.list[[1]][, erDvs], 2, 
  function(x){ sum(!is.na(x)) }
)
fold.size <- data.frame(table(fold))

pdta[, 'sample.size'] <- NA
counter <- 0
for(i in erDvs){
  counter <- counter+1
  pdta[, 'sample.size'] <- ifelse(
    pdta[, 'depVar'] == i, sample.size[counter], 
      pdta[, 'sample.size']
  )  
}

pdta[, 'fold.size'] <- NA
counter <- 0
for(i in paste0('fold', 1:10)){
  counter <- counter+1
  pdta[, 'fold.size'] <- ifelse(
    pdta[, 'variable'] == i, fold.size[counter, 2], 
    pdta[, 'fold.size']
  )  
}
rm(i, counter, fold.size, sample.size)

## define plot
levels(pdta[, 'variable']) <- 1:10
pdta[, 'depVar'] <- factor(pdta[, 'depVar'],
  levels = erDvs, labels = paste('ER at t', 1:5, sep = '+')
)

library('gridExtra')

p <- ggplot(                 ## plot within vs. out-of-sample fit
  data = pdta, 
                                   ## Standardize by sample size: 
                         ##  Number of obs changes, absolute loss 
                      ##  otherwise not comparable across depvars
  aes(
    x = absLossTrain/sample.size, y = absLossTest/sample.size, 
    size = fold.size 
  )
) +
geom_point(colour = 'lightgrey') +
geom_point(shape = 1) +
scale_size(range = c(1, 2.5)) +
geom_text(aes(label = variable), size = 2, vjust = 1.5, hjust = 1.5) +
labs(
  x = 'Within-Sample Absolute Loss',
  y = 'Out-of-Sample Absolute Loss',
  size = 'Observations in fold'
) +
facet_wrap(~ depVar, nrow = 1) +
theme_bw() +
theme(
  legend.background = element_blank(),
  legend.direction = 'horizontal',
  legend.position = 'top',
  legend.background = element_rect(fill = 'transparent'),
  legend.key = element_blank(),
  panel.border = element_blank()
)

library('gridExtra')
sub.label <- textGrob(                 ## Labeling note at bottom
  'All figures were standardized by sample size.', 
  gp = gpar(fontsize = 6), x = unit(1, "npc"), hjust = 1, vjust = 0
)

ggsave(                                    ## save merged figures
  filename=file.path(pathOut, 'figPointAbsLoss.pdf'),
  plot = arrangeGrob(p, sub = sub.label, clip = FALSE),
  scale = 1, width = 7, height = 7/1.618, dpi = 1200,
  family = 'serif'
)
detach(package:gridExtra)
dev.off()                                 ## close popping window
rm(p, pdta, sub.label, absLossTest, absLossTrain, ylevels)
## END