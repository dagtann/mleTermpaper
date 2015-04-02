## This script creates separation plots for all ER dvs and 
## imputations. It returns a pdf file.
## --- Preliminaries --------------------------------------------
load(file.path(pathOut, 'omodels.RData'))
plotlist <- list()

for(dv in erDvs){
  ## --- pull and prepare predicted probabilities ---------------
  yhat <- lapply(
    er.replicate[[dv]], 
    function(x) { fitted(x[[1]]) }
  )
  yhat <- lapply(yhat, function(x)(x <- x[order(row.names(x)), ]))
  yhat <- meanArray(yhat)
  colnames(yhat) <- paste0('cat', colnames(yhat))
  yhat <- data.frame(yhat)
  yhat[, 'id'] <- row.names(yhat)
  yhat <- reshape2::melt(
    yhat, 
    id.vars = c('id'),
    measure.vars = paste0('cat', 2:7),
    value.name = 'yhat'
  )

  ## --- pull and prepare actual observations ---------------------
  yobs <- er.replicate[[dv]][[1]][[1]][['model']][, dv]
  names(yobs) <- row.names(er.replicate[[dv]][[1]][[1]][['model']])
  ylevels <- as.character(sort(unique(yobs)))
  ymat <- matrix(
    NA, nrow = length(yobs), ncol = length(unique(yobs))
  )
  colnames(ymat) <- ylevels
  for(k in ylevels){
    ymat[, k] <- ifelse(yobs == k, 1, 0)
  }
  colnames(ymat) <- paste0('cat', ylevels)
  rownames(ymat) <- names(yobs)
  ymat <- data.frame(ymat)
  ymat[, 'id'] <- row.names(ymat)
  ymat <- reshape2::melt(
    ymat, 
    id.vars = c('id'),
    measure.vars = paste0('cat', 2:7),
    value.name = 'yobs'
  )
  ydf <- merge(yhat, ymat)  ## df containing all plot information

  ## --- Proceed to plotting ------------------------------------
  library('grid')
  library('gridExtra')
  count <- 2
  for(i in paste0('cat', ylevels)) {
    assign(i, 
      ggplot(
        data = subset(ydf, variable == i),
        aes(x = reorder(id, yhat), y = yhat, group = variable)
      ) + 
      geom_bar(
        aes(y = 1, fill = factor(yobs)),
        stat = 'identity', position = 'identity', width = 1
      ) +
      geom_line(size = .3) + 
      guides(fill = 'none') +
      labs(y = count) +
      scale_fill_manual(values = c('#f0f0f0', '#9E3173')) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .5), expand = c(.125, 0)) +
      theme_minimal() +
      if(dv == 'lead_fh1'){
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_text(angle = 0, hjust = 1.5), 
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_blank(),
          plot.margin = unit(c(0, 0.1, -1.1, -.6), 'lines')
        )
      } else {
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(), 
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_blank(),
          plot.margin = unit(c(0, 0.1, -1.1, -.8), 'lines')
        )
      }
    )
    count <- count + 1
  }
  plotlist[[dv]] <- mget(
    ls()[grep(x = ls(), pattern = '[[:alpha:]]{3}[[:digit:]]')]
  )
}
## --- save results ---------------------------------------------
ggsave( ## clumsily wraps all plots in list to single page
  file = file.path(pathOut, paste0('superSeperationER.pdf')),
  arrangeGrob(
    do.call(arrangeGrob, c(plotlist[['lead_fh1']], list(ncol = 1, main = textGrob('   t+1', vjust = .7)))),
    do.call(arrangeGrob, c(plotlist[['lead_fh2']], list(ncol = 1, main = textGrob('t+2', vjust = .7)))),
    do.call(arrangeGrob, c(plotlist[['lead_fh3']], list(ncol = 1, main = textGrob('t+3', vjust = .7)))),
    do.call(arrangeGrob, c(plotlist[['lead_fh4']], list(ncol = 1, main = textGrob('t+4', vjust = .7)))),
    do.call(arrangeGrob, c(plotlist[['lead_fh5']], list(ncol = 1, main = textGrob('t+5', vjust = .7)))),
    ncol = 5
  ),
  width = 3.5, height = 3/2, family = 'sans'
)

## --- clean up -------------------------------------------------
rm(list = c(
  'dv', 'i', 'plotlist', 'yhat', 'yobs', 'ymat', 'ylevels', 'k', 
  'count', 'ydf',  
  ls()[grep(x = ls(), pattern = '[[:alpha:]]{3}[[:digit:]]')]
  )
)
## END