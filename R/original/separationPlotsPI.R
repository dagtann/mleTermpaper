## This script creates separation plots for all PI dvs and 
## imputations. It returns a pdf file.
## --- Preliminaries --------------------------------------------
load(file.path(pathOut, 'omodels.RData'))
plotlist <- list()

for(dv in piDvs){
  ## --- pull and prepare predicted probabilities -----------------
  yhat <- lapply(
    pi.replicate[[dv]], 
    function(x) { fitted(x[[1]]) }
  )
  yhat[['imp2']] <- yhat[['imp2']][-which(
    row.names(yhat[['imp2']])%in%row.names(yhat[['imp1']]) == FALSE
  ), ]
  # Problem: Imp2 contains predictions on el salvador that
  # do not exist in other imputations. It is one prediction in each 
  # case. The file seems to be corrupt as there have been other
  # problems before. See dataBuild.R
  yhat <- lapply(yhat, function(x)(x <- x[order(row.names(x)), ]))
  yhat <- meanArray(yhat)
  colnames(yhat) <- paste0('cat', colnames(yhat))
  yhat <- data.frame(yhat)
  yhat[, 'id'] <- row.names(yhat)
  yhat <- reshape2::melt(
    yhat, 
    id.vars = c('id'),
    measure.vars = paste0('cat', 0:8),
    value.name = 'yhat'
  )

  ## --- pull and prepare actual observations ---------------------
  yobs <- pi.replicate[[dv]][[1]][[1]][['model']][, dv]
  names(yobs) <- row.names(pi.replicate[[dv]][[1]][[1]][['model']])
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
    measure.vars = paste0('cat', 0:8),
    value.name = 'yobs'
  )
  ydf <- merge(yhat, ymat)  ## df containing all plot information

  ## --- Proceed to plotting ------------------------------------
  library('grid')
  library('gridExtra')
  count <- 0
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
      scale_fill_manual(values = c('white', '#727272')) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .5)
      ) +
      theme_minimal() +
      if(dv == 'lead_flip1'){
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_text(angle = 0, hjust = 1.5), 
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_blank(),
          plot.margin = unit(c(0, 0.1, -1, -.6), 'lines')
        )
      } else {
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(), 
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_blank(),
          plot.margin = unit(c(0, 0.1, -1, -.8), 'lines')
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
  file = file.path(pathOut, paste0('superSeparationPI.pdf')),
  arrangeGrob(
    do.call(arrangeGrob, c(plotlist[['lead_flip1']], list(ncol = 1, main = textGrob('PI at t+1', vjust = .7)))),
    do.call(arrangeGrob, c(plotlist[['lead_flip2']], list(ncol = 1, main = textGrob('PI at t+2', vjust = .7)))),
    do.call(arrangeGrob, c(plotlist[['lead_flip3']], list(ncol = 1, main = textGrob('PI at t+3', vjust = .7)))),
    do.call(arrangeGrob, c(plotlist[['lead_flip4']], list(ncol = 1, main = textGrob('PI at t+4', vjust = .7)))),
    do.call(arrangeGrob, c(plotlist[['lead_flip5']], list(ncol = 1, main = textGrob('PI at t+5', vjust = .7)))),
    ncol = 5
  ),
  width = 7, height = 7/1.618, family = 'serif'
)
## --- clean up -------------------------------------------------
rm(list = c(
  'dv', 'i', 'plotlist', 'yhat', 'yobs', 'ymat', 'ylevels', 'k', 
  'count', 'ydf',  
  ls()[grep(x = ls(), pattern = '[[:alpha:]]{3}[[:digit:]]')]
  )
)
## END