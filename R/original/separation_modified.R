load(file.path(pathOut, 'omodels.RData'))
load(file.path(pathOut, 'orgData.RData'))
rownames(org.dta) <- with(org.dta, paste(cowcode, year, sep = ':'))
case.id <- row.names(org.dta)

ylevels <- sort(unique(org.dta[, 'flip_ciri_phys']))
## print(ylevels) ## levels 0:8 observed in data
yobs <- lapply(
  c(erDvs, piDvs), 
  function(dv) { sapply(ylevels, function(x) { ifelse(org.dta[, dv] == x, 1, 0) } )
  }
)
names(yobs) <- c(erDvs, piDvs)
for(ii in names(yobs)){
  row.names(yobs[[ii]]) <- case.id
  colnames(yobs[[ii]]) <- paste0('cat', ylevels)
  yobs[[ii]] <- reshape2::melt(
    data = yobs[[ii]],
    id.vars = row.names(yobs[[ii]]),
    measure.vars = 1:ncol(yobs[[ii]]),
    value.name = 'observed'
  )
}
yobs <- do.call(rbind.data.frame, yobs)
yobs[, 'dv'] <- row.names(yobs)
yobs[, 'dv'] <- sapply(strsplit(yobs[, 'dv'], '.', fixed = TRUE), '[', 1)
names(yobs) <- c('case.id', 'category', 'observed', 'dv')
row.names(yobs) <- NULL
yobs <- within(yobs, {category <- as.character(category)})

yhat <- lapply(
  c(er.replicate, pi.replicate),
  function(dv) { lapply(dv, function(imp) {fitted(imp[[1]])}) }
)
for(i in piDvs){ ## imp2 for pi obs that do not appear anywhere else 
                              ## dropped until time to reimpute data
  yhat[[i]][[2]] <- yhat[[i]][[2]][-which(
    row.names(yhat[[i]][[2]])%in%row.names(yhat[[i]][[1]]) == FALSE
  ), ]
}
yhat <- lapply(yhat, meanArray)

for(ii in names(yhat)){
  colnames(yhat[[ii]]) <- paste0('cat', colnames(yhat[[ii]]))
  yhat[[ii]] <- reshape2::melt(
    data = yhat[[ii]],
    id.vars = row.names(yhat[[ii]]),
    measure.vars = 1:length(ylevels),
    value.name = 'fitted'
  )
}
yhat <- do.call(rbind.data.frame, yhat)
yhat[, 'dv'] <- row.names(yhat)
yhat[, 'dv'] <- sapply(strsplit(yhat[, 'dv'], '.', fixed = TRUE), '[', 1)
names(yhat) <- c('case.id', 'category', 'fitted', 'dv')
row.names(yhat) <- NULL
yhat <- within(yhat, {category <- as.character(category)})
head(yhat)

tmp <- merge(yobs, yhat, all.x = TRUE)
tmp <- subset(tmp, dv %in% c(erDvs[1:2], piDvs[1:2]) & !is.na(observed) & !is.na(fitted))
tmp <- with(tmp, tmp[order(dv, category, fitted), ])
tmp[, 'order'] <- with(tmp, ave(rep(1, nrow(tmp)), 
    dv, category, 
    FUN = function(x) { 1:length(x) })
)
tmp <- within(tmp, {
  category <- substr(category, nchar(category), nchar(category))
  dv <- ifelse(dv == 'lead_fh1', paste0("plain(ER)[t+1]"), dv)
  dv <- ifelse(dv == 'lead_fh2', paste0("plain(ER)[t+2]"), dv)
  dv <- ifelse(dv == 'lead_flip1', paste0("plain(PI)[t+1]"), dv)
  dv <- ifelse(dv == 'lead_flip2', paste0("plain(PI)[t+2]"), dv)
  }
)

library('grid')
p <- ggplot(
  data = tmp,
  aes(x = order, y = fitted)
) + 
geom_bar(
  aes(y = 1, fill = factor(observed)), 
  width = 1, stat = 'identity', position = 'identity'
) +
geom_line(size = .3) + 
scale_y_continuous(limits = c(0,1)) +
scale_fill_manual(values = c('#f0f0f0', '#9E3173')) +
guides(fill = 'none') +
facet_grid(category~dv, labeller=label_parsed) +
theme_minimal() +
theme(
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_blank(),
  panel.grid = element_blank(),
  strip.text.y = element_text(angle = 0),
  plot.margin = unit(c(0, 0, -1, -1)+.1, units = 'lines')
)
ggsave(
  plot = p,
  file = file.path(pathOut, 'separation_revise.pdf'),
  width = 3, height = 3/1.618, family = 'serif'
)