## This script extracts and plots LOOCV MSEs and generates a
## boxplot
## This script requires reference model fit
## NOT USED ON POSTER
## --- Leave one out cross validation ---------------------------
n <- length(resid(fit))
num <- fitted(fit)-fit[['model']][, 'lead_flip_cl1']
den <- 1-hatvalues(fit)
loocv.mses <- (num/den)^2
loocv.rmses <- sqrt(loocv.mses)
mean(loocv.mses)                                     ## LOOCV MSE
mean(resid(fit)^2)                                   ## model mse

dta <- data.frame(x = 0, y = loocv.rmses)

library(countrycode)
dta <- within(dta, {
  cow <- sapply(
    strsplit(row.names(dta), split = ':', fixed = TRUE), '[', 1
  )
  cow <- as.numeric(cow)
  year <- sapply(
    strsplit(row.names(dta), split = ':', fixed = TRUE), '[', 2
  )
  year <- as.numeric(year)
  cname <- countrycode(
    sourcevar = cow, origin = 'cown', destination = 'iso3c', 
    warn = TRUE
  )
  label = paste(cname, year, sep = ', ')
  }
)
dta <- dta[with(dta, order(y)), ]
dta[, 'order'] <- 1:nrow(dta)
dta[, 'tag'] <- with(dta, 
  factor(ifelse(order >= nrow(dta)-5, 1, 0))
)
detach(package:countrycode)

library('grid')
p <- ggplot(data = dta, aes(x = x, y = y)) + 
geom_point(
  x = -.35, y = sqrt(mean(loocv.mses)), 
  shape = 25, colour = '#9E3173', fill = '#9E3173'
) +
geom_tufteboxplot() + 
geom_text(
  aes(label = label, colour = tag), 
  vjust = -.5, hjust = -.125, angle = 45, size = 2) + 
scale_y_continuous(limits = c(0, 5)) +
scale_colour_manual(values = c('transparent', 'black')) +
guides(colour = 'none') +
theme_minimal() + 
theme(
  text = element_text(family = 'serif'),
  axis.ticks.y = element_blank(),
  axis.text.y = element_blank(),
  axis.title.x = element_blank(),
  panel.grid = element_blank(),
  plot.margin = unit(c(0, -.5, -0.5, -2)+.1, units = 'lines')
) +
coord_flip()
detach(package:grid)
ggsave(
  file = file.path(pathOut, 'loocvBoxplot.pdf'), 
  width = 3, height = 1, family = 'serif'
)