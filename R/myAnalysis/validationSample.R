## This script generates a slope plot for out of sample predictions
## --------------------------------------------------------------
load(file.path(pathOut, 'myExtension.RData'))

## --- data management and predictions --------------------------
test.dta <- subset(org.dta, test == 1)    ## isolate test dataset
# with(test.dta,                   ## 3 ctries completely missing
#   ave(
#     lead_flip_cl1, cowcode, 
#     FUN = function(x){ mean(x, na.rm = TRUE) }
#   )
# )
test.dta[, 'yhat'] <- predict(fit, newdata = test.dta)
test.dta <- within(test.dta, {
  e <- yhat-lead_flip_cl1
  e2 <- e^2
  rmse <- ave(e2, cowcode, FUN = function(x){ sqrt(mean(x, na.rm = TRUE)) })
  sd <- ave(lead_flip_cl1, cowcode, FUN = function(x){ sd(x, na.rm = TRUE) })
  }
)
summary(test.dta[, c('yhat', 'e', 'e2', 'rmse', 'sd')])

validation.rmse <- c(                       ## vector for comparison
  sqrt(mean(resid(fit)^2)),                            ## model RMSE
  sqrt(mean(test.dta[, 'e2'], na.rm = TRUE))      ## validation RMSE
)
validation.rmse

## --- prepare for plotting -------------------------------------
test.dta <- with(test.dta,    ## aggregate to unique country list
  aggregate(
    test.dta[, c('sd', 'rmse')], by = list(cowcode = cowcode),
    FUN = mean, na.rm = TRUE
  )
)
test.dta <- reshape2::melt(test.dta,
  id.vars = 'cowcode', measure.vars = c('sd', 'rmse')
)
## handle faulty entries
test.dta[, 'n'] <- with(test.dta,    ## (A) sd, but no prediction
  ave(rep(1, nrow(test.dta)), cowcode, FUN = sum)
)
test.dta <- subset(test.dta, !is.nan(value))            ## NaN sd
test.dta <- subset(test.dta, n > 1)
test.dta <- test.dta[with(test.dta, order(variable, value)), ]

library('countrycode')          ## create speaking country labels
test.dta <- within(test.dta, {
  cname <- countrycode(
    sourcevar = cowcode, origin = 'cown', destination = 'iso3c'
  )
  order <- ave(value, variable, FUN = function(x){ 1:length(x)})
  }
)
detach(package:countrycode)

## --- proceed to plotting --------------------------------------
library('grid')
p <- ggplot( data = test.dta, 
  aes(x = variable, y = value, group = cowcode)
) + 
geom_line(data = subset(
  test.dta, cowcode %in% c(500,  703, 90, 565) == FALSE
  ), 
  size = .3, colour = '#878787'
) +
geom_line(
  data = subset(test.dta, cowcode %in% c(703, 500)), 
  color = '#0380B5'
) +
geom_line(
  data = subset(test.dta, cowcode %in% c(90, 565)), 
  color = '#9E3173'
) +
geom_text(data = subset(
    test.dta, variable == 'rmse' & order %in% c(1,2, 21, 22)
  ), 
  x = 2, aes(label = cname), size = 2, hjust = -.1, 
  family = 'serif'
) +
scale_x_discrete(
  labels = c(
    expression(plain(SD)[plain(Country)]),
    expression(plain(RMSE)[plain(Country)])
  ),
  expand = c(0.1, 0)
) +
theme_minimal(base_size = 12*.7, base_family = 'serif') +
theme(
  panel.grid.minor = element_blank(),
  axis.title = element_blank(),
  plot.margin = unit(rep(0, 4)+.15, units = 'lines')
)
ggsave(
  plot = p,
  file.path(pathOut, 'testSamplePred.pdf'),
  width = 3, height = 3/1.618, dpi = 1200
)
## --- finishing ------------------------------------------------
rm(test.dta, p, validation.rmse)
detach(package:grid)
## END