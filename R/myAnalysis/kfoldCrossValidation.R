## This script runs a 'leave-one-country-out cross-validation over 
## all countries in the data
## --------------------------------------------------------------
load(file.path(pathOut, 'myExtension.RData'))

## --------------------------------------------------------------
k <- length(coef(fit))                 ## k coefficients in model

cv.dta <- fit[['model']]                 ## cross-validation data
cv.dta[, 'cowcode'] <- sapply(   ## retrieve cowcode from rowname
  strsplit(row.names(cv.dta), split = ':', fixed = TRUE), '[', 1
)
cv.dta[, 'cowcode'] <- as.numeric(cv.dta[, 'cowcode'])
cowcodes <- unique(cv.dta[, 'cowcode'])  ## reference list for cv

cv.list <- list()                                ## store results
cv.list[['coef.mat']] <- matrix(            ## coefficient matrix
  NA, nrow = length(cowcodes), ncol = k+1
)
colnames(cv.list[['coef.mat']]) <- c('cowcode', names(coef(fit)))
cv.list[['se.mat']] <- matrix(           ## standard error matrix
  NA, nrow = length(cowcodes), ncol = k+1
)
colnames(cv.list[['se.mat']]) <- c('cowcode', names(coef(fit)))

cv.list[['mse.mat']] <- matrix(       ## mean square error matrix
  NA, nrow = length(cowcodes), ncol = 4
)
colnames(cv.list[['mse.mat']]) <- c(
  'cowcode', 'size', 'train', 'test'
)

## --- execute cross-validation ---------------------------------
## Attention: You are about to run a bottleneck procedure
count <- 0                                      ## position count
for(i in cowcodes){                   ## execute cross validation
  # i = 1
  print(i)
  count <- count+1
  train <- subset(cv.dta, cowcode != i)
  ## summary(train)
  test <- subset(cv.dta, cowcode == i)
  ## summary(test)
  fit.tmp <- lm(formula(fit), data = train)  ## fit is reference!
  ## coef(fit.tmp)
  ## regression coefficients
  cv.list[['coef.mat']][count, 1] <- i           ## store country
  cv.list[['coef.mat']][count, -1] <- coef(fit.tmp)  ## store est
  ## robust standard errors
  cv.list[['se.mat']][count, 1] <- i                    ## repeat
  cv.list[['se.mat']][count, -1] <- sqrt(diag(vcovHAC(fit.tmp)))
  ## mean square error estimate
  cv.list[['mse.mat']][count, 1] <- i
  cv.list[['mse.mat']][count, 2] <- nrow(test)
  cv.list[['mse.mat']][count, 3] <- mean(resid(fit.tmp)^2)
  y.pred <- predict(fit.tmp, newdata = test, type = 'response')
  cv.list[['mse.mat']][count, 4] <- mean((y.pred-test[, 'lead_flip_cl1'])^2)
}
rm(i, test, train, fit.tmp)

## --- data management for plotting -----------------------------
## (A) Pool estimates
cv.list[['coef.mat']] <- reshape2::melt(
  data = data.frame(cv.list[['coef.mat']]), 
  id.vars = 'cowcode', measure.vars = 2:ncol(cv.list[['coef.mat']]),
  value.name = 'coefficient'
)
cv.list[['se.mat']] <- reshape2::melt(
  data = data.frame(cv.list[['se.mat']]), 
  id.vars = 'cowcode', measure.vars = 2:ncol(cv.list[['se.mat']]),
  value.name = 'se'
)
pdta <- merge(x = cv.list[['coef.mat']], y = cv.list[['se.mat']])

## (B) order by estimate size
pdta <- pdta[with(pdta, order(variable, coefficient)), ]
pdta[, 'orderObs'] <- ave(
  pdta[, 'coefficient'], pdta[, 'variable'], 
  FUN = function(x){1:length(x)}
)

## (C) Add country labels & tag stronges deviatons
library('countrycode')
pdta <- within(pdta, {
  cname <- countrycode(sourcevar = cowcode, origin = 'cown', 
    destination = 'iso3c', warn = TRUE
  )
  tag <- ifelse(orderObs <= 1 | orderObs >= 83, 1, 0)
  }
)
mse.pdta <- data.frame(cv.list[['mse.mat']])
mse.pdta <- within(mse.pdta, {
  cname <- countrycode(
    sourcevar = cowcode, 
    origin = 'cown', destination = 'iso3c',
    warn = TRUE
  )
  }
)
detach(package:countrycode)

## --- Proceed to plotting --------------------------------------
source(            ## this produces a by country caterpillar plot
  file.path(pathCode, 'myAnalysis', 'plotCvCoefficients.R'),
  echo = TRUE
)
source(   ## this produces a slope plot for the validation sample
  file.path(pathCode, 'myAnalysis', 'validationSample.R'),
  echo = TRUE
)
## END