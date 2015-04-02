load(file.path(pathOut, 'omodels.RData'))
library(foreign)
org.data <- read.dta(
  file.path(pathData, 'amelia_data_input_version12.dta'),
  convert.factors = TRUE,
  convert.underscore = FALSE, 
  convert.dates = TRUE
)
detach(package:foreign)

org.data[, 'geddes_party'] <- factor( ## reg types to fact
  org.data[, 'geddes_party'], 
  levels = 0:1, labels = c('No', 'Yes')
)
org.data[, 'geddes_personal'] <- factor(
  org.data[, 'geddes_personal'], 
  levels = 0:1, labels = c('No', 'Yes')
)
org.data[, 'geddes_monarch'] <- factor(
  org.data[, 'geddes_monarch'], 
  levels = 0:1, labels = c('No', 'Yes')
)
org.data[, 'geddes_military'] <- factor(
  org.data[, 'geddes_military'], 
  levels = 0:1, labels = c('No', 'Yes')
)
org.data[, 'cooptation_discrete'] <- factor( ## discrete iv
  org.data[, 'cooptation'],
  levels = 0:3, 
  labels = c('NoLegPar', 'NoLegLeast1Par', 'Leg1Par', 
    'legMore1Par'
  )
)
org.data[, 'coldwar'] <- factor(         ## cold war dummy
  org.data[, 'coldwar'], 
  levels = 0:1, labels = c('No', 'Yes')
)
org.data[, 'election'] <- factor(   ## election year dummy
  org.data[, 'election'], 
  levels = 0:1, labels = c('No', 'Yes')
)
#with(org.data, which(is.na(archigos_duration)))
org.data <- org.data[with(org.data, order(cowcode, year)), ]

for(i in 1:length(erDvs)) {
  org.data[, erDvs[i]] <- ave(
    org.data[, 'fh_ordinal'], org.data[, 'cowcode'],
    FUN = function(x) { Hmisc::Lag(x, shift = -i) }
  )
  org.data[, erDvs[i]] <- factor(
    org.data[, erDvs[i]], levels = 2:7, ordered = TRUE
  )
}
for(i in 1:length(piDvs)) {
  org.data[, piDvs[i]] <- ave(
    org.data[, 'flip_ciri_phys'], org.data[, 'cowcode'],
    FUN = function(x) { Hmisc::Lag(x, shift = -i) }
  )
  org.data[, piDvs[i]] <- factor(
    org.data[, piDvs[i]], levels = 0:8, ordered = TRUE
  )
}
summary(org.data)
## --- Archigos past leadership fail ----------------------------
dta <- subset(                                   ## vs. er rights
  org.data, select = c(erDvs, 'archigos_pastleaderfail')
)
dta <- reshape2::melt(
  dta, measure.vars = erDvs, id.vars = 'archigos_pastleaderfail'
)
dta <- na.omit(dta)
ggplot(
  dta, aes(x = archigos_pastleaderfail, y = as.numeric(value))
) +
geom_jitter() +
facet_wrap(~variable, nrow = 1) +
geom_smooth(method = 'lm', colour = 'red') +
geom_smooth(method = 'loess', colour = 'blue') 

## check 'control var' status
cor.data <- subset(org.data,
 select = c('cooptation', 'archigos_pastleaderfail', erDvs)
)
cor.data <- apply(cor.data, 2, as.numeric)
cor(
  cor.data,  method = 'spearman', use = 'complete.obs'
)

dta <- subset(org.data,                      ## vs. pi violations
  select = c(piDvs, 'archigos_pastleaderfail')
)
dta <- reshape2::melt(
  dta, measure.vars = piDvs, id.vars = 'archigos_pastleaderfail'
)
dta <- na.omit(dta)
ggplot(dta, aes(x = archigos_pastleaderfail, y = as.numeric(value))) +
geom_jitter() +
facet_wrap(~variable, nrow = 1) +
geom_smooth(method = 'lm', colour = 'red') +
geom_smooth(method = 'loess', colour = 'blue')

cor.data <- subset(org.data,
 select = c('cooptation', 'archigos_pastleaderfail', piDvs)
)
cor.data <- apply(cor.data, 2, as.numeric)
cor(
  cor.data,  method = 'spearman', use = 'complete.obs'
)


## --- Powthy prior attempts ------------------------------------
dta <- subset(                                   ## vs. er rights
  org.data, 
  select = c(erDvs, 'powthy_pastattempts')
)
## Plot functional relationship
dta <- reshape2::melt(
  dta, measure.vars = erDvs, id.vars = 'powthy_pastattempts'
)
dta <- na.omit(dta)
ggplot(
  dta, aes(x = powthy_pastattempts, y = as.numeric(value))
) +
geom_jitter() +
facet_wrap(~variable, nrow = 1) +
geom_smooth(method = 'lm', colour = 'red') +
geom_smooth(method = 'loess', colour = 'blue') 

## check 'control var' status
cor.data <- subset(org.data,
 select = c('cooptation', 'powthy_pastattempts', erDvs)
)
cor.data <- apply(cor.data, 2, as.numeric)
cor(
  cor.data,  method = 'spearman', use = 'complete.obs'
)


dta <- subset(org.data,                      ## vs. pi violations
  select = c(piDvs, 'powthy_pastattempts')
)

## Plot functional relationship
dta <- reshape2::melt(
  dta, measure.vars = piDvs, id.vars = 'powthy_pastattempts'
)
dta <- na.omit(dta)
ggplot(dta, aes(x = powthy_pastattempts, y = as.numeric(value))) +
geom_jitter() +
facet_wrap(~variable, nrow = 1) +
geom_smooth(method = 'lm', colour = 'red') +
geom_smooth(method = 'loess', colour = 'blue')

## check 'control var' status
cor.data <- subset(org.data,
 select = c('cooptation', 'powthy_pastattempts', piDvs)
)
cor.data <- apply(cor.data, 2, as.numeric)
cor(
  cor.data,  method = 'pearson', use = 'complete.obs'
)