load(file.path(pathOut, 'omodels.RData'))
load(file.path(pathOut, 'orgData.RData'))

## --- withhold countries for prediction ------------------------
n.ctry <- length(unique(org.dta[, 'cowcode'])) ## 138 ctry in total
set.seed(8590)
test.cow <- sample(           ## withhold 20 per cent for testing
  unique(org.dta[, 'cowcode']), size = ceiling(n.ctry*.20)
)
test.dta <- subset(org.dta, cowcode %in% test.cow == TRUE)
train.dta <- subset(org.dta, cowcode %in% test.cow == FALSE)
str(train.dta); str(test.dta)
save(test.dta, file = file.path(pathOut, 'testCtry.RData'))
length(unique(train.dta[, 'cowcode']))  ## 110 ctries in training
rm(org.dta, test.dta)                     ## not needed right now

## --- check variable associations ------------------------------
terms <-  c(
"lead_flip1",                "fh_ordinal",             
"cooptation",              "prio_conflict_intra",    
"prio_conflict_inter",     "gled_lpop",              
"gled_lgdppc",             "geddes_personal",        
"geddes_monarch",          "geddes_party",           
"wdi_tradegdp",            "coldwar",                
"wdi_gdppcgrowth",         "archigos_pastleaderfail", 
"powthy_pastattempts",     "pseudologross",
"election",                "banks_genstrike",
"banks_riot",              "banks_antigovdem",
"flip_ciri_phys"
)
round( 
  cor( sapply(train.dta[, terms], as.numeric), use = 'complete.obs'), 
  digits = 2
)

myTerms <- c(
  'lead_flip1', 'cooptation_discrete', "geddes_party",
  "geddes_personal", "geddes_monarch", "geddes_military",
  'pseudologross', 'fh_ordinal', 'powthy_pastattempts'
)
round( 
  cor( sapply(train.dta[, myTerms], as.numeric), 
    use = 'complete.obs'
  ), digits = 2
)
pairs(
  as.formula(paste0('~', paste(myTerms, collapse = '+'))),
  data = sapply(train.dta[ , myTerms], as.numeric)
)
qplot(data = train.dta, 
  y = as.numeric(lead_flip1), x = cooptation_discrete,
  geom = 'boxplot', notch = TRUE
)
with(train.dta, table(lead_flip1, exclude = NULL))
with(train.dta, table(cooptation_discrete, exclude = NULL))
with(train.dta, table(lead_flip1, cooptation_discrete))
## Reason to assume curvature in relationship
## lead_flip1: middling scores most numerous, every level present
## cooptation_discrete:  lower 2 about 300, upper 2 >=850,
##    highest level most numerous in data

## --- time trend in bivariate relations? -----------------------
train.dta[, 't'] <- train.dta[, 'year']-1989 ## center t post cold war
summary(train.dta[, 't']); with(train.dta, plot(year, t))
summary(train.dta[, 'year'])                      ## 1972 -- 2007
qplot(
  data = train.dta, x = t, y = as.numeric(lead_flip1), 
  geom = 'jitter'
) + geom_smooth(method = 'loess', span = .7) + 
geom_smooth(method = 'lm', color = 'red')
with(train.dta,                        ## association seems small
  cor(as.numeric(lead_flip1), t, 
    use = 'complete.obs', method = 'spearman'
  )
) 
summary(with(train.dta, aov(as.numeric(lead_flip1) ~ factor(t))))
## shaky, but no strong indication of trend
#rm(terms, myTerms)

qplot(                                ## clear mean shift in 1989
  data = train.dta, x = t, y = cooptation, 
  geom = 'jitter'
) + geom_smooth(method = 'loess', span = .7) + 
geom_smooth(method = 'lm', color = 'red')
with(train.dta,                             ## strong association
  cor(cooptation, t, 
    use = 'complete.obs', method = 'spearman'
  )
) 

train.dta <- within(train.dta, 
  t.cut <- cut(t, breaks = seq(-17, 18, 5), include.lowest = TRUE)
)
table(train.dta[, 't.cut'])
ggplot(data = train.dta, 
  aes(x = cooptation, y = as.numeric(lead_flip1))) +
geom_point() + facet_wrap(~t.cut, scales = 'free_x') 


## --- 