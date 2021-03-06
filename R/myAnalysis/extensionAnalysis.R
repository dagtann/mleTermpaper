## This file implements my attempted extension of the original 
## model. It generates a new workspace image.
## --------------------------------------------------------------
load(file.path(pathOut, 'omodels.RData'))
source(file.path(pathCode, 'myAnalysis', 'setupOriginalData.R'))
load(file.path(pathOut, 'orgData.RData'))
packsHere <- c('car', 'sandwich', 'lmtest')
invisible(lapply(packsHere, library, character.only = TRUE))
rm(packsHere)

## --- variable modifications -----------------------------------
## (A) new time trend
org.dta[, 't'] <- org.dta[, 'year']-1989 ## center t post cold war

## (B) new dep var
cld <- read.csv(                               ## read new depVar
  file.path(pathData, 'cldCow.csv'), stringsAsFactors = FALSE
)
library('ETLUtils')
org.dta <- matchmerge(                 ## merge into analysis dta
  x = org.dta, y = cld, 
  by.x = with(org.dta, paste(cowcode, year, sep = ":")),
  by.y = with(cld, paste(cowcode, year, sep = ":")),
  by.iskey = TRUE, all.x = TRUE, suffix = "", 
  add.columns = c('freexp', 'freass'), check.duplicates = TRUE,
  trace = TRUE
)
detach(package:ETLUtils)
detach(package:ff)
detach(package:bit)
org.dta <- within(org.dta, {
  cl.rep <- freexp + freass               ## create summary index
  flip_cl <- recode(cl.rep,                         ## flip index
    recodes = "8=0; 7=1; 6=2; 5=3; 4=4; 3=5; 2=6"
  )
  lead_flip_cl1 <- ave(                             ## create lag
    flip_cl, cowcode, FUN = function(x){ Hmisc::Lag(x, shift = -1) }
  )
  }
)
poly.t <- with(org.dta, poly(t, 2))   ## independent time splines
org.dta[, 'tlin'] <- poly.t[, 1]
org.dta[, 'tsqu'] <- poly.t[, 2]
rm(poly.t)

## (C) create readable ids
rownames(org.dta) <- with(org.dta, paste(cowcode, year, sep = ':'))

## --- withhold countries for prediction ------------------------
n.ctry <- length(unique(org.dta[, 'cowcode'])) ## 138 ctry in total
set.seed(8590)
test.cow <- sample(           ## withhold 20 per cent for testing
  unique(org.dta[, 'cowcode']), size = ceiling(n.ctry*.20)
)
org.dta <- within(org.dta, 
  test <- ifelse(cowcode %in% test.cow, 1, 0)
)
save(org.dta, file = file.path(pathOut, 'myAnalysis.RData'))
train.dta <- subset(org.dta, test == 0)
length(unique(train.dta[, 'cowcode']))  ## 110 ctries in training
rm(test.cow)

## --- replicate variable associations --------------------------
terms <-  c(                  ## original terms with neew dep var
  "lead_flip_cl1", "fh_ordinal", "cooptation", "flip_ciri_phys",
  "prio_conflict_intra", "prio_conflict_inter", "gled_lpop", 
  "gled_lgdppc", "geddes_personal", "geddes_monarch", "geddes_party", 
  "wdi_tradegdp", "coldwar", "wdi_gdppcgrowth", 
  "archigos_pastleaderfail", "powthy_pastattempts", 
  "pseudologross", "election", "banks_genstrike", "banks_riot", 
  "banks_antigovdem"
)
round( 
  cor( 
    sapply(train.dta[, terms], as.numeric), 
      use = 'complete.obs')[, 1:4], ## only show columns 1:4
  digits = 2
)
rm(terms)

myTerms <- c(       ## drop vars that are not meaningful controls
  "lead_flip_cl1", "cooptation", "flip_ciri_phys", 
  'gled_lgdppc', "gled_lpop", "geddes_personal", 
  "geddes_monarch", "geddes_party", 
  "archigos_pastleaderfail", "powthy_pastattempts", 
  "prio_conflict_intra", "prio_conflict_inter",
  'banks_genstrike', 'banks_riot', 'banks_antigovdem'
)
round( 
  cor( 
    sapply(train.dta[, myTerms], as.numeric), 
    use = 'complete.obs')[, 1:3], 
  digits = 2
)

## --- setup base model -----------------------------------------
form <- formula(                         ## generic model formula
  lead_flip_cl1 ~ cooptation_discrete + flip_ciri_phys +  ## core
  gled_lgdppc + gled_lpop +           ## socio-economic structure
  geddes_personal + geddes_monarch + geddes_party +  ## political
  archigos_pastleaderfail + powthy_pastattempts +     ## domestic
  prio_conflict_intra+ prio_conflict_inter+           ## conflict
  banks_genstrike + banks_riot + banks_antigovdem +
  tlin                                     ## temporal dependence
)

fit.prime <- lm(form, data = train.dta)
coeftest(fit.prime)                                ## standard se
coeftest(fit.prime, vcov = vcovHAC(fit.prime))      ## HAC cor se

residualPlots(fit.prime)    ## strong indication of non-linearity
influencePlot(fit.prime, id.n = 3)
influenceIndexPlot(fit.prime, id.n = 3)
outlierTest(fit.prime)
avPlots(fit.prime, id.n = 3)
crPlots(fit.prime, id.n = 3)

with(train.dta, symbox(archigos_pastleaderfail+1))
form <- formula(                         ## generic model formula
  lead_flip_cl1 ~ cooptation_discrete + flip_ciri_phys +  ## core
  gled_lgdppc + gled_lpop +           ## socio-economic structure
  geddes_personal + geddes_monarch + geddes_party +  ## political
  I(log(archigos_pastleaderfail+1)) + powthy_pastattempts + ## domestic
  I(log(prio_conflict_intra+1)) + prio_conflict_inter +# conflict
  I(log(banks_genstrike+1)) + I(log(banks_riot+1)) + 
  I(log(banks_antigovdem+1)) +
  tlin + tsqu                               ## temporal dependence
)
fit.second <- lm(form, data = train.dta)

residualPlots(fit.second)           ## better, still questionable
influencePlot(fit.second, id.n = 3)
influenceIndexPlot(fit.second, id.n = 3)
outlierTest(fit.second)
avPlots(fit.second, id.n = 3)
crPlots(fit.prime, id.n = 3)

org.dta <- within(org.dta, {
  log.archigos_pastleaderfail <- log(archigos_pastleaderfail+1)
  log.prio_conflict_intra <- log(prio_conflict_intra+1)
  log.banks_genstrike <- log(banks_genstrike+1)
  log.banks_riot <- log(banks_riot+1)
  log.banks_antigovdem <- log(banks_antigovdem+1)
  }
)
train.dta <- subset(org.dta, test == 0)

form <- formula(                         ## generic model formula
  lead_flip_cl1 ~ cooptation_discrete + flip_ciri_phys +  ## core
  gled_lgdppc + gled_lpop +           ## socio-economic structure
  geddes_personal + geddes_monarch + geddes_party +  ## political
  log.archigos_pastleaderfail + powthy_pastattempts + ## domestic
  log.prio_conflict_intra + prio_conflict_inter +# conflict
  log.banks_genstrike + log.banks_riot + 
  log.banks_antigovdem +
  tlin + tsqu +                            ## temporal dependence
  cooptation_discrete:flip_ciri_phys               ## interaction
)
fit <- lm(form, data = train.dta)

anova(fit.second, fit) ## either test shows sig improvement in fit
lrtest(fit.second, fit)
round(coeftest(fit), digits = 3)           ## at least partly sig
round(coeftest(fit, vcov = vcovHAC(fit)), digits = 3)
rm(fit.prime, fit.second)
## --- execute cross validation ---------------------------------
save.image(file.path(pathOut, 'myExtension.RData'))
## END