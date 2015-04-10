## FILE NOT USED -- CONTENT MOSTLY EXPERIMENTAL
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
rownames(train.dta) <- with(train.dta, 
  paste(cowcode, year, sep = ':')
)

## --- check variable associations ------------------------------
terms <-  c(
  "lead_flip1", "fh_ordinal", "cooptation", "prio_conflict_intra",
  "prio_conflict_inter", "gled_lpop", "gled_lgdppc", 
  "geddes_personal", "geddes_monarch", "geddes_party", 
  "wdi_tradegdp", "coldwar", "wdi_gdppcgrowth", 
  "archigos_pastleaderfail", "powthy_pastattempts", 
  "pseudologross", "election", "banks_genstrike", "banks_riot", 
  "banks_antigovdem", "flip_ciri_phys"
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
) + 
geom_smooth(method = 'loess', span = .7) + 
geom_smooth(method = 'lm', color = 'red')
with(train.dta,                        ## association seems small
  cor(as.numeric(lead_flip1), t, 
    use = 'complete.obs', method = 'spearman'
  )
) 
summary(with(train.dta, aov(as.numeric(lead_flip1) ~ factor(t))))
## no strong indication of trend

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
  aes(x = cooptation, y = as.numeric(lead_flip1))
) +
geom_smooth() + facet_wrap(~t.cut, scales = 'free_x') 

## --- test.model -----------------------------------------------
library('sandwich'); library('lmtest'); library('car')
fit <- lm(
  as.numeric(lead_fh1) ~ cooptation + 
  geddes_party + geddes_personal + geddes_monarch +    
  pseudologross + flip_ciri_phys + powthy_pastattempts + t,
  data = train.dta
)
summary(fit)
coeftest(fit, vcov = vcovHAC(fit))
residualPlots(fit)

fit2 <- update(fit, . ~ . + I(pseudologross^2) + I(t^2))
coeftest(fit2, vcov = vcovHAC(fit2))
anova(fit, fit2)

fit3 <- update(fit2, . ~ . + cooptation:flip_ciri_phys)
anova(fit2, fit3)
summary(fit3)
coeftest(fit3, vcov = vcovHAC(fit3))


cld <- read.csv(
  file.path(pathData, 'cldCow.csv'), 
  stringsAsFactors = FALSE
)
str(cld)
library('ETLUtils')
train.dta <- matchmerge(
  x = train.dta, y = cld, 
  by.x = with(train.dta, paste(cowcode, year, sep = ":")),
  by.y = with(cld, paste(cowcode, year, sep = ":")),
  by.iskey = TRUE,
  all.x = TRUE,
  suffix = "",
  add.columns = c('freexp', 'freass'),
  check.duplicates = TRUE,
  trace = TRUE
)
detach(package:ETLUtils)
detach(package:ff)
detach(package:bit)

train.dta <- within(train.dta, {
  cl.rep <- freexp + freass
  flip_cl <- recode(cl.rep, recodes = "8=0; 7=1; 6=2; 5=3; 4=4; 3=5; 2=6")
  lead_flip_cl1 <- ave(flip_cl, cowcode, FUN = function(x){ Hmisc::Lag(x, shift = -1)})
  }
)

fit.cld <- update(fit, lead_flip_cl1 ~ . - cooptation + cooptation_discrete)
coeftest(fit.cld, vcov = vcovHAC(fit.cld))
residualPlots(fit.cld)

fit.cld2 <- update(fit.cld, . ~ . - t + poly(t, 2))
coeftest(fit.cld2, vcov = vcovHAC(fit.cld2))
residualPlots(fit.cld2)

marginalModelPlots(fit.cld2)
crPlots(fit.cld2)

## --- outlier diagnosis ----------------------------------------
influenceIndexPlot(fit.cld2, id.n = 4)
influencePlot(fit.cld2, id.n = 3)
outlierTest(fit.cld2)

fit.cld2.310 <- update(
  fit.cld2, . ~ ., data = subset(train.dta,  cowcode != 310 & year != 1989)
)
compareCoefs(fit.cld2, fit.cld2.310)
outlierTest(fit.cld2.310) ## Hungary 1989 -- already democratic!

train.dta <- within(train.dta, {
  hungary.flag <- ifelse(year == 1989 & cowcode == 310, 1, 0)
  hungary.flag <- factor(hungary.flag)
  }
)
summary(train.dta)
fit.cld3 <- update(fit.cld2, . ~ . + hungary.flag)
coeftest(fit.cld3, vcov = vcovHAC(fit.cld3))
residualPlots(fit.cld3, id.n = 5)

fit.cld4 <- update(fit.cld3, . ~ . + cooptation_discrete:flip_ciri_phys)
anova(fit.cld3, fit.cld4)
round(coeftest(fit.cld4, vcov = vcovHAC(fit.cld4)), digits = 2)

## --- refit clean model ----------------------------------------
poly.t <- with(train.dta, poly(t, 2))
train.dta[, 'tlin'] <- poly.t[, 1]
train.dta[, 'tsqu'] <- poly.t[, 2]
rm(poly.t)

fit <- lm(
  lead_flip_cl1 ~
  cooptation_discrete + flip_ciri_phys +
  geddes_party + geddes_personal + geddes_monarch +
  pseudologross + powthy_pastattempts +
  tlin + tsqu + hungary.flag +
  cooptation_discrete:flip_ciri_phys,
  data = train.dta
)
summary(fit)
coeftest(fit, vcov = vcovHAC(fit))

n.sim <- 1000
draws <- mvrnorm(n.sim, coef(fit), vcov(fit))
scen.dta <- expand.grid(
  intercept = 1,
  cooptation_discreteNoLegLeast1Par     = 0:1,
  cooptation_discreteLeg1Par            = 0:1,       
  cooptation_discretelegMore1Par        = 0:1,
  flip_ciri_phys = seq(0, 8, length.out = 100),
  geddes_party = 1,
  geddes_personal = 0,
  geddes_monarch = 0,
  pseudologross = mean(train.dta[, 'pseudologross'], na.rm = TRUE),
  powthy_pastattempts = mean(train.dta[, 'pseudologross'], na.rm = TRUE),
  tlin = 0,
  tsqu = 0,
  hungary.flag = 0
)
scen.dta[, "cooptation_discreteNoLegLeast1Par:flip_ciri_phys"] <- with(scen.dta, cooptation_discreteNoLegLeast1Par*flip_ciri_phys)
scen.dta[, "cooptation_discreteLeg1Par:flip_ciri_phys"] <- with(scen.dta, cooptation_discreteLeg1Par*flip_ciri_phys)
scen.dta[, "cooptation_discretelegMore1Par:flip_ciri_phys"] <- with(scen.dta, cooptation_discretelegMore1Par*flip_ciri_phys)

tmp <- data.frame(
  scen.dta, 
  t(draws %*% t(scen.dta))
)
tmp <- within(tmp, {
  cooptation <- NA
  cooptation <- ifelse(
    cooptation_discretelegMore1Par == 0 & 
    cooptation_discreteLeg1Par == 0 &
    cooptation_discreteNoLegLeast1Par == 1, 1, cooptation
  )
  cooptation <- ifelse(
    cooptation_discretelegMore1Par == 0 & 
    cooptation_discreteLeg1Par == 1 &
    cooptation_discreteNoLegLeast1Par == 0, 2, cooptation
  )
  cooptation <- ifelse(
    cooptation_discretelegMore1Par == 1 & 
    cooptation_discreteLeg1Par == 0 &
    cooptation_discreteNoLegLeast1Par == 0, 3, cooptation
  )
  cooptation <- ifelse(
    cooptation_discretelegMore1Par == 0 & 
    cooptation_discreteLeg1Par == 0 &
    cooptation_discreteNoLegLeast1Par == 0, 0, cooptation
  )
  }
)
tmp <- subset(tmp, !is.na(cooptation))
summary(tmp)
names(tmp)[1:20]
tmp <- reshape2::melt(tmp,
  id.vars = c(
    "flip_ciri_phys",
    "cooptation"
  ),
  measure.vars = grep('X', names(tmp), fixed = TRUE)
)

ggplot(
  data = tmp, 
  aes(x = flip_ciri_phys, y = value, group = variable)
) +
geom_line(alpha = .1) + facet_wrap(~cooptation)
