## This file performs a simulation analysis on the assumed interaction
## It generates a pdf plot
## --------------------------------------------------------------
load(file.path(pathOut, 'myExtension.RData'))

## --- Simulation setup -----------------------------------------
n.sim <- 1000                               ## No. of simulations
set.seed(4995)
draws <- mvrnorm(n.sim, coef(fit), vcovHAC(fit))  ## robust draws
scen.dta <- expand.grid(                         ## scenario data
  intercept = 1,
  cooptation_discreteNoLegLeast1Par     = 0:1,
  cooptation_discreteLeg1Par            = 0:1,       
  cooptation_discretelegMore1Par        = 0:1,
  flip_ciri_phys = seq(0, 8, length.out = 100),             
  gled_lgdppc = mean(train.dta[, 'gled_lgdppc'], na.rm = TRUE),
  gled_lpop = mean(train.dta[, 'gled_lpop'], na.rm = TRUE),                  
  geddes_personal = 0,         
  geddes_monarch = 0,
  geddes_party = 0,
  log.archigos_pastleaderfail = mean(train.dta[, 'log.archigos_pastleaderfail'], na.rm = TRUE),
  powthy_pastattempts = mean(train.dta[, 'powthy_pastattempts'], na.rm = TRUE),
  log.prio_conflict_intra = mean(train.dta[, 'log.prio_conflict_intra'], na.rm = TRUE),
  prio_conflict_inter = mean(train.dta[, 'prio_conflict_inter'], na.rm = TRUE),
  log.banks_genstrike = mean(train.dta[, 'log.banks_genstrike'], na.rm = TRUE),
  log.banks_riot = mean(train.dta[, 'log.banks_riot'], na.rm = TRUE),
  log.banks_antigovdem = mean(train.dta[, 'log.banks_antigovdem'], na.rm = TRUE),
  tlin = 0,
  tsqu = 0
)
scen.dta[, "cooptation_discreteNoLegLeast1Par:flip_ciri_phys"] <- with(scen.dta, cooptation_discreteNoLegLeast1Par*flip_ciri_phys)
scen.dta[, "cooptation_discreteLeg1Par:flip_ciri_phys"] <- with(scen.dta, cooptation_discreteLeg1Par*flip_ciri_phys)
scen.dta[, "cooptation_discretelegMore1Par:flip_ciri_phys"] <- with(scen.dta, cooptation_discretelegMore1Par*flip_ciri_phys)

## --- Systematic uncertainty -----------------------------------
tmp <- data.frame(scen.dta, t(draws %*% t(scen.dta))) ## syst unc

## --- refine results -------------------------------------------
tmp <- within(tmp, {   ## expand grid generated all! permutations
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
tmp <- reshape2::melt(tmp,                   ## melt for plotting
  id.vars = c("flip_ciri_phys", "cooptation"),
  measure.vars = grep('X', names(tmp), fixed = TRUE)
)

## --- add stochastic uncertainty -------------------------------
tmp.stoch <- tmp
dfCorr <- 1/(length(resid(fit))-length(coef(fit)))       
sigma <- sqrt(dfCorr*t(resid(fit))%*%resid(fit))         ## sigma
tmp.stoch <- within(tmp.stoch,         ## fundamental uncertainty
  value <- value + rnorm(n = nrow(tmp.stoch), mean = 0, sd =sigma)
)
tmp.agg <- aggregate(               ## slope with expected effect
  value ~ flip_ciri_phys + cooptation, data = tmp,
  FUN = mean
)
## --- proceed to plotting --------------------------------------
source(file.path(pathCode, 'myAnalysis', 'spaghettiSimulation.R'))

## --- Finishing ------------------------------------------------
rm(dfCorr, sigma, n.sim, draws, scen.dta)