## Simulation of ordered logit data
load(file.path(pathOut, 'models.Rdata'))
## --- setup scenario data -----------------------------------------
scenario.dta <- expand.grid(
  fh_ordinal = mean(dta.list[[1]][, 'fh_ordinal'], na.rm = TRUE),
  cooptation = c(1, 3),  ##           
  prio_conflict_intra = mean(dta.list[[1]][, 'prio_conflict_intra'], na.rm = TRUE),
  prio_conflict_inter = mean(dta.list[[1]][, 'prio_conflict_inter'], na.rm = TRUE),
  gled_lpop = mean(dta.list[[1]][, 'gled_lpop'], na.rm = TRUE),
  gled_lgdppc = mean(dta.list[[1]][, 'gled_lgdppc'], na.rm = TRUE),
  geddes_personal = 0,
  geddes_monarch = 0,
  geddes_party = 1,
  wdi_tradegdp = mean(dta.list[[1]][, 'wdi_tradegdp'], na.rm = TRUE),
  coldwar = 1,
  wdi_gdppcgrowth = mean(dta.list[[1]][, 'wdi_gdppcgrowth'], na.rm = TRUE),
  archigos_durationLin = mean(dta.list[[1]][, 'archigos_durationLin'], na.rm = TRUE),
  archigos_durationSqu = mean(dta.list[[1]][, 'archigos_durationSqu'], na.rm = TRUE),
  archigos_durationCub = mean(dta.list[[1]][, 'archigos_durationCub'], na.rm = TRUE),
  archigos_pastleaderfail = mean(dta.list[[1]][, 'archigos_pastleaderfail'], na.rm = TRUE),
  powthy_pastattempts = mean(dta.list[[1]][, 'powthy_pastattempts'], na.rm = TRUE),
  pseudologross = mean(dta.list[[1]][, 'pseudologross'], na.rm = TRUE),
  election = 1,
  banks_genstrike = mean(dta.list[[1]][, 'banks_genstrike'], na.rm = TRUE),
  banks_riot = mean(dta.list[[1]][, 'banks_riot'], na.rm = TRUE),
  banks_antigovdem = mean(dta.list[[1]][, 'banks_antigovdem'], na.rm = TRUE),
  flip_ciri_phys = mean(dta.list[[1]][, 'flip_ciri_phys'], na.rm = TRUE)
)
scenario.dta <- as.matrix(scenario.dta)
## --- simluate coefficients ---------------------------------------
mod <- er.train[[1]][[1]][[1]]
betas <- coef(mod)
tau <- mod[['zeta']]
sigma <- solve(mod[['Hessian']])
N <- 10000
preds <- mvrnorm(N, c(betas, tau), sigma)
pred.0 <- preds[, 1:length(betas)] %*% scenario.dta[1, ]
pred.4 <- preds[, 1:length(betas)] %*% scenario.dta[2, ]
tau <- preds[, (length(betas)+1):dim(preds)[2]]
dim(tau)
dta <- data.frame(
  id = 1:N,
  c2 = plogis(tau[, 1] - pred.0) - plogis(tau[, 1] - pred.4),
  c3 = plogis(tau[, 2] - pred.0) - plogis(tau[, 1] - pred.0) - 
    plogis(tau[, 2] - pred.4) - plogis(tau[, 1] - pred.4),
  c4 = plogis(tau[, 3] - pred.0) - plogis(tau[, 2] - pred.0) - 
    plogis(tau[, 3] - pred.4) - plogis(tau[, 2] - pred.4),
  c5 = plogis(tau[, 4] - pred.0) - plogis(tau[, 3] - pred.0) - 
    plogis(tau[, 4] - pred.4) - plogis(tau[, 3] - pred.4),
  c6 = plogis(tau[, 5] - pred.0) - plogis(tau[, 4] - pred.0) - 
    plogis(tau[, 5] - pred.4) - plogis(tau[, 4] - pred.4),
  c7 = (1 - plogis(tau[, 5] - pred.0)) - 
    (1 - plogis(tau[, 5] - pred.4))
)

dta.md <- reshape2::melt(
  dta, id.vars = 'id', measure.vars = paste0('c', 2:7)
)

ggplot(dta.md, aes(x = value)) +
geom_line(stat = 'density') +
geom_vline(xintercept = 0, linetype = 'longdash') +
facet_wrap(~ variable, scales = 'free')