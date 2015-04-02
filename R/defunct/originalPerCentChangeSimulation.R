## Original simulation for per cent change in category membership

# mod <- er.replicate[[1]][[1]][[1]]
# betas <- coef(mod)
# tau <- mod[['zeta']]
# sigma <- solve(mod[['Hessian']])
# pred.0 <- preds[, 1:length(betas)] %*% scenario.dta[1, ]
# pred.4 <- preds[, 1:length(betas)] %*% scenario.dta[2, ]
# tau <- preds[, (length(betas)+1):dim(preds)[2]]
# dim(tau)
# dta <- data.frame(
#   id = 1:N,
#   c2 = plogis(tau[, 1] - pred.0) - plogis(tau[, 1] - pred.4),
#   c3 = plogis(tau[, 2] - pred.0) - plogis(tau[, 1] - pred.0) - 
#     plogis(tau[, 2] - pred.4) - plogis(tau[, 1] - pred.4),
#   c4 = plogis(tau[, 3] - pred.0) - plogis(tau[, 2] - pred.0) - 
#     plogis(tau[, 3] - pred.4) - plogis(tau[, 2] - pred.4),
#   c5 = plogis(tau[, 4] - pred.0) - plogis(tau[, 3] - pred.0) - 
#     plogis(tau[, 4] - pred.4) - plogis(tau[, 3] - pred.4),
#   c6 = plogis(tau[, 5] - pred.0) - plogis(tau[, 4] - pred.0) - 
#     plogis(tau[, 5] - pred.4) - plogis(tau[, 4] - pred.4),
#   c7 = (1 - plogis(tau[, 5] - pred.0)) - 
#     (1 - plogis(tau[, 5] - pred.4))
# )
# dta.md <- reshape2::melt(
#   dta, id.vars = 'id', measure.vars = paste0('c', 2:7)
# )
