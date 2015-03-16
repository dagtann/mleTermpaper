ylevels <- 2:7
ylevels

dta <- dta.list[[1]]
dta <- within(dta, {
  ybin2 <- ifelse(lead_fh1 >= 3, 1, 0)
  ybin3 <- ifelse(lead_fh1 >= 4, 1, 0) 
  ybin4 <- ifelse(lead_fh1 >= 5, 1, 0)
  ybin5 <- ifelse(lead_fh1 >= 6, 1, 0)
  ybin6 <- ifelse(lead_fh1 >= 7, 1, 0)
  }
)
with(dta, table(lead_fh1, ybin2))
gen.glm <- function(x){
  glm(
    as.formula(paste0(x, '~', ivTerms)), 
    family="binomial", data = dta
  )
}
mod.list <- lapply(paste0('ybin', 2:6), gen.glm)
summary(mod.list)
coef.data <- data.frame(sapply(mod.list, coef))
names(coef.data) <- paste0('yit>=', 3:7)
coef.data[, 'vars'] <- rownames(coef.data)
head(coef.data)
names(coef)
coef.data <- reshape2::melt(
  coef.data,
  id.vars = 'vars',
  measure.vars = 1:5
)

qplot(data = subset(coef.data, vars == 'cooptation'), x = variable, y = value, geom = 'point')

   +
  facet_grid(.~vars)


head(coef.data)
