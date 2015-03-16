clusterRobustSe <- function(fm, cluster){
  require(sandwich)
  #require(lmtest)
  M <- length(unique(cluster))
  N <- length(cluster)
  dfc <- (M/(M-1))*((N-1)/(N-length(c(coef(fm), fm[['zeta']]))))
  u <- apply(
    estfun(fm),
    2,
    function(x) { tapply(x, cluster, sum) }
  )
  return(vcovCL <- dfc*sandwich(fm, meat=crossprod(u)/N))
  #coeftest(fm, vcovCL) 
}