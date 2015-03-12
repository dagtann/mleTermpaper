meanArray <- function(x) { Reduce("+", x)/length(x) }
## Uncomment for example
# exList <- list(
#   imp1 = matrix(2:5, ncol = 2), 
#   imp2 = matrix(3:6, ncol = 2),
#   imp3 = matrix(4:7, ncol = 2)
# )
# exList; add(exList)
## function to average over cell entries in 3dim array
## no missing allowed!