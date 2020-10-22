library("fBasics")
library("dfoptim")

#predicted values for the logistic function
log_func <- function(log_par, tvec){
  x0 <- log_par[1]
  L <- log_par[2]
  k <- log_par[3]
  log_pred <- L/(1+exp(-k(tvec-x0)))
}