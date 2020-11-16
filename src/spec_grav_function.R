library("fBasics")
library("dfoptim")

# predicted values for the logistic function
log_func <- function(log_par, tvec){
  x0 <- log_par[1]
  L <- log_par[2]
  k <- log_par[3]
  log_pred <- L/(1+exp(-k*(tvec-x0)))
}

#5 functions to calculate the sums of squares
log_dev_sq <- function(vars, tvec, obs){
  y_pred <- log_func(vars, tvec)
  dev_sq <- (y_pred-obs)^2
  out <- sum(dev_sq)
  return(out)
}

# log_fit <- optim(par = gamGuess, fn = gamDevSq, method = "Nelder-Mead", tvec = tvec, obs = obs)