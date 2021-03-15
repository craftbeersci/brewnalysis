# Functions for calculating the residual sums of squares (RSS) for specific gravity
# pH, and cells in suspension

###################
########## Specific Gravity Functions

#####
# RSS calculation for the log function from equation 1 in Reid et al. 2021
log_rss <- function(vars, tvec, obs){
  y_pred <- log_func(vars, tvec)
  dev_sq <- (y_pred-obs)^2
  out <- sum(dev_sq)
  return(out)
}

#####
# RSS calculation for the generalized logistic function (eq. 2 in Reid et al. 2021)
gen_log_rss <- function(vars, tvec, obs){
  y_pred <- gen_log_func(vars, tvec)
  dev_sq <- (y_pred-obs)^2
  out <- sum(dev_sq)
  return(out)
}

#####
# RSS calculation for the exponential (decay) function
exp_rss <- function(vars, tvec, obs){
  y_pred <- exp_func(vars, tvec)
  dev_sq <- (y_pred-obs)^2
  out <- sum(dev_sq)
  return(out)
}






###################
# Functions for pH





###################
# Function for cells in suspension

#gama
gamDevSq <- function(vars, tvec, obs){
  ypred <- gamFunc(vars, tvec)
  DevSq <- (ypred-obs)^2
  out <- sum(DevSq)
  return(out)
}

#gam +
gamADevSq <- function(vars, tvec, obs){
  ypred <- gamAFunc(vars, tvec)
  DevSq <- (ypred-obs)^2
  out <- sum(DevSq)
  return(out)
}


