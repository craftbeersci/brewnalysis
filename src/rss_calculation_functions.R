# Functions for calculating the residual sums of squares (RSS) for specific gravity
# pH, and cells in suspension

###################
########## Specific Gravity Functions

#####
# RSS calculation for the log function from equation 1 in Reid et al. 2021
log_rss <- function(vars, tvec, obs){
  y_pred <- log_func(vars, tvec)
  log_rss <- sum((y_pred-obs)^2)
  return(log_rss)
}

#####
# RSS calculation for the generalized logistic function (eq. 2 in Reid et al. 2021)
gen_log_rss <- function(vars, tvec, obs){
  y_pred <- gen_log_func(vars, tvec)
  gen_log_rss <- sum((y_pred-obs)^2)
  return(gen_log_rss)
}

#####
# RSS calculation for the exponential (decay) function
exp_rss <- function(vars, tvec, obs){
  y_pred <- exp_func(vars, tvec)
  exp_rss <- sum((y_pred-obs)^2)
  return(exp_rss)
}






###################
# Functions for pH





###################
# Function for cells in suspension

#gama
gam_rss <- function(vars, tvec, obs){
  y_pred <- gam_func(vars, tvec)
  gam_rss <- sum((y_pred-obs)^2)
  return(gam_rss)
}

#gam +
gam_a_rss <- function(vars, tvec, obs){
  ypred <- gamAFunc(vars, tvec)
  gam_a_rss <- sum((ypred-obs)^2)
  return(gam_a_rss)
}


