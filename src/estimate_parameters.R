# Functions for estimating parameters for specific gravity pH, and cells in suspension

###################
########## Specific Gravity

#####
# Estimate parameters for the log function from equation 1 in Reid et al. 2021
est_log_pars <- function(log_guess, tvec, obs){
  log_fit <- optim(par = log_guess, fn = log_rss, method = "Nelder-Mead", tvec = tvec, obs = obs)
  log_pars <- c(log_fit$par,log_fit$value)
  return(list(log_pars))
}

#####
# Estimate parameters for the generalized logistic function (eq. 2 in Reid et al. 2021)
est_gen_log_pars <-function(gen_log_guess, tvec, obs){
  gen_log_fit <- optim(par = gen_log_guess, fn = gen_log_rss, method = "Nelder-Mead", tvec = tvec, obs = obs)
  gen_log_pars <- c(gen_log_fit$par, gen_log_fit$value)
  return(list(gen_log_pars))
}

#####
# Estimate parameters for the exponential (decay) function
est_exp_pars <- function(exp_guess, tvec, obs){
  exp_fit <- optim(par = exp_guess, fn = exp_rss, method = "Nelder-Mead", tvec = tvec, obs = obs)
  exp_pars <- c(exp_fit$par, exp_fit$value)
  return(list(exp_pars))
}

###################
# Functions for pH





###################
# Function for cells in suspension

#gama
tvec = cell_density$Tvec
gamPred = cell_density$celldensity
gamPar <- c(3,15)
fitted<-optim(par=gamPar,fn=gamDevSq,method="Nelder-Mead", tvec=tvec, obs=gamPred)

#gama+
tvec_plus = cell_density$Tvec
gamAPred = cell_density$celldensity
gamAPar <- c(1,3,15)
fittedA<-optim(par=gamAPar,fn=gamADevSq,method="Nelder-Mead", tvec=tvec, obs=gamAPred)
