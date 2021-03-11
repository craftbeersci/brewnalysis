# Functions for estimating parameters for specific gravity pH, and cells in suspension

###################
########## Specific Gravity

#####
# Estimate parameters for the log function from equation 1 in Reid et al. 2021
est_log_pars <- function(log_guess, tvec, obs){
  log_pars <- optim(par = log_guess, fn = log_rss, method = "Nelder-Mead", tvec = tvec, obs = obs)
  log_pars <- log_pars$par
  return(list(log_pars))
  # return(log_pars)
}

#####
# Estimate parameters for the generalized logistic function (eq. 2 in Reid et al. 2021)
est_gen_log_pars <-function(gen_log_guess, tvec, obs){
  log_pars <- optim(par = gen_log_guess, fn = gen_log_rss, method = "Nelder-Mead", tvec = tvec, obs = obs)
  log_pars <- log_pars$par
  return(list(log_pars))
  # return(log_pars)
}

#####
# Estimate parameters for the exponential (decay) function
est_exp_pars <- function(exp_guess, tvec, obs){
  exp_pars <- optim(par = exp_guess, fn = exp_rss, method = "Nelder-Mead", tvec = tvec, obs = obs)
  exp_pars <- exp_pars$par
  return(list(exp_pars))
  # return(exp_pars)
}

###################
# Functions for pH





###################
# Function for cells in suspension
