# Functions for calculating predicted values based on models of specific
# gravity, pH, and cells in suspension

###################
########## Specific Gravity Functions

#####
# predicted value for the log function from equation 1 in Reid et al. 2021
log_func <- function(log_par, tvec){
  pe <- log_par[1] # lower asymptote (i.e. final gravity, FG)
  pi <- log_par[2] # higher asymptote (i.e. original gravity, OG) 
  m <- log_par[3] # inflection point of curve
  b <- log_par[4] # rate of decrease at inflection point (max rate of decrease)
  log_pred <- pe + (pi-pe)/(1+exp(-b*(tvec-m)))
}

#####
# predicted value for the generalized logistic function (eq. 2 in Reid et al. 2021)
gen_log_func <- function(gen_log_par, tvec){
  pe <- gen_log_par[1] # lower asymptote (i.e. final gravity, FG)
  pi <- gen_log_par[2] # higher asymptote (i.e. original gravity, OG) 
  m <- gen_log_par[3] # inflection point of curve
  b <- gen_log_par[4] # rate of decrease at inflection point (max rate of decrease)
  s <- gen_log_par[5] # asymmetric adjustment parameter
  gen_log_pred <- pe + (pi-pe)/((1+s*exp(-b*(tvec-m)))^(1/s))
  # TODO check out this s parameter, I don't know if it's actually multiplied or what?
}

#####
# predicted values for the exponential (decay) function
exp_func <- function(exp_par, tvec){
  n0 <- exp_par[1] # OG of fermentation
  lambda <- exp_par[2] # Decay constant
  fg <- exp_par[3] #FG of fermentation
  exp_pred <- fg + (n0 * exp(-lambda*tvec))
}

###################
# Functions for pH





###################
# Function for cells in suspension

#gama
gam_func <- function(gam_par, tvec){
  k <- gam_par[1]
  theta <- gam_par[2]
  gam_pred <- tvec^(k-1)*exp(-tvec/theta)
  return(gam_pred)
}


#gama+
gam_a_func <- function(gam_a_par, tvec){
  a <- gam_a_par[1]
  k <- gam_a_par[2]
  theta <- gama_a_par[3]
  gam_a_pred <- a+tvec^(k-1)*exp(-tvec/theta)
  return(gam_a_pred)
}
