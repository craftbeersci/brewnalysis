# Functions for calculating predicted values based on models of specific
# gravity, pH, and cells in suspension

###################
### Specific Gravity Functions

# predicted value for the log function from equation 1 in Reid et al. 2021
log_func <- function(log_par, tvec){
  pe <- log_par[1] # lower asymtote (i.e. final gravity, FG)
  pi <- log_par[2] # higher asymtote (i.e. original gravity, OG) 
  m <- log_par[3] # inflection point of curve
  b <- log_par[4] # rate of decrease at inflection point (max rate of decrease)
  log_pred <- pe + (pi-pe)/(1+exp(-b(tvec-m)))
}

# predicted value for the generalized logistic function (eq. 2 in Reid et al. 2021)
gen_log_func <- lfunction(gen_log_par, tvec){
  pe <- gen_log_par[1] # lower asymtote (i.e. final gravity, FG)
  pi <- gen_log_par[2] # higher asymtote (i.e. original gravity, OG) 
  m <- gen_log_par[3] # inflection point of curve
  b <- gen_log_par[4] # rate of decrease at inflection point (max rate of decrease)
  s <- gen_log_par[5] # asymmetric sdjustment parameter
  gen_log_pred <- pe + (pi-pe)/((1+s*exp(-b(tvec-m)))^(1/s))
  # TODO check out this s parameter, I don't know if it's actually multiplied or what?
}

# predicted value from the Apparent Degree of Fermentation (ADF)
# From the paper: "The difficulty with the use of this method as reported is that the
# Original Gravity or P0 must be fixed rather than be free to allow the best fit. Practically, P0
# is set to the largest density value in the fermentation dataset. This procedure is analogous
# to assuming or fixing an intercept in linear regression, limiting one’s ability to accurately
# predict the change in the dependent variable ADFt"

adf_func <- function(log_par, tvec){
  p0 <- adf_par[1] # lower asymtote (i.e. final gravity, FG)
  pi <- adf_par[2] # higher asymtote (i.e. original gravity, OG) 
  m <- adf_par[3] # inflection point of curve
  b <- adf_par[4] # rate of decrease at inflection point (max rate of decrease)
  log_pred <- pe + (pi-pe)/(1+exp(-b(tvec-m)))
}

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
