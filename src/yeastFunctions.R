library("fBasics")
library("dfoptim")

#predicted values for the basic gama function
gamFunc <- function(gamPar, tvec){
  k <- gamPar[1]
  theta <- gamPar[2]
  gamPred <- tvec^(k-1)*exp(-tvec/theta)
}

#predicted values for the gama function with an amplitude parameter.
gamAFunc <- function(gamAPar, tvec){
  A <- gamAPar[1]
  k <- gamAPar[2]
  theta <- gamAPar[3]
  gamAPred <- A+tvec^(k-1)*exp(-tvec/theta)
}

#predicted values for the normal function
normFunc <- function(normPar, tvec){
  A <- normPar[1]
  mu <- normPar[2]
  sig <- normPar[3]
  normPred <- A*exp(((tvec-mu)^2/(-2*sig^2)))
}

#predicted values for the tilted normal function
tiltFunc <- function(tiltPar, tvec){
  A <- tiltPar[1]
  mu <- tiltPar[2]
  sig <- tiltPar[3]
  R <- tiltPar[4]
  tiltPred <- R*tvec + A*exp(((tvec-mu)^2/(-2*sig^2)))
}

#predicted values for the MacIntosh function
macFunc <- function(macPar, tvec){
  A <- macPar[1]
  mu <- macPar[2]
  sig <- macPar[3]
  S <- macPar[4]
  macPred <- A*exp(-.5*((tvec-mu)/(sig/(1+Heaviside(tvec, a = mu)*S)))^2)/(1+Heaviside(tvec, a = mu)*S)+(A-A/(1+S))*Heaviside(tvec, a = mu)
}

######################################################
#5 populaion functions to minimize the sums of squares
gamDevSq <- function(vars, tvec, obs){
  ypred <- gamFunc(vars, tvec)
  DevSq <- (ypred-obs)^2
  out <- sum(DevSq)
  return(out)
}

gamADevSq <- function(vars, tvec, obs){
  ypred <- gamAFunc(vars, tvec)
  DevSq <- (ypred-obs)^2
  out <- sum(DevSq)
  return(out)
}

#normal function populatin curve
normDevSq <- function(vars, tvec, obs){
  ypred <- normFunc(vars, tvec) #calculates the predicted values
  DevSq <- (ypred-obs)^2 # calculates the square deviations
  out <- sum(DevSq) #sums the sq devs.
  return(out) #returns the sq devs from function
}

tiltDevSq <- function(vars, tvec, obs){
  ypred <- tiltFunc(vars, tvec)
  DevSq <- (ypred-obs)^2
  out <- sum(DevSq)
  return(out)
}

macDevSq <- function(vars, tvec, obs){
  ypred <- macFunc(vars, tvec)
  DevSq <- (ypred-obs)^2
  out <- sum(DevSq)
  return(out)
}



#This function falls all of the optimization functions and returns a list of [[1]] aicList, [[2]] deltAIC, [[3]]normFit, [[4]]tiltFit [[5]] macFit, [[6]] gamFit, [[7]] gamAFit
#the macFunc can optimize better with either the tiltfit parms or the normfit parms depending on the time serries
fitFunc <- function(normGuess, tiltGuess, macGuess, gamGuess, gamAGuess, tvec, obs){
  gamFit <- optim(par = gamGuess, fn = gamDevSq, method = "Nelder-Mead", tvec = tvec, obs = obs)
  gamAGuess[2:3] <-gamFit$par
  gamAFit <- optim(par = gamAGuess, fn = gamADevSq, method = "Nelder-Mead", tvec = tvec, obs = obs)
  normFit <- optim(par = normGuess, fn = normDevSq, method = "Nelder-Mead", tvec = tvec, obs = obs)  #call all 5 functions and optimize their parameters
  tiltFit<- optim(par = tiltGuess, fn = tiltDevSq, method = "Nelder-Mead", tvec = tvec, obs = obs)
  # macGuess[1:3] <- tiltFit$par[1:3]
  macGuess[1:3] <- normFit$par
  macFit <- optim(par = macGuess, fn = macDevSq, method = "Nelder-Mead", tvec = tvec, obs = obs)

  N <- length(obs)
  
  #calculate AIC values for all 5 model fits
  gamAIC <- 2*length(gamGuess) + N*log(gamFit$value/N)
  gamAAIC <- 2*length(gamAGuess) + N*log(gamAFit$value/N)
  normAIC <- 2*length(normGuess) + N*log(normFit$value/N) 
  tiltAIC <- 2*length(tiltGuess) + N*log(tiltFit$value/N)
  macAIC <- 2*length(macGuess) + N*log(macFit$value/N)

  
  #This calculates the correction value AICc
  gamAICc <- gamAIC + ((2*length(gamGuess)^2) + 2*length(gamGuess))/(N-length(gamGuess)-1)
  gamAAICc <- gamAAIC + ((2*length(gamAGuess)^2) + 2*length(gamAGuess))/(N-length(gamAGuess)-1)
  normAICc <- normAIC + ((2*length(normGuess)^2) + 2*length(normGuess))/(N-length(normGuess)-1)
  tiltAICc <- tiltAIC + ((2*length(tiltGuess)^2) + 2*length(tiltGuess))/(N-length(tiltGuess)-1)
  macAICc <- macAIC + ((2*length(macGuess)^2) + 2*length(macGuess))/(N-length(macGuess)-1)
  
  
  aiccList <- c(gamAICc,gamAAICc,normAICc,tiltAICc,macAICc) #make list of the AIC values to return
  deltAICc <- aiccList - min(aiccList) #calculate delta aic values to return
  results <- list(aiccList, deltAICc, gamFit, gamAFit, normFit, tiltFit, macFit, tvec, obs)
  names(results) <- c("AICc", "deltAICc", "gam", "gamA", "norm", "tilt", "mac", "tvec", "obs")
  return(results)  #sptit out the optim results, aic, and delta aic values.
}

#this function fits all 5 models for batches of data sets
#"data" should be a data frame with ID, time, and count columns
#guessVars contains initial parameters for optimization in the following order:
#tilt, step, k, theta, gAmp
#amp, mu, and sig are estimated from each data set individually
#IDList is a list of the individual/unique ID names
batchFit <- function(data, guessVars, IDList){
  fits <- lapply(IDList, function(x){
    dat <- data.frame(time = subset(data, ID == x, select = time), count = subset(data, ID == x, select = count))
    amp <- max(dat$count)
    mu <- dat$time[which.max(dat$count)]
    sig <- sd(dat$time)
    normGuess <- c(amp, mu, sig)
    tiltGuess <- c(amp, mu, sig, tilt)
    macGuess <- c(amp, mu, sig, step)
    gamGuess <- c(k, theta)
    gamAGuess <- c(gAmp, k, theta)
    fitFunc( 
      normGuess = normGuess,
      tiltGuess = tiltGuess,
      macGuess = macGuess,
      gamGuess = gamGuess,
      gamAGuess = gamAGuess,
      tvec = dat$time,
      obs = dat$count
    )})
  names(fits) <- IDList
  return(fits)
}

#this function simulates data from an observed data set by using the MacIntosh function for modeling yeast in suspension
#Data is generated by sampling at each simTvec time point from a normal distribution
#with mean = expected value of the MacIntosh function for the progenitor data
#and sd = sd of the residuals of the progenitor data and predicted values from the MacIntosh function
#Output for the simNum data sets is identical in format to the batchFit output 
simFit <- function(dataFits, progID, simNum, simTvec){
  sims <- c(1:simNum)
  simPar <- dataFits[[progID]]$mac$par #set which data set you want to use with these three rows (change 'C2')
  tvec <- dataFits[[progID]]$tvec
  obs <- dataFits[[progID]]$obs
  A <- simPar[1] # amplitude parameter
  mu <- simPar[2] # mean parameter
  sig <- simPar[3] # s.d. parameter
  S <- simPar[4] # step height parameter.
  
  #simulating the data!
  ypred <- A*exp(-.5*((tvec-mu)/(sig/(1+Heaviside(tvec, a = mu)*S)))^2)/(1+Heaviside(tvec, a = mu)*S)+(A-A/(1+S))*Heaviside(tvec, a = mu)
  simSD <- sqrt(sum((ypred-obs)^2)/length(obs))
  realY <- A*exp(-.5*((simTvec-mu)/(sig/(1+Heaviside(simTvec, a = mu)*S)))^2)/(1+Heaviside(simTvec, a = mu)*S)+(A-A/(1+S))*Heaviside(simTvec, a = mu)
  simDat <- as.data.frame(sapply(sims, function(y) sapply(realY, function (x) rnorm(1, mean = x, sd = simSD))))
  simDat <- as.data.frame(unlist(as.data.frame(simDat), use.names=FALSE))
  names(simDat) <- 'count'
  simDat$ID <- rep(sims, each = length(simTvec))
  simDat$time <- rep(simTvec)
  
  #fit the models to the simulated data using batchFit
  simFits <- batchFit(data = simDat, guessVars, IDList = sims)
  return(simFits)
}

#this function calculates the mean square error (MSE) of all the parameters in 1 model for all the simulations
#this function is primarily called by the MSE analysis function.
#this function requires simPars and progPars that are calculated by the MSEanalysis function
simMSE <- function(simPars, progPars, model){
  #calculates the variance of each parameter individually for all the simulations
  simVar <- sapply(c(1:nrow(simPars)), function(x) var(simPars[x,]))
  #calculates the bias of each parameter.  variable mean - progenetor mean
  simBias <- sapply(c(1:nrow(simPars)), function(x) mean(simPars[x,]) - progPars[[model]][[x]])
  #calculates the MSE as var + bias^2
  simMSE <- simVar + simBias^2
  MSEresults <- list(simVar, simBias, simMSE)
  names(MSEresults) <- c("variance", "bias", "MSE")
  return(MSEresults)
}

#this function uses the simMSE function to calculate the MSE for all 5 models.  
#Output includes the variance, bias, and MSE for each model
#this function requires the progID (progenitor ID) from the original data set.
#this function also requires the fitted results of the original data and the fitted data 
#see yeastSimFits & yeastDataFits files for more information on those
MSEanalysis <- function(progID, dataFits, simFits){
  #prarameters from the progenetor of the simulations
  progPars <- list(dataFits[[progID]]$norm$par, dataFits[[progID]]$tilt$par, dataFits[[progID]]$mac$par, dataFits[[progID]]$gam$par, dataFits[[progID]]$gamA$par)
  
  #parameters for each model from the simulated data
  simNormPar <- sapply(c(1:length(simFits)), function(x) simFits[[x]]$norm$par)
  simTiltPar <- sapply(c(1:length(simFits)), function(x) simFits[[x]]$tilt$par)
  simMacPar <- sapply(c(1:length(simFits)), function(x) simFits[[x]]$mac$par)
  simGamPar <- sapply(c(1:length(simFits)), function(x) simFits[[x]]$gam$par)
  simGamAPar <- sapply(c(1:length(simFits)), function(x) simFits[[x]]$gamA$par)
  
  #calculates the variance, bias, and MSE using the simMSE function for each model
  normMSEresults <- simMSE(simNormPar, progPars, model = 1)
  tiltMSEresults <- simMSE(simTiltPar, progPars, model = 2)
  macMSEresults <- simMSE(simMacPar, progPars, model = 3)
  gamMSEresults <- simMSE(simGamPar, progPars, model = 4)
  gamAMSEresults <- simMSE(simGamAPar, progPars, model = 5)
  MSEresults <- list(normMSEresults, tiltMSEresults, macMSEresults, gamMSEresults, gamAMSEresults)
  names(MSEresults) <- c('normMSEresults', 'tiltMSEresults', 'macMSEresults', 'gamMSEresults', 'gamAMSEresults')
  return(MSEresults)
}



