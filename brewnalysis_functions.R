#************************FUNCTION USED FOR BREWNALYSIS**************************
#*

#**************************CONDITION********************************************

ConditionFunc<-function(x){
  cond1<-c(paste(x[1,1],"batch",x[1,2]))
  return(cond1)
}

#****************************CELL DENSITY***************************************
#For this function, the size of the square is set a default value
#The default value length is 0.2 and the default value for the depth is 0.1
#To change these default value type cellcountFunc(data,L="enter the length of the square",D="Enter the depth of the square")
cellcountFunc<- function(x,L=0.2,D=0.1) {
  ACPSS=c(x[,11]+x[,14])
  #ACPSS
  DF=c(x[,10])
  #DF
  VSS=as.numeric(L)*as.numeric(L)*as.numeric(D)
  VSS
  y<-data.frame(ACPSS,DF,VSS)
  y
  #table of 3 columns. 1st column= average cells per small square ; 2nd column=dilution factor ; 3rd column=volume small square (mL)
  
  
  for (i in 1:nrow(y))
    celldensity=c((y[,1]*y[,2])/y[,3])
  return(celldensity)
}


#*******************************************************************************

TvecFunc<-function(x){
  Time<-x[,5:6]
  Time<-paste(Time$Date, format(as.POSIXct(Time$`Sample Time`), '%T'))
  Tvec = numeric()
  for(i in 1:(length(Time))) {
    start<- Time [1]
    finish<- Time[i]
    A<-difftime(finish,start,units="hours")
    Tvec = c(Tvec,A)
  }
  print(Tvec)
  z<-data.frame(Tvec,celldens)
  return(z)
  
}


TvecFuncATB<-function(x,y){
  Time<-x[,5:6]
  Time<-paste(Time$Date, format(as.POSIXct(Time$`Sample Time`), '%T'))
  Tvec = numeric()
  for(i in 1:(length(Time))) {
    start<- Time [1]
    finish<- Time[i]
    A<-difftime(finish,start,units="hours")
    Tvec = c(Tvec,A)
  }
  # print(Tvec)
  # z<-data.frame(Tvec,y)
  return(Tvec)
  
}

#*******************************************************************************
#predicted values for the gama function with an amplitude parameter.
gamFunc <- function(gamPar, tvec){
  k <- gamPar[1]
  theta <- gamPar[2]
  gamPred <- tvec^(k-1)*exp(-tvec/theta)
  return(gamPred)
}

gamDevSq <- function(vars, tvec, obs){
  ypred <- gamFunc(vars, tvec)
  DevSq <- (ypred-obs)^2
  out <- sum(DevSq)
  return(out)
}

#*******************************************************************************
#predicted values for the gama + function with an amplitude parameter.
gamAFunc <- function(gamAPar, tvec){
  A <- gamAPar[1]
  k <- gamAPar[2]
  theta <- gamAPar[3]
  gamAPred <- A+tvec^(k-1)*exp(-tvec/theta)
}

gamADevSq <- function(vars, tvec, obs){
  ypred <- gamAFunc(vars, tvec)
  DevSq <- (ypred-obs)^2
  out <- sum(DevSq)
  return(out)
}

#*******************************************************************************



