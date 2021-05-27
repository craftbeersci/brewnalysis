#************************FUNCTION USED FOR BREWNALYSIS**************************


#****************************CELL DENSITY***************************************
#For this function, the size of the square is set a default value
#The default value length is 0.2 and the default value for the depth is 0.1
#To change these default value type cell_count_func(data,L="enter the length of 
#the square",D="Enter the depth of the square")

#***************************DATA HOMOGENEITY ***********************************

data_homog <- function (ferment_data,L=0.2,D=0.1) {
   
    volume = (L*L*D)*10^-3
    squares_counted = ferment_data$square_count
    cell_count = ferment_data$cell_count
    dilution = ferment_data$dilution_factor
    cell_density = ((cell_count/squares_counted)*dilution)/volume 
    
  
return(cell_density)
}
  




#*******************************************************************************


#TvecFunc that uses the 'date' therefore cannot be used with all datasets
TvecFunc_date<-function(ferment_data){
  Time<-ferment_data[,c('date','sample_time')]
  Time<-paste(Time$date, format(as.POSIXct(Time$sample_time), '%T'))
  Tvec = 0
  for(i in 2:(length(Time))) {
    start<- Time[i-1]
    finish<- Time[i]
    A<-difftime(finish,start,units="hours")
    Tvec = c(Tvec,A)
  }
  print(Tvec)
 z<-data.frame(Tvec)
  return(z)
}  

Cell_dens_func_date = function(ferment_data, L=.2,D=.1) {
  celldensity = ferment_data$celldensity
  tvec = TvecFunc_date(ferment_data)
  results = data.frame(tvec,celldensity)
  return(results)
}

# TvecFunc that uses the 'fermentation_day' 

TvecFunc_fermentation_day<-function(ferment_data){
  
  fermentation_day<-data.frame(ferment_data$fermentation_day)
  fermentation_hour<-fermentation_day*24
  
  Time<-ferment_data$sample_time
  Tvec = 0
  for(i in 2:(length(Time))) {
    start<- Time[i-1]
    finish<- Time[i]
    A<-difftime(finish,start,units="hours")
    Tvec = c(Tvec,A)
  }

  Tvec= fermentation_hour+ Tvec
  print(Tvec)
  z<-data.frame(Tvec)
  return(z)
}  


Cell_dens_func_fermentation_day = function(ferment_data, L=.2,D=.1) {
  celldensity = ferment_data$celldensity
  tvec = TvecFunc_fermentation_day(ferment_data)
  results = data.frame(tvec,celldensity)
  return(results)
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



