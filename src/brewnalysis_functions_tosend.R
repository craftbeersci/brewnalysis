#************************FUNCTION USED FOR BREWNALYSIS**************************


#****************************CELL DENSITY***************************************
#For this function, the size of the square is set a default value
#The default value length is 0.2 and the default value for the depth is 0.1
#To change these default value type cell_count_func(data,L="enter the length of 
#the square",D="Enter the depth of the square")

#***************************CELL DENSITY ************************************

cell_count_func <- function(ferment_data,L=0.2,D=0.1){
  volume = L*L*D
  squares_counted = ferment_data$cell_count_1_squares_counted
  cell_count = ferment_data$cell_count_1_live_cells
  dilution = ferment_data$dilution_factor
  ave_cells_sq = ((cell_count/squares_counted)*dilution)/volume
  return(ave_cells_sq)
}


#*******************************************************************************

TvecFunc<-function(ferment_data){
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




Cell_dens_func = function(ferment_data, L=.2,D=.1) {
  celldensity = cell_count_func(ferment_data, L, D)
  tvec = TvecFunc(ferment_data)
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



