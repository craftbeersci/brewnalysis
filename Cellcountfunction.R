#*************************************LEGEND****************************************** 
#ACPSS=average cells per small square                                                *
#DF=dilution factor                                                                  *
#VSS=volume small square (mL)                                                        *
#                                                                                    *
#dataset has to be organized in the same way than data in brewnalysis_test_data.xlsx.*
#If columns are different, change the corresponding number of columns r.24 & 26      *
#*************************************************************************************

#************************IMPORT DATA**************************************************
library("readxl")
data <- read_excel("brewnalysis_test_data.xlsx") 
data  #verify if dataset well imported

batch2<-data[8:13,]
batch2

#************************PRE REQUISITE*********************************************** 
#knowing the size of the square
L <- readline(prompt="Enter square side length: ")
W <- readline(prompt="Enter width: ")

#************************FUNCTION****************************************************

cellcountFunc<- function(x, L, W) {
  ACPSS=c(x[,11]+x[,14])
  #ACPSS
  DF=c(x[,10])
  #DF
  VSS=as.numeric(L)*as.numeric(L)*as.numeric(W)
  #VSS
  y<-data.frame(ACPSS,DF,VSS)
  #y 
  #table of 3 columns. 1st column= average cells per small square ; 2nd column=dilution factor ; 3rd column=volume small square (mL)
  
  
  for (i in 1:nrow(y))
    celldensity=c((y[,1]*y[,2])/y[,3])
  return(celldensity)
}

#************************RESULT****************************************************

#celldens = cellcountFunc(data)
celldens = cellcountFunc(batch2$)
celldens


#************************CELL DENSITY/TIME FUNCTION****************************************************
TvecFunc<-function(x){
  Time<-x[,5:6]
  Time
  Time<-paste(Time$Date, format(as.POSIXct(Time$`Sample Time`), '%T'))
  Time
  for(i in 2:(length(Time))) {
    start<- Time [1]
    finish<- Time[i]
    A<-difftime(finish,start,units="hours")
    Tvec = c(Tvec,A)
  }
  print(Tvec)
  z<-data.frame(Tvec,celldens)
  return(z)
}

#************************RESULT****************************************************
celldenstime=TvecFunc(batch2)
celldenstime
plot(celldenstime)

#************************Fit gamFunc****************************************************

#predicted values for the basic gama function
gamFunc <- function(gamPar, tvec){
  k <- gamPar[1]
  theta <- gamPar[2]
  gamPred <- tvec^(k-1)*exp(-tvec/theta)
  return(gamPred)
}

tvec = celldenstime$Tvec
gamPred = celldenstime$celldens

obj = function(gamPar){-gamFunc(gamPar,tvec)}
# obj(c(0,0),gamPar)
out<-optim(par=c(1,1),fn=obj,method="Nelder-Mead")

gamFunc(c(0,0),tvec)
