<<<<<<< HEAD
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
=======
library("readxl")
library("ggplot2")
library(grid)
data <- read_excel("~/Cellcountfunction/brewnalysis_test_data.xlsx") 
>>>>>>> 89b6fae54475771b1f6757a4d6bedbacc8ce10ea
data  #verify if dataset well imported

batch2<-data[8:13,]
batch2

<<<<<<< HEAD
=======

>>>>>>> 89b6fae54475771b1f6757a4d6bedbacc8ce10ea
#************************PRE REQUISITE*********************************************** 
#knowing the size of the square
L <- readline(prompt="Enter square side length: ")
W <- readline(prompt="Enter width: ")

#************************FUNCTION****************************************************

<<<<<<< HEAD
cellcountFunc<- function(x, L, W) {
=======
cellcountFunc<- function(x) {
>>>>>>> 89b6fae54475771b1f6757a4d6bedbacc8ce10ea
  ACPSS=c(x[,11]+x[,14])
  #ACPSS
  DF=c(x[,10])
  #DF
  VSS=as.numeric(L)*as.numeric(L)*as.numeric(W)
<<<<<<< HEAD
  #VSS
  y<-data.frame(ACPSS,DF,VSS)
  #y 
=======
  VSS
  y<-data.frame(ACPSS,DF,VSS)
  y
>>>>>>> 89b6fae54475771b1f6757a4d6bedbacc8ce10ea
  #table of 3 columns. 1st column= average cells per small square ; 2nd column=dilution factor ; 3rd column=volume small square (mL)
  
  
  for (i in 1:nrow(y))
    celldensity=c((y[,1]*y[,2])/y[,3])
  return(celldensity)
}

#************************RESULT****************************************************

#celldens = cellcountFunc(data)
<<<<<<< HEAD
celldens = cellcountFunc(batch2$)
celldens

=======
celldens = cellcountFunc(batch2)
celldens
#************************CONDITION FUNCTION****************************************************
ConditionFunc<-function(x){
  cond1<-c(paste(x[1,1],"batch",x[1,2]))
  return(cond1)
}
#************************RESULT****************************************************
Cond1=ConditionFunc(batch2)
Cond1
>>>>>>> 89b6fae54475771b1f6757a4d6bedbacc8ce10ea

#************************CELL DENSITY/TIME FUNCTION****************************************************
TvecFunc<-function(x){
  Time<-x[,5:6]
<<<<<<< HEAD
  Time
  Time<-paste(Time$Date, format(as.POSIXct(Time$`Sample Time`), '%T'))
  Time
  for(i in 2:(length(Time))) {
=======
  Time<-paste(Time$Date, format(as.POSIXct(Time$`Sample Time`), '%T'))
  Tvec = numeric()
  for(i in 1:(length(Time))) {
>>>>>>> 89b6fae54475771b1f6757a4d6bedbacc8ce10ea
    start<- Time [1]
    finish<- Time[i]
    A<-difftime(finish,start,units="hours")
    Tvec = c(Tvec,A)
  }
  print(Tvec)
  z<-data.frame(Tvec,celldens)
  return(z)
<<<<<<< HEAD
}

#************************RESULT****************************************************
celldenstime=TvecFunc(batch2)
celldenstime
plot(celldenstime)
=======
  
}
#************************RESULT****************************************************
celldenstime=TvecFunc(batch2)
celldenstime

# plot(celldenstime)


#Possible to merge these 2 functions depending on the result we want to obtain
>>>>>>> 89b6fae54475771b1f6757a4d6bedbacc8ce10ea

#************************Fit gamFunc****************************************************

#predicted values for the basic gama function
gamFunc <- function(gamPar, tvec){
  k <- gamPar[1]
  theta <- gamPar[2]
  gamPred <- tvec^(k-1)*exp(-tvec/theta)
  return(gamPred)
}

<<<<<<< HEAD
tvec = celldenstime$Tvec
gamPred = celldenstime$celldens

obj = function(gamPar){-gamFunc(gamPar,tvec)}
# obj(c(0,0),gamPar)
out<-optim(par=c(1,1),fn=obj,method="Nelder-Mead")

gamFunc(c(0,0),tvec)
=======
gamDevSq <- function(vars, tvec, obs){
  ypred <- gamFunc(vars, tvec)
  DevSq <- (ypred-obs)^2
  out <- sum(DevSq)
  return(out)
}

tvec = celldenstime$Tvec
gamPred = celldenstime$celldens
gamPar <- c(3,15)
fitted<-optim(par=gamPar,fn=gamDevSq,method="Nelder-Mead", tvec=tvec, obs=gamPred)
fitted

k<-fitted$par[1]
theta<-fitted$par[2]
Pval<-tvec^(k-1)*exp(-tvec/theta)
PVAL<-data.frame(tvec,Pval)
cond<-c("Model data")
PVAL<-data.frame(tvec,Pval,cond)
PVAL

a<-rep(Cond1,length(celldens))
b<-rep(cond,length(Pval))

Tvec<-c(tvec,tvec)
cell.density<-c(celldens,Pval)
Condition<-c(a,b)
DataFinal<-data.frame(Tvec,cell.density,Condition)
DataFinal

plot<- ggplot(data=DataFinal,aes(x=Tvec,y=cell.density))+geom_point(aes(colour = factor(Condition)))+geom_line(aes(colour = factor(Condition)))+labs(title = "Cell density in wort according to fermentation time", x="Fermentation time (hours)",y="Cell density")
plot

>>>>>>> 89b6fae54475771b1f6757a4d6bedbacc8ce10ea
