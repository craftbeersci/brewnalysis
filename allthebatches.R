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
library("ggplot2")
library(grid)
data <- read_excel("~/Cellcountfunction/brewnalysis_test_data.xlsx") 
data  #verify if dataset well imported

batch1a = data[data$Brand=='beer_a'&data$`Batch Number`=='1',]
batch2a=data[data$Brand=='beer_a'&data$`Batch Number`=='2',]
batch3a=data[data$Brand=='beer_a'&data$`Batch Number`=='3',]
batch4a=data[data$Brand=='beer_a'&data$`Batch Number`=='4',]
batch5a=data[data$Brand=='beer_a'&data$`Batch Number`=='5',]
batch1b=data[data$Brand=='beer_b'&data$`Batch Number`=='1',]
batch2b=data[data$Brand=='beer_b'&data$`Batch Number`=='2',]
batch3b=data[data$Brand=='beer_b'&data$`Batch Number`=='3',]
batch4b=data[data$Brand=='beer_b'&data$`Batch Number`=='4',]
batch5b=data[data$Brand=='beer_b'&data$`Batch Number`=='5',]
batch1c=data[data$Brand=='beer_c'&data$`Batch Number`=='1',]
batch2c=data[data$Brand=='beer_c'&data$`Batch Number`=='2',]
batch3c=data[data$Brand=='beer_c'&data$`Batch Number`=='3',]
batch4c=data[data$Brand=='beer_c'&data$`Batch Number`=='4',]
batch5c=data[data$Brand=='beer_c'&data$`Batch Number`=='1',]
batch1d=data[data$Brand=='beer_d'&data$`Batch Number`=='2',]
batch2d=data[data$Brand=='beer_d'&data$`Batch Number`=='3',]
batch3d=data[data$Brand=='beer_d'&data$`Batch Number`=='3',]
batch4d=data[data$Brand=='beer_d'&data$`Batch Number`=='4',]
batch5d=data[data$Brand=='beer_d'&data$`Batch Number`=='5',]
batch1e=data[data$Brand=='beer_e'&data$`Batch Number`=='1',]
batch2e=data[data$Brand=='beer_e'&data$`Batch Number`=='2',]
batch3e=data[data$Brand=='beer_e'&data$`Batch Number`=='3',]
batch4e=data[data$Brand=='beer_e'&data$`Batch Number`=='4',]
batch5e=data[data$Brand=='beer_e'&data$`Batch Number`=='5',]
#************************PRE REQUISITE*********************************************** 
#knowing the size of the square
L <- readline(prompt="Enter square side length: ")
W <- readline(prompt="Enter width: ")

#************************FUNCTION****************************************************

cellcountFunc<- function(x) {
  ACPSS=c(x[,11]+x[,14])
  #ACPSS
  DF=c(x[,10])
  #DF
  VSS=as.numeric(L)*as.numeric(L)*as.numeric(W)
  VSS
  y<-data.frame(ACPSS,DF,VSS)
  y
  #table of 3 columns. 1st column= average cells per small square ; 2nd column=dilution factor ; 3rd column=volume small square (mL)
  
  
  for (i in 1:nrow(y))
    celldensity=c((y[,1]*y[,2])/y[,3])
  return(celldensity)
}

#************************RESULT****************************************************

#celldens = cellcountFunc(data)
celldens1a = cellcountFunc(batch1a)
celldens2a = cellcountFunc(batch2a)
celldens3a = cellcountFunc(batch3a)
celldens4a = cellcountFunc(batch4a)
celldens5a = cellcountFunc(batch5a)
celldens1b = cellcountFunc(batch1b)
celldens2b = cellcountFunc(batch2b)
celldens3b = cellcountFunc(batch3b)
celldens4b = cellcountFunc(batch4b)
celldens5b = cellcountFunc(batch5b)
celldens1c = cellcountFunc(batch1c)
celldens2c = cellcountFunc(batch2c)
celldens3c = cellcountFunc(batch3c)
celldens4c = cellcountFunc(batch4c)
celldens5c = cellcountFunc(batch5c)
celldens1d = cellcountFunc(batch1d)
celldens2d = cellcountFunc(batch2d)
celldens3d = cellcountFunc(batch3d)
celldens4d = cellcountFunc(batch4d)
celldens5d = cellcountFunc(batch5d)
celldens1e = cellcountFunc(batch1e)
celldens2e = cellcountFunc(batch2e)
celldens3e = cellcountFunc(batch3e)
celldens4e = cellcountFunc(batch4e)
celldens5e = cellcountFunc(batch5e)

#************************CONDITION FUNCTION****************************************************
ConditionFunc<-function(x){
  cond1<-c(paste(x[1,1],"batch",x[1,2]))
  return(cond1)
}
#************************RESULT****************************************************
Cond1a=ConditionFunc(batch1a)
Cond2a=ConditionFunc(batch2a)
Cond3a=ConditionFunc(batch3a)
Cond4a=ConditionFunc(batch4a)
Cond5a=ConditionFunc(batch5a)
Cond1b=ConditionFunc(batch1b)
Cond2b=ConditionFunc(batch2b)
Cond3b=ConditionFunc(batch3b)
Cond4b=ConditionFunc(batch4b)
Cond5b=ConditionFunc(batch5b)
Cond1c=ConditionFunc(batch1c)
Cond2c=ConditionFunc(batch2c)
Cond3c=ConditionFunc(batch3c)
Cond4c=ConditionFunc(batch4c)
Cond5c=ConditionFunc(batch5c)
Cond1d=ConditionFunc(batch1d)
Cond2d=ConditionFunc(batch2d)
Cond3d=ConditionFunc(batch3d)
Cond4d=ConditionFunc(batch4d)
Cond5d=ConditionFunc(batch5d)
Cond1e=ConditionFunc(batch1e)
Cond2e=ConditionFunc(batch2e)
Cond3e=ConditionFunc(batch3e)
Cond4e=ConditionFunc(batch4e)
Cond5e=ConditionFunc(batch5e)


#************************CELL DENSITY/TIME FUNCTION****************************************************
TvecFunc<-function(x,y){
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
#************************RESULT****************************************************
celldenstime1a=data.frame(TvecFunc(batch1a),celldens1a)
celldenstime2a=data.frame(TvecFunc(batch2a),celldens2a)
celldenstime3a=data.frame(TvecFunc(batch3a),celldens3a)
celldenstime4a=data.frame(TvecFunc(batch4a),celldens4a)
celldenstime5a=data.frame(TvecFunc(batch5a),celldens5a)
celldenstime1b=data.frame(TvecFunc(batch1b),celldens1b)
celldenstime2b=data.frame(TvecFunc(batch2b),celldens2b)
celldenstime3b=data.frame(TvecFunc(batch3b),celldens3b)
celldenstime4b=data.frame(TvecFunc(batch4b),celldens4b)
celldenstime5b=data.frame(TvecFunc(batch5b),celldens5b)
celldenstime1c=data.frame(TvecFunc(batch1c),celldens1c)
celldenstime2c=data.frame(TvecFunc(batch2c),celldens2c)
celldenstime3c=data.frame(TvecFunc(batch3c),celldens3c)
celldenstime4c=data.frame(TvecFunc(batch4c),celldens4c)
celldenstime5c=data.frame(TvecFunc(batch5c),celldens5c)
celldenstime1d=data.frame(TvecFunc(batch1d),celldens1d)
celldenstime2d=data.frame(TvecFunc(batch2d),celldens2d)
celldenstime3d=data.frame(TvecFunc(batch3d),celldens3d)
celldenstime4d=data.frame(TvecFunc(batch4d),celldens4d)
celldenstime5d=data.frame(TvecFunc(batch5d),celldens5d)
celldenstime1e=data.frame(TvecFunc(batch1e),celldens1e)
celldenstime2e=data.frame(TvecFunc(batch2e),celldens2e)
celldenstime3e=data.frame(TvecFunc(batch3e),celldens3e)
celldenstime4e=data.frame(TvecFunc(batch4e),celldens4e)
celldenstime5e=data.frame(TvecFunc(batch5e),celldens5e)


# plot(celldenstime)


#Possible to merge these 2 functions depending on the result we want to obtain

#************************Fit gamFunc****************************************************

#predicted values for the basic gama function
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

# tvec = celldenstime$Tvec
# gamPred = celldenstime$celldens
# gamPar <- c(3,15)
# fitted<-optim(par=gamPar,fn=gamDevSq,method="Nelder-Mead", tvec=tvec, obs=gamPred)
# fitted



# k<-fitted$par[1]
# theta<-fitted$par[2]
# Pval<-tvec^(k-1)*exp(-tvec/theta)
# PVAL<-data.frame(tvec,Pval)
# cond<-c("Model data")
# PVAL<-data.frame(tvec,Pval,cond)
# PVAL

a1<-rep(Cond1a,length(celldens1a))
a2<-rep(Cond2a,length(celldens2a))
a3<-rep(Cond3a,length(celldens3a))
a4<-rep(Cond4a,length(celldens4a))
a5<-rep(Cond5a,length(celldens5a))
b1<-rep(Cond1b,length(celldens1b))
b2<-rep(Cond2b,length(celldens2b))
b3<-rep(Cond3b,length(celldens3b))
b4<-rep(Cond4b,length(celldens4b))
b5<-rep(Cond5b,length(celldens5b))
c1<-rep(Cond1c,length(celldens1c))
c2<-rep(Cond2c,length(celldens2c))
c3<-rep(Cond3c,length(celldens3c))
c4<-rep(Cond4c,length(celldens4c))
c5<-rep(Cond5c,length(celldens5c))
d1<-rep(Cond1d,length(celldens1d))
d2<-rep(Cond2d,length(celldens2d))
d3<-rep(Cond3d,length(celldens3d))
d4<-rep(Cond4d,length(celldens4d))
d5<-rep(Cond5d,length(celldens5d))
e1<-rep(Cond1e,length(celldens1e))
e2<-rep(Cond2e,length(celldens2e))
e3<-rep(Cond3e,length(celldens3e))
e4<-rep(Cond4e,length(celldens4e))
e5<-rep(Cond5e,length(celldens5e))
# b<-rep(cond,length(Pval))

Tvec<-c(celldenstime1a$TvecFunc.batch1a.,celldenstime2a$TvecFunc.batch2a.,celldenstime3a$TvecFunc.batch3a.,celldenstime4a$TvecFunc.batch4a.,celldenstime5a$TvecFunc.batch5a.,celldenstime1b$TvecFunc.batch1b.,celldenstime2b$TvecFunc.batch2b.,celldenstime3b$TvecFunc.batch3b.,celldenstime4b$TvecFunc.batch4b.,celldenstime5b$TvecFunc.batch5b.,celldenstime1c$TvecFunc.batch1c.,celldenstime2c$TvecFunc.batch2c.,celldenstime3c$TvecFunc.batch3c.,celldenstime4c$TvecFunc.batch4c.,celldenstime5c$TvecFunc.batch5c.,celldenstime1d$TvecFunc.batch1d.,celldenstime2d$TvecFunc.batch2d.,celldenstime3d$TvecFunc.batch3d.,celldenstime4d$TvecFunc.batch4d.,celldenstime5d$TvecFunc.batch5d.,celldenstime1e$TvecFunc.batch1e.,celldenstime2e$TvecFunc.batch2e.,celldenstime3e$TvecFunc.batch3e.,celldenstime4e$TvecFunc.batch4e.,celldenstime5e$TvecFunc.batch5e.)
cell.density<-c(celldens1a,celldens2a,celldens3a,celldens4a,celldens5a,celldens1b,celldens2b,celldens3b,celldens4b,celldens5b,celldens1c,celldens2c,celldens3c,celldens4c,celldens5c,celldens1d,celldens2d,celldens3d,celldens4d,celldens5d,celldens1e,celldens2e,celldens3e,celldens4e,celldens5e)
Condition<-c(a1,a2,a3,a4,a5,b1,b2,b3,b4,b5,c1,c2,c3,c4,c5,d1,d2,d3,d4,d5,e1,e2,e3,e4,e5)
DataFinal<-data.frame(Tvec,cell.density,Condition)
DataFinal

plot<- ggplot(data=DataFinal,aes(x=Tvec,y=cell.density))+geom_point(aes(colour = factor(Condition)))+geom_line(aes(colour = factor(Condition)))+labs(title = "Cell density in wort according to fermentation time", x="Fermentation time (hours)",y="Cell density")
plot

