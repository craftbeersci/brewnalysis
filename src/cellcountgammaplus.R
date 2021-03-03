#*************************************LEGEND****************************************** 
#ACPSS=average cells per small square                                                *
#DF=dilution factor                                                                  *
#VSS=volume small square (mL)                                                        *
#                                                                                    *
#dataset has to be organized in the same way than data in brewnalysis_test_data.xlsx.*
#If columns are different, change the corresponding number of columns r.24 & 26      *
#*************************************************************************************

#************************IMPORT DATA********************************************
library("readxl")
library("ggplot2")
library(grid)
source("brewnalysis_functions.R")
data <- read_excel("~/Cellcountfunction/brewnalysis_test_data.xlsx") 
data  #verify if dataset well imported

batch2<-data[8:13,]
batch2

#************************Condition**********************************************

Cond1=ConditionFunc(batch2)
#Cond1


#************************Cell density*******************************************

#For this function, the size of the square is set a default value
#The default value length is 0.2 and the default value for the depth is 0.1
#To change these default value type cellcountFunc(data,L="enter the length of the square",D="Enter the depth of the square")
celldens = cellcountFunc(batch2)

#************************CELL DENSITY/TIME *************************************

celldenstime=TvecFunc(batch2)

# plot(celldenstime)


#Possible to merge these 2 functions depending on the result we want to obtain

#************************Fitted data : Gamma + model****************************
tvec = celldenstime$Tvec
gamAPred = celldenstime$celldens
gamAPar <- c(1,3,15)  #I put 1 but I didn't know what number to use
fitted<-optim(par=gamAPar,fn=gamADevSq,method="Nelder-Mead", tvec=tvec, obs=gamAPred)

k<-fitted$par[1]
theta<-fitted$par[2]
A<-fitted$par[3]
Pval<- A+tvec^(k-1)*exp(-tvec/theta)
PVAL<-data.frame(tvec,Pval)
cond<-c("Model data")
PVAL<-data.frame(tvec,Pval,cond)

#*************************************Plot**************************************

a<-rep(Cond1,length(celldens))
b<-rep(cond,length(Pval))

Tvec<-c(tvec,tvec)
cell.density<-c(celldens,Pval)
Condition<-c(a,b)
DataFinal<-data.frame(Tvec,cell.density,Condition)
DataFinal

plot<- ggplot(data=DataFinal,aes(x=Tvec,y=cell.density))+geom_point(aes(colour = factor(Condition)))+geom_line(aes(colour = factor(Condition)))+labs(title = "Cell density in wort according to fermentation time", x="Fermentation time (hours)",y="Cell density")
plot
