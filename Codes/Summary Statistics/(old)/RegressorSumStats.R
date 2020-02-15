rm(list=ls(all=TRUE))
library(car)
library(plm)
library(tseries)
library(xlsx)
library(lmtest)
library(sandwich)
library(moments)

#panel <- function(jj){

windows<-c(120,60,30,10)
ticks<-c("AAPL","CSCO","EBAY","FB","GOOG","INTC","MSFT","YHOO")
folder<-"Panel_Post_Ratio_NoInst"

#for (k in 1:length(windows)){

k<-3
w<-windows[k]

#unstandardized data
i<-1

mypath<-"C:/Users/user/Dropbox/Projects/Trader Anonymity/Data/"

mydata<-read.table(file=paste(mypath,ticks[i],"_",w,"sec.rda",sep=""))
hasbrouck<-read.table(file=paste(mypath,"/Hasbrouck/Hasbrouck_",ticks[i],"_",w,"sec.rda",sep=""))
mydata$HASBROUCK.MAX<-hasbrouck$HASBROUCK.MAX
mydata$HASBROUCK.MEAN<-hasbrouck$HASBROUCK.MEAN
mydata$TICK<-rep(ticks[i],nrow(mydata))
mydata$TIMEPOINT<-seq(1,nrow(mydata),1)
rm(hasbrouck)

mydata$MPID.SUB.RATIO<-mydata$MPID.SUB/mydata$SUB.ALL.NUM
mydata$MPID.SUB.BUY.RATIO<-mydata$MPID.SUB.BUY/mydata$SUB.BUY.NUM
mydata$MPID.SUB.SELL.RATIO<-mydata$MPID.SUB.SELL/mydata$SUB.SELL.NUM  

mydata$MPID.CANC.RATIO<-mydata$MPID.CANC/mydata$CANC.ALL.NUM
mydata$MPID.CANC.BUY.RATIO<-mydata$MPID.CANC.BUY/mydata$CANC.BUY.NUM
mydata$MPID.CANC.SELL.RATIO<-mydata$MPID.CANC.SELL/mydata$CANC.SELL.NUM  

mydata$MPID.EXE.RATIO<-mydata$MPID.EXE/mydata$EXE.ALL.NUM
mydata$MPID.EXE.BUY.RATIO<-mydata$MPID.EXE.BUY/mydata$EXE.BUY.NUM
mydata$MPID.EXE.SELL.RATIO<-mydata$MPID.EXE.SELL/mydata$EXE.SELL.NUM    

##need to standardize variables by the standard deviation

#sk<-which(colnames(mydata)=="DAY"|colnames(mydata)=="TICK"|colnames(mydata)=="TIMEPOINT")

#for (j in 1:(ncol(mydata))){
#  if (j%in%sk){next}
#  mydata[,j]<-mydata[,j]/sd(mydata[,j],na.rm=T)
#}

for (i in 2:length(ticks)){
  
  mydata2<-read.table(file=paste(mypath,ticks[i],"_",w,"sec.rda",sep=""))
  hasbrouck<-read.table(file=paste(mypath,"/Hasbrouck/Hasbrouck_",ticks[i],"_",w,"sec.rda",sep=""))
  mydata2$HASBROUCK.MAX<-hasbrouck$HASBROUCK.MAX
  mydata2$HASBROUCK.MEAN<-hasbrouck$HASBROUCK.MEAN
  mydata2$TICK<-rep(ticks[i],nrow(mydata2))   
  mydata2$TIMEPOINT<-seq(1,nrow(mydata2),1)
  
  mydata2$MPID.SUB.RATIO<-mydata2$MPID.SUB/mydata2$SUB.ALL.NUM
  mydata2$MPID.SUB.BUY.RATIO<-mydata2$MPID.SUB.BUY/mydata2$SUB.BUY.NUM
  mydata2$MPID.SUB.SELL.RATIO<-mydata2$MPID.SUB.SELL/mydata2$SUB.SELL.NUM  
  
  mydata2$MPID.CANC.RATIO<-mydata2$MPID.CANC/mydata2$CANC.ALL.NUM
  mydata2$MPID.CANC.BUY.RATIO<-mydata2$MPID.CANC.BUY/mydata2$CANC.BUY.NUM
  mydata2$MPID.CANC.SELL.RATIO<-mydata2$MPID.CANC.SELL/mydata2$CANC.SELL.NUM  
  
  mydata2$MPID.EXE.RATIO<-mydata2$MPID.EXE/mydata2$EXE.ALL.NUM
  mydata2$MPID.EXE.BUY.RATIO<-mydata2$MPID.EXE.BUY/mydata2$EXE.BUY.NUM
  mydata2$MPID.EXE.SELL.RATIO<-mydata2$MPID.EXE.SELL/mydata2$EXE.SELL.NUM    
  
  #for (j in 1:(ncol(mydata2))){
  #  if (j%in%sk){next}
  #  mydata2[,j]<-mydata2[,j]/sd(mydata2[,j],na.rm=T)
  #}
  
  mydata<-rbind(mydata,mydata2)
  
}

rm(mydata2)
mydata_unst<-mydata
rm(mydata)

##standardized data

i<-1

mypath<-"C:/Users/user/Dropbox/Projects/Trader Anonymity/Data/"

mydata<-read.table(file=paste(mypath,ticks[i],"_",w,"sec.rda",sep=""))
hasbrouck<-read.table(file=paste(mypath,"/Hasbrouck/Hasbrouck_",ticks[i],"_",w,"sec.rda",sep=""))
mydata$HASBROUCK.MAX<-hasbrouck$HASBROUCK.MAX
mydata$HASBROUCK.MEAN<-hasbrouck$HASBROUCK.MEAN
mydata$TICK<-rep(ticks[i],nrow(mydata))
mydata$TIMEPOINT<-seq(1,nrow(mydata),1)
rm(hasbrouck)

mydata$MPID.SUB.RATIO<-mydata$MPID.SUB/mydata$SUB.ALL.NUM
mydata$MPID.SUB.BUY.RATIO<-mydata$MPID.SUB.BUY/mydata$SUB.BUY.NUM
mydata$MPID.SUB.SELL.RATIO<-mydata$MPID.SUB.SELL/mydata$SUB.SELL.NUM  

mydata$MPID.CANC.RATIO<-mydata$MPID.CANC/mydata$CANC.ALL.NUM
mydata$MPID.CANC.BUY.RATIO<-mydata$MPID.CANC.BUY/mydata$CANC.BUY.NUM
mydata$MPID.CANC.SELL.RATIO<-mydata$MPID.CANC.SELL/mydata$CANC.SELL.NUM  

mydata$MPID.EXE.RATIO<-mydata$MPID.EXE/mydata$EXE.ALL.NUM
mydata$MPID.EXE.BUY.RATIO<-mydata$MPID.EXE.BUY/mydata$EXE.BUY.NUM
mydata$MPID.EXE.SELL.RATIO<-mydata$MPID.EXE.SELL/mydata$EXE.SELL.NUM    

##need to standardize variables by the standard deviation

sk<-which(colnames(mydata)=="DAY"|colnames(mydata)=="TICK"|colnames(mydata)=="TIMEPOINT")

for (j in 1:(ncol(mydata))){
  if (j%in%sk){next}
  mydata[,j]<-mydata[,j]/sd(mydata[,j],na.rm=T)
}

for (i in 2:length(ticks)){
  
  mydata2<-read.table(file=paste(mypath,ticks[i],"_",w,"sec.rda",sep=""))
  hasbrouck<-read.table(file=paste(mypath,"/Hasbrouck/Hasbrouck_",ticks[i],"_",w,"sec.rda",sep=""))
  mydata2$HASBROUCK.MAX<-hasbrouck$HASBROUCK.MAX
  mydata2$HASBROUCK.MEAN<-hasbrouck$HASBROUCK.MEAN
  mydata2$TICK<-rep(ticks[i],nrow(mydata2))   
  mydata2$TIMEPOINT<-seq(1,nrow(mydata2),1)
  
  mydata2$MPID.SUB.RATIO<-mydata2$MPID.SUB/mydata2$SUB.ALL.NUM
  mydata2$MPID.SUB.BUY.RATIO<-mydata2$MPID.SUB.BUY/mydata2$SUB.BUY.NUM
  mydata2$MPID.SUB.SELL.RATIO<-mydata2$MPID.SUB.SELL/mydata2$SUB.SELL.NUM  
  
  mydata2$MPID.CANC.RATIO<-mydata2$MPID.CANC/mydata2$CANC.ALL.NUM
  mydata2$MPID.CANC.BUY.RATIO<-mydata2$MPID.CANC.BUY/mydata2$CANC.BUY.NUM
  mydata2$MPID.CANC.SELL.RATIO<-mydata2$MPID.CANC.SELL/mydata2$CANC.SELL.NUM  
  
  mydata2$MPID.EXE.RATIO<-mydata2$MPID.EXE/mydata2$EXE.ALL.NUM
  mydata2$MPID.EXE.BUY.RATIO<-mydata2$MPID.EXE.BUY/mydata2$EXE.BUY.NUM
  mydata2$MPID.EXE.SELL.RATIO<-mydata2$MPID.EXE.SELL/mydata2$EXE.SELL.NUM    
  
  for (j in 1:(ncol(mydata2))){
    if (j%in%sk){next}
    mydata2[,j]<-mydata2[,j]/sd(mydata2[,j],na.rm=T)
  }
  
  mydata<-rbind(mydata,mydata2)
  
}

rm(mydata2)  

vars<-c('SUB.ALL.NUM','SUB.BUY.NUM','SUB.SELL.NUM',"RELSPR","VOL.GRID.SHORT.PRE","AGGR.REL.MPID",
        "ORSZ.DVOL.MPID","SUB.ALL.DVOL","EXE.ALL.DVOL","SUB.BUY.DVOL","EXE.BUY.DVOL","SUB.SELL.DVOL","EXE.SELL.DVOL",
        "DEPTH.TOTAL.DVOL","DEPTH.BUY.DVOL","DEPTH.SELL.DVOL","RELDPR.MID",'HASBROUCK.MAX',"AGGR.REL.ANON",
        "ORSZ.DVOL.ANON")

d<-mydata_unst

stats<-matrix(NA,length(vars),6)

for (j in 1:length(vars)){
  
q<-which(colnames(d)==vars[j])   
dd<-d[,q]  
row<-c(mean(dd,na.rm=T),median(dd,na.rm=T),sd(dd,na.rm=T),min(dd,na.rm=T),max(dd,na.rm=T),skewness(dd,na.rm=T))
stats[j,]<-row  
  
}

colnames(stats)<-c('MEAN','MEDIAN','STDEV','MIN','MAX','SKEWNESS')
rownames(stats)<-vars

filename<-paste(mypath,"/REGRESSOR_SUMSTATS_",w,"sec.xlsx",sep="")
write.xlsx(stats,file=filename,append=FALSE) 








