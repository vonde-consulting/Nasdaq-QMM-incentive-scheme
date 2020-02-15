rm(list=ls(all=TRUE))
library(car)
library(plm)
library(tseries)
library(xlsx)
library(lmtest)
library(sandwich)
library(quantmod)
library(AER)
library(plyr)

#panel <- function(jj){

windows<-c(120,60,30,10)
ticks<-c("AAPL","CSCO","EBAY","FB","GOOG","INTC","MSFT","YHOO")
folder<-"Individual_Post_Ratio_TMBR_NoInst"

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

mydata$MPID.SUB.RATIO<-mydata$TMBR.SUB/mydata$SUB.ALL.NUM
mydata$MPID.SUB.BUY.RATIO<-mydata$TMBR.SUB.BUY/mydata$SUB.BUY.NUM
mydata$MPID.SUB.SELL.RATIO<-mydata$TMBR.SUB.SELL/mydata$SUB.SELL.NUM  

mydata$MPID.CANC.RATIO<-mydata$TMBR.CANC/mydata$CANC.ALL.NUM
mydata$MPID.CANC.BUY.RATIO<-mydata$TMBR.CANC.BUY/mydata$CANC.BUY.NUM
mydata$MPID.CANC.SELL.RATIO<-mydata$TMBR.CANC.SELL/mydata$CANC.SELL.NUM  

mydata$MPID.EXE.RATIO<-mydata$TMBR.EXE/mydata$EXE.ALL.NUM
mydata$MPID.EXE.BUY.RATIO<-mydata$TMBR.EXE.BUY/mydata$EXE.BUY.NUM
mydata$MPID.EXE.SELL.RATIO<-mydata$TMBR.EXE.SELL/mydata$EXE.SELL.NUM     

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
  
  mydata2$MPID.SUB.RATIO<-mydata2$TMBR.SUB/mydata2$SUB.ALL.NUM
  mydata2$MPID.SUB.BUY.RATIO<-mydata2$TMBR.SUB.BUY/mydata2$SUB.BUY.NUM
  mydata2$MPID.SUB.SELL.RATIO<-mydata2$TMBR.SUB.SELL/mydata2$SUB.SELL.NUM  
  
  mydata2$MPID.CANC.RATIO<-mydata2$TMBR.CANC/mydata2$CANC.ALL.NUM
  mydata2$MPID.CANC.BUY.RATIO<-mydata2$TMBR.CANC.BUY/mydata2$CANC.BUY.NUM
  mydata2$MPID.CANC.SELL.RATIO<-mydata2$TMBR.CANC.SELL/mydata2$CANC.SELL.NUM  
  
  mydata2$MPID.EXE.RATIO<-mydata2$TMBR.EXE/mydata2$EXE.ALL.NUM
  mydata2$MPID.EXE.BUY.RATIO<-mydata2$TMBR.EXE.BUY/mydata2$EXE.BUY.NUM
  mydata2$MPID.EXE.SELL.RATIO<-mydata2$TMBR.EXE.SELL/mydata2$EXE.SELL.NUM  
  
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

mydata$MPID.SUB.RATIO<-mydata$TMBR.SUB/mydata$SUB.ALL.NUM
mydata$MPID.SUB.BUY.RATIO<-mydata$TMBR.SUB.BUY/mydata$SUB.BUY.NUM
mydata$MPID.SUB.SELL.RATIO<-mydata$TMBR.SUB.SELL/mydata$SUB.SELL.NUM  

mydata$MPID.CANC.RATIO<-mydata$TMBR.CANC/mydata$CANC.ALL.NUM
mydata$MPID.CANC.BUY.RATIO<-mydata$TMBR.CANC.BUY/mydata$CANC.BUY.NUM
mydata$MPID.CANC.SELL.RATIO<-mydata$TMBR.CANC.SELL/mydata$CANC.SELL.NUM  

mydata$MPID.EXE.RATIO<-mydata$TMBR.EXE/mydata$EXE.ALL.NUM
mydata$MPID.EXE.BUY.RATIO<-mydata$TMBR.EXE.BUY/mydata$EXE.BUY.NUM
mydata$MPID.EXE.SELL.RATIO<-mydata$TMBR.EXE.SELL/mydata$EXE.SELL.NUM     

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
  
  mydata2$MPID.SUB.RATIO<-mydata2$TMBR.SUB/mydata2$SUB.ALL.NUM
  mydata2$MPID.SUB.BUY.RATIO<-mydata2$TMBR.SUB.BUY/mydata2$SUB.BUY.NUM
  mydata2$MPID.SUB.SELL.RATIO<-mydata2$TMBR.SUB.SELL/mydata2$SUB.SELL.NUM  
  
  mydata2$MPID.CANC.RATIO<-mydata2$TMBR.CANC/mydata2$CANC.ALL.NUM
  mydata2$MPID.CANC.BUY.RATIO<-mydata2$TMBR.CANC.BUY/mydata2$CANC.BUY.NUM
  mydata2$MPID.CANC.SELL.RATIO<-mydata2$TMBR.CANC.SELL/mydata2$CANC.SELL.NUM  
  
  mydata2$MPID.EXE.RATIO<-mydata2$TMBR.EXE/mydata2$EXE.ALL.NUM
  mydata2$MPID.EXE.BUY.RATIO<-mydata2$TMBR.EXE.BUY/mydata2$EXE.BUY.NUM
  mydata2$MPID.EXE.SELL.RATIO<-mydata2$TMBR.EXE.SELL/mydata2$EXE.SELL.NUM    
  
  for (j in 1:(ncol(mydata2))){
    if (j%in%sk){next}
    mydata2[,j]<-mydata2[,j]/sd(mydata2[,j],na.rm=T)
  }
  
  mydata<-rbind(mydata,mydata2)
  
}

rm(mydata2)    
#y<-mydata$MPID.SUB
#timepoint<-mydata$TIMEPOINT
#tick<-mydata$TICK

#scatterplot(y~timepoint|tick, boxplots=FALSE, smooth=TRUE, reg.line=FALSE)

##OPEN DUMMY

intervals<-seq(9.5*60*60,16*60*60,w); temp<-rep(intervals[1:(length(intervals)-1)],14); temp<-rep(temp,8)
if (length(temp)!=nrow(mydata)){stop("Incorrect number of intervals")}

temp2<-as.numeric(substr(rownames(mydata),1,5)); q<-sum(temp2-temp)
if (q!=0){stop("Incorrect number of intervals 2")}
time<-temp; rm(temp,temp2,q)

#temp<-as.numeric(time<=9.75*60*60); mydata$OPEN<-temp
temp<-as.numeric(time<=10*60*60); mydata$OPEN<-temp
temp<-as.numeric(time>=15.75*60*60); mydata$CLOSE<-temp

##PRICE CHANGES

temp<-mydata$RELDPR.MID
dummy_neg<-rep(1,length(temp)); dummy_neg<-replace(dummy_neg,temp>=0,0); 
dummy_pos<-rep(1,length(temp)); dummy_pos<-replace(dummy_pos,temp<=0,0); 

mydata$POS.DUMMY<-dummy_pos
mydata$NEG.DUMMY<-dummy_neg

mydata$TICK<-replace(mydata$TICK,mydata$TICK=="AAPL",1)
mydata$TICK<-replace(mydata$TICK,mydata$TICK=="CSCO",2)
mydata$TICK<-replace(mydata$TICK,mydata$TICK=="EBAY",3)
mydata$TICK<-replace(mydata$TICK,mydata$TICK=="FB",4)
mydata$TICK<-replace(mydata$TICK,mydata$TICK=="GOOG",5)
mydata$TICK<-replace(mydata$TICK,mydata$TICK=="INTC",6)
mydata$TICK<-replace(mydata$TICK,mydata$TICK=="MSFT",7)
mydata$TICK<-replace(mydata$TICK,mydata$TICK=="YHOO",8)
mydata$TICK<-as.numeric(mydata$TICK)

mydata[is.na(mydata)]<-0
mydata_unst[is.na(mydata_unst)]<-0

##make sure tick and time are first
q1<-which(colnames(mydata)=="TICK")
q2<-which(colnames(mydata)=="TIMEPOINT")

mydatatemp<-mydata[,c(q1,q2,1:ncol(mydata))] 
q1<-which(colnames(mydatatemp)=="TICK.1")
q2<-which(colnames(mydatatemp)=="TIMEPOINT.1")
mydatatemp<-mydatatemp[,-c(q1,q2)] 

mydata<-mydatatemp
rm(mydatatemp)

vars<-c('TICK','TIMEPOINT','DAY','MPID.SUB.RATIO','MPID.SUB.BUY.RATIO','MPID.SUB.SELL.RATIO','MPID.SUB.INST',
        'MPID.SUB.BUY.INST','MPID.SUB.SELL.INST','SUB.ALL.NUM','SUB.BUY.NUM','SUB.SELL.NUM',
        'SUB.ALL.NUM.INST','SUB.BUY.NUM.INST','SUB.SELL.NUM.INST',"RELSPR","RELEFFSPR.EXE.REP","RELRLZSPR.EXE.REP.PRE",
        "RELRLZSPR.EXE.REP.POST",
        "VOL.GRID.SHORT.PRE","VOL.SD","VOL.GRID.LONG.PRE","VOL.GRID.SHORT.POST","VOL.GRID.LONG.POST",
        "AGGR.REL.TOTAL","ORSZ.DVOL.TOTAL","AGGR.REL.TMBR",
        "ORSZ.DVOL.TMBR","SUB.ALL.DVOL","EXE.ALL.DVOL","SUB.BUY.DVOL","EXE.BUY.DVOL","SUB.SELL.DVOL","EXE.SELL.DVOL",
        "DEPTH.TOTAL.DVOL","DEPTH.BUY.DVOL","DEPTH.SELL.DVOL","RELDPR.MID","NEG.DUMMY",'OPEN')
mydata<-mydata[,vars]

mydata<-rename(mydata, c("AGGR.REL.TMBR"="AGGR.REL.MPID", "ORSZ.DVOL.TMBR"="ORSZ.DVOL.MPID"))

mydata<-pdata.frame(mydata,index=c("TICK","TIMEPOINT"))

##RUN THROUGH DIFFERENT MODELS###################################

#  for (l in 1:20){

numtests<-28

day.f<-factor(mydata$DAY)
DUMMIES<-model.matrix(~day.f)

tests<-matrix(NA,numtests,12)
numtest<-6
stationary<-matrix(NA,numtests,numtest*2*length(ticks))
mpidlags<-1
depvarlags<-5

for (jj in 1:numtests){
  
  if (jj==1){     
    model<-RELSPR~lag(MPID.SUB.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1:depvarlags)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.ALL.NUM,k=1:mpidlags)
    model2<-RELSPR~lag(MPID.SUB.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1:depvarlags)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.ALL.NUM,k=1:mpidlags)+DUMMIES
  }
  if (jj==2){    
    model<-RELSPR~lag(MPID.SUB.BUY.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1:depvarlags)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.BUY.NUM,k=1:mpidlags)
    model2<-RELSPR~lag(MPID.SUB.BUY.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1:depvarlags)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.BUY.NUM,k=1:mpidlags)+DUMMIES
  }    
  
  if (jj==3){    
    model<-RELSPR~lag(MPID.SUB.SELL.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1:depvarlags)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.SELL.NUM,k=1:mpidlags)
    model2<-RELSPR~lag(MPID.SUB.SELL.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1:depvarlags)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.SELL.NUM,k=1:mpidlags)+DUMMIES
    
  }    
  if (jj==4){     
    model<-RELEFFSPR.EXE.REP~lag(MPID.SUB.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELEFFSPR.EXE.REP,k=1:depvarlags)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.ALL.NUM,k=1:mpidlags)
    model2<-RELEFFSPR.EXE.REP~lag(MPID.SUB.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELEFFSPR.EXE.REP,k=1:depvarlags)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.ALL.NUM,k=1:mpidlags)+DUMMIES
    
  }
  if (jj==5){     
    model<-RELRLZSPR.EXE.REP.POST~lag(MPID.SUB.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELRLZSPR.EXE.REP.PRE,k=1:depvarlags)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.ALL.NUM,k=1:mpidlags) 
    model2<-RELRLZSPR.EXE.REP.POST~lag(MPID.SUB.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELRLZSPR.EXE.REP.PRE,k=1:depvarlags)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.ALL.NUM,k=1:mpidlags)+DUMMIES
    
  }
  ##VOLATILITY
  if (jj==6){     
    model<-VOL.GRID.SHORT.POST~lag(MPID.SUB.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1:depvarlags)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.ALL.NUM,k=1:mpidlags)
    model2<-VOL.GRID.SHORT.POST~lag(MPID.SUB.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1:depvarlags)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.ALL.NUM,k=1:mpidlags)+DUMMIES
    
  }
  if (jj==7){    
    model<-VOL.GRID.SHORT.POST~lag(MPID.SUB.BUY.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1:depvarlags)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.BUY.NUM,k=1:mpidlags)
    model2<-VOL.GRID.SHORT.POST~lag(MPID.SUB.BUY.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1:depvarlags)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.BUY.NUM,k=1:mpidlags)+DUMMIES
    
  }    
  if (jj==8){    
    model<-VOL.GRID.SHORT.POST~lag(MPID.SUB.SELL.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1:depvarlags)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.SELL.NUM,k=1:mpidlags)
    model2<-VOL.GRID.SHORT.POST~lag(MPID.SUB.SELL.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1:depvarlags)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.SELL.NUM,k=1:mpidlags)+DUMMIES
  }    
  
  if (jj==9){     
    model<-VOL.SD~lag(MPID.SUB.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.SD,k=1:depvarlags)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.ALL.NUM,k=1:mpidlags)
    model2<-VOL.SD~lag(MPID.SUB.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.SD,k=1:depvarlags)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.ALL.NUM,k=1:mpidlags)+DUMMIES
  }
  
  if (jj==10){     
    model<-VOL.GRID.LONG.POST~lag(MPID.SUB.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.LONG.PRE,k=1:depvarlags)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.ALL.NUM,k=1:mpidlags)
    model2<-VOL.GRID.LONG.POST~lag(MPID.SUB.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.LONG.PRE,k=1:depvarlags)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.ALL.NUM,k=1:mpidlags)+DUMMIES
  }
  ##SUBMISSION VOLUME
  if (jj==11){     
    model<-SUB.ALL.DVOL~lag(MPID.SUB.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1:depvarlags)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.ALL.NUM,k=1:mpidlags)
    model2<-SUB.ALL.DVOL~lag(MPID.SUB.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1:depvarlags)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.ALL.NUM,k=1:mpidlags)+DUMMIES
    
  }
  if (jj==12){    
    model<-SUB.BUY.DVOL~lag(MPID.SUB.BUY.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1:depvarlags)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.BUY.NUM,k=1:mpidlags)
    model2<-SUB.BUY.DVOL~lag(MPID.SUB.BUY.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1:depvarlags)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.BUY.NUM,k=1:mpidlags)+DUMMIES
    
  }    
  if (jj==13){    
    model<-SUB.SELL.DVOL~lag(MPID.SUB.SELL.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1:depvarlags)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.SELL.NUM,k=1:mpidlags)
    model2<-SUB.SELL.DVOL~lag(MPID.SUB.SELL.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1:depvarlags)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.SELL.NUM,k=1:mpidlags)+DUMMIES
  }    
  if (jj==14){    
    model<-SUB.BUY.DVOL~lag(MPID.SUB.SELL.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1:depvarlags)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.BUY.NUM,k=1:mpidlags)
    model2<-SUB.BUY.DVOL~lag(MPID.SUB.SELL.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1:depvarlags)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.BUY.NUM,k=1:mpidlags)+DUMMIES
    
  }    
  if (jj==15){    
    model<-SUB.SELL.DVOL~lag(MPID.SUB.BUY.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1:depvarlags)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.SELL.NUM,k=1:mpidlags)
    model2<-SUB.SELL.DVOL~lag(MPID.SUB.BUY.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1:depvarlags)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.SELL.NUM,k=1:mpidlags)+DUMMIES
    
  } 
  ##EXECUTION VOLUME
  if (jj==16){     
    model<-EXE.ALL.DVOL~lag(MPID.SUB.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1:depvarlags)+
      lag(DEPTH.TOTAL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.ALL.NUM,k=1:mpidlags)
    model2<-EXE.ALL.DVOL~lag(MPID.SUB.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1:depvarlags)+
      lag(DEPTH.TOTAL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.ALL.NUM,k=1:mpidlags)+DUMMIES
    
  }
  if (jj==17){    
    model<-EXE.BUY.DVOL~lag(MPID.SUB.BUY.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1:depvarlags)+
      lag(DEPTH.BUY.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.BUY.NUM,k=1:mpidlags)
    model2<-EXE.BUY.DVOL~lag(MPID.SUB.BUY.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1:depvarlags)+
      lag(DEPTH.BUY.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.BUY.NUM,k=1:mpidlags)+DUMMIES
    
  }    
  if (jj==18){    
    model<-EXE.SELL.DVOL~lag(MPID.SUB.SELL.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1:depvarlags)+
      lag(DEPTH.SELL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.SELL.NUM,k=1:mpidlags)
    model2<-EXE.SELL.DVOL~lag(MPID.SUB.SELL.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1:depvarlags)+
      lag(DEPTH.SELL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.SELL.NUM,k=1:mpidlags)+DUMMIES
    
  }  
  if (jj==19){    
    model<-EXE.BUY.DVOL~lag(MPID.SUB.SELL.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1:depvarlags)+
      lag(DEPTH.BUY.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.BUY.NUM,k=1:mpidlags)
    model2<-EXE.BUY.DVOL~lag(MPID.SUB.SELL.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1:depvarlags)+
      lag(DEPTH.BUY.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.BUY.NUM,k=1:mpidlags)+DUMMIES
    
  }    
  if (jj==20){    
    model<-EXE.SELL.DVOL~lag(MPID.SUB.BUY.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1:depvarlags)+
      lag(DEPTH.SELL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.SELL.NUM,k=1:mpidlags)
    model2<-EXE.SELL.DVOL~lag(MPID.SUB.BUY.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1:depvarlags)+
      lag(DEPTH.SELL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.SELL.NUM,k=1:mpidlags)+DUMMIES
  }       
  ##DEPTH
  if (jj==21){     
    model<-DEPTH.TOTAL.DVOL~lag(MPID.SUB.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1:depvarlags)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.ALL.NUM,k=1:mpidlags)
    model2<-DEPTH.TOTAL.DVOL~lag(MPID.SUB.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1:depvarlags)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.ALL.NUM,k=1:mpidlags)+DUMMIES
    
  }
  if (jj==22){    
    model<-DEPTH.BUY.DVOL~lag(MPID.SUB.BUY.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1:depvarlags)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.BUY.NUM,k=1:mpidlags)
    model2<-DEPTH.BUY.DVOL~lag(MPID.SUB.BUY.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1:depvarlags)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.BUY.NUM,k=1:mpidlags)+DUMMIES
    
  }    
  if (jj==23){    
    model<-DEPTH.SELL.DVOL~lag(MPID.SUB.SELL.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1:depvarlags)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.SELL.NUM,k=1:mpidlags)
    model2<-DEPTH.SELL.DVOL~lag(MPID.SUB.SELL.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1:depvarlags)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.SELL.NUM,k=1:mpidlags)+DUMMIES
    
  }  
  if (jj==24){    
    model<-DEPTH.BUY.DVOL~lag(MPID.SUB.SELL.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1:depvarlags)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.BUY.NUM,k=1:mpidlags)
    model2<-DEPTH.BUY.DVOL~lag(MPID.SUB.SELL.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1:depvarlags)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.BUY.NUM,k=1:mpidlags)+DUMMIES
    
  }    
  if (jj==25){    
    model<-DEPTH.SELL.DVOL~lag(MPID.SUB.BUY.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1:depvarlags)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.SELL.NUM,k=1:mpidlags)
    model2<-DEPTH.SELL.DVOL~lag(MPID.SUB.BUY.RATIO,k=c(1:mpidlags))+lag(HASBROUCK.MAX,k=1)+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1:depvarlags)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.SELL.NUM,k=1:mpidlags)+DUMMIES
  }   
  ##HASBROUCK
  if (jj==26){     
    model<-HASBROUCK.MAX~lag(MPID.SUB.RATIO,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.ALL.NUM,k=1:mpidlags)+lag(HASBROUCK.MAX,k=1:depvarlags)
    model2<-HASBROUCK.MAX~lag(MPID.SUB.RATIO,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.ALL.NUM,k=1:mpidlags)+DUMMIES+lag(HASBROUCK.MAX,k=1:depvarlags)
    
  }
  if (jj==27){    
    model<-HASBROUCK.MAX~lag(MPID.SUB.BUY.RATIO,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1:depvarlags)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.BUY.NUM,k=1:mpidlags)+lag(HASBROUCK.MAX,k=1:depvarlags)
    model2<-HASBROUCK.MAX~lag(MPID.SUB.BUY.RATIO,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1:depvarlags)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.BUY.NUM,k=1:mpidlags)+DUMMIES+lag(HASBROUCK.MAX,k=1:depvarlags)
    
  }    
  if (jj==28){    
    model<-HASBROUCK.MAX~lag(MPID.SUB.SELL.RATIO,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1:depvarlags)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.SELL.NUM,k=1:mpidlags)+lag(HASBROUCK.MAX,k=1:depvarlags)
    model2<-HASBROUCK.MAX~lag(MPID.SUB.SELL.RATIO,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1:depvarlags)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.SELL.NUM,k=1:mpidlags)+DUMMIES+lag(HASBROUCK.MAX,k=1:depvarlags)
    
  }  
  
  
  
  filename<-paste(mypath,"Results/", folder, "/Panel_Regression_Post_",w,"sec_",jj,".txt",sep="")
  
  
  ##pooled OLS regression
  cat(paste("\n \n","Pooled OLS Regression #", jj, ", Dep Var: ",all.vars(model)[1],"\n \n",sep=""),file=filename,append=FALSE)
  fit1<-plm(model,data=mydata,model="pooling",index=c("TICK","TIMEPOINT"))
  s<-summary(fit1)
  capture.output(s, file=filename,append=T)
  #clustered errors
  #s<-coeftest(fit1, vcov=vcovHC(fit1,type="HC0",cluster="group"))
  #capture.output(s, file=filename,append=T)
  dw1<-pdwtest(fit1,alternative="two.sided")
  capture.output(dw1, file=filename,append=T)
  
  ##firm fixed effects
  cat(paste("\n \n","Firm Fixed Regression #", jj, ", Dep Var: ",all.vars(model)[1],"\n \n",sep=""),file=filename,append=TRUE)
  fit2<-plm(model,data=mydata,model="within",index=c("TICK","TIMEPOINT"))
  s<-summary(fit2)
  capture.output(s, file=filename,append=T)
  #clustered errors
  #s<-coeftest(fit2, vcov=vcovHC(fit2,type="HC0",cluster="group"))
  #capture.output(s, file=filename,append=T)
  dw1<-pdwtest(fit2,alternative="two.sided")
  capture.output(dw1, file=filename,append=T)
  
  ##firm+day fixed effects
  #model2<-update(model, . ~ . + DUMMIES)
  cat(paste("\n \n","Firm+Day Fixed Regression #", jj, ", Dep Var: ",all.vars(model)[1],"\n \n",sep=""),file=filename,append=TRUE)
  fit3<-plm(model2,data=mydata,model="within",index=c("TICK","TIMEPOINT"))
  s<-summary(fit3)
  capture.output(s,file=filename,append=T)    
  #clustered errors
  #s<-coeftest(fit3, vcov=vcovHC(fit3,type="HC0",cluster="group"))
  #capture.output(s, file=filename,append=T)
  dw1<-pdwtest(fit3,alternative="two.sided")
  capture.output(dw1, file=filename,append=T)
  
  filename<-paste(mypath,"Results/", folder, "/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
  save(fit1,fit2,fit3,file=filename)
  
  ##test errors for stationarity 
  
  u<-residuals(fit3)
  uindex<-attr(u,"index")
  
  for (vv in 1:length(ticks)){
    
    q<-which(uindex$TICK==vv)
    v<-u[q]
    
    #pdf(paste(mypath,"/Graphs/PanelErrors_Post/",ticks[vv],"_PanelErrors_Ratio_",w,"sec.pdf",sep=""))
    #plot(v,type = "l",ylab="Panel Errors",main=ticks[vv],xlab="Date")
    #dev.off()
    
    qv<-which(!is.na(v))
    v<-v[qv]
    
    test1<-adf.test(v)
    test2<-PP.test(v,lshort=FALSE)
    test3<-kpss.test(v,null=c("Level"))
    test4<-kpss.test(v,null=c("Trend"))
    
    #mydatatemp<-mydata[q,]
    test5<-Box.test(v, lag=(2*mpidlags), type = c("Ljung-Box"),fitdf=(mpidlags))
    test6<-summary(lm(v[-length(v)]~v[-1]))
    
    stationary[jj,(1+(numtest*2)*(vv-1)):((numtest*2)+(numtest*2)*(vv-1))]<-c(test1$statistic,test1$p.value,test2$statistic,test2$p.value,
                                                                              test3$statistic,test3$p.value,test4$statistic,test4$p.value,
                                                                              test5$statistic,test5$p.value,
                                                                              test6$coefficients[2,3],test6$coefficients[2,4])
  }
  
  
}

colnames(stationary)<-c('AAPL',vector(mode="numeric",length=(2*numtest-1)),'CSCO',vector(mode="numeric",length=(2*numtest-1)),
                        'EBAY',vector(mode="numeric",length=(2*numtest-1)),'FB',vector(mode="numeric",length=(2*numtest-1)),
                        'GOOG',vector(mode="numeric",length=(2*numtest-1)),
                        'INTC',vector(mode="numeric",length=(2*numtest-1)),'MSFT',vector(mode="numeric",length=(2*numtest-1)),
                        'YHOO',vector(mode="numeric",length=(2*numtest-1)))

filename<-paste(mypath,"Results/", folder, "/POST_PANELERRORS_STATIONARITY_BYFIRM_",w,"sec.xlsx",sep="")
write.xlsx(stationary,file=filename,append=FALSE) 


#}

