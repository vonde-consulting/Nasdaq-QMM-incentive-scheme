rm(list=ls(all=TRUE))
library(car)
library(plm)
library(tseries)

library(lmtest)
library(sandwich)
library(lfe)
library(foreign)


#head<-"C:/DatiLocali/"
head<-"C:/Users/Julia/"
windows<-c(120,60,30,10)
ticks<-c("AAPL","CSCO","EBAY","FB","GOOG","INTC","MSFT","YHOO")

#for (k in 1:length(windows)){

k<-3
w<-windows[k]

#unstandardized data
i<-1

mypath<-paste(head,"Dropbox/Projects/Trader Anonymity/Data/",sep="")

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
mydata[is.na(mydata)]<-0

##make sure tick and time are first
q1<-which(colnames(mydata)=="TICK")
q2<-which(colnames(mydata)=="TIMEPOINT")

mydatatemp<-mydata[,c(q1,q2,1:ncol(mydata))] 
q1<-which(colnames(mydatatemp)=="TICK.1")
q2<-which(colnames(mydatatemp)=="TIMEPOINT.1")
mydatatemp<-mydatatemp[,-c(q1,q2)] 

mydata<-mydatatemp
rm(mydatatemp)

vars<-c('TICK','TIMEPOINT','DAY','MPID.SUB','MPID.SUB.BUY','MPID.SUB.SELL','SUB.ALL.NUM','SUB.BUY.NUM','SUB.SELL.NUM',
        'MPID.VOL.SUB','MPID.VOL.SUB.BUY','MPID.VOL.SUB.SELL',
        'MPID.CANC','MPID.CANC.BUY','MPID.CANC.SELL','EXE.ALL.NUM','EXE.BUY.NUM','EXE.SELL.NUM',
        'MPID.EXE','MPID.EXE.BUY','MPID.EXE.SELL','CANC.ALL.NUM','CANC.BUY.NUM','CANC.SELL.NUM',
        "RELSPR","RELEFFSPR.EXE.REP","RELRLZSPR.EXE.REP.PRE",
        "RELRLZSPR.EXE.REP.POST",
        "VOL.GRID.SHORT.PRE","VOL.SD","VOL.GRID.LONG.PRE","VOL.GRID.SHORT.POST","VOL.GRID.LONG.POST",
        "AGGR.REL.TOTAL","ORSZ.DVOL.TOTAL","AGGR.REL.MPID",
        "ORSZ.DVOL.MPID","SUB.ALL.DVOL","EXE.ALL.DVOL","SUB.BUY.DVOL","EXE.BUY.DVOL","SUB.SELL.DVOL","EXE.SELL.DVOL",
        "CANC.ALL.DVOL","CANC.BUY.DVOL","CANC.SELL.DVOL",
        "DEPTH.TOTAL.DVOL","DEPTH.BUY.DVOL","DEPTH.SELL.DVOL","RELDPR.MID","NEG.DUMMY",'OPEN','HASBROUCK.MAX')
mydata<-mydata[,vars]

filename<-paste(mypath,"DATA_LEVELS_UNST_",w,"sec.dta",sep="")
write.dta(mydata,file=filename) 

