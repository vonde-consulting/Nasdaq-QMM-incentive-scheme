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

k<-4
w<-windows[k]

#unstandardized data
i<-1

mypath<-paste(head,"Dropbox/Projects/Trader Anonymity/Data/",sep="")

mydata<-read.table(file=paste(mypath,ticks[i],"_",w,"sec.rda",sep=""))

hasbrouck<-read.table(file=paste(mypath,"/Hasbrouck/Hasbrouck_",ticks[i],"_",w,"sec.rda",sep=""))
mydata$HASBROUCK.MAX<-hasbrouck$HASBROUCK.MAX
mydata$HASBROUCK.MEAN<-hasbrouck$HASBROUCK.MEAN

extra<-read.table(file=paste(mypath,"Data2/",ticks[i],"_",w,"sec2.rda",sep=""))

q<-colnames(extra)[which(colnames(extra)%in%colnames(mydata))]

for (kk in 1:length(q)){
  v1<-which(colnames(extra)==q[i])  
  v2<-which(colnames(mydata)==q[i])    
  if (sum(extra[,v1]-mydata[,v2],na.rm=T)!=0){stop("something is different")}  
}

TIME1<-paste(mydata$DAY,rownames(mydata),sep=".")
TIME2<-paste(extra$DAY,rownames(extra),sep=".")
  if (all.equal(TIME1,TIME2)!=TRUE){stop("unequal times")}

q<-which(!colnames(extra)%in%colnames(mydata))
mydata<-cbind(mydata,extra[,q])

mydata$TICK<-rep(ticks[i],nrow(mydata))
mydata$TIMEPOINT<-seq(1,nrow(mydata),1)
rm(hasbrouck,extra,test)

########other stocks

for (i in 2:length(ticks)){
  
  mydata2<-read.table(file=paste(mypath,ticks[i],"_",w,"sec.rda",sep=""))
  
  hasbrouck<-read.table(file=paste(mypath,"/Hasbrouck/Hasbrouck_",ticks[i],"_",w,"sec.rda",sep=""))
  mydata2$HASBROUCK.MAX<-hasbrouck$HASBROUCK.MAX
  mydata2$HASBROUCK.MEAN<-hasbrouck$HASBROUCK.MEAN
  
  extra<-read.table(file=paste(mypath,"Data2/",ticks[i],"_",w,"sec2.rda",sep=""))
  
  q<-colnames(extra)[which(colnames(extra)%in%colnames(mydata2))]
  
  for (kk in 1:length(q)){
    v1<-which(colnames(extra)==q[i])  
    v2<-which(colnames(mydata2)==q[i])    
    if (sum(extra[,v1]-mydata2[,v2],na.rm=T)!=0){stop("something is different")}  
  }
  
  TIME1<-paste(mydata2$DAY,rownames(mydata2),sep=".")
  TIME2<-paste(extra$DAY,rownames(extra),sep=".")
  if (all.equal(TIME1,TIME2)!=TRUE){stop("unequal times")}
  
  q<-which(!colnames(extra)%in%colnames(mydata2))
  mydata2<-cbind(mydata2,extra[,q])
  
  mydata2$TICK<-rep(ticks[i],nrow(mydata2))
  mydata2$TIMEPOINT<-seq(1,nrow(mydata2),1)
  rm(hasbrouck,extra,test)
  
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
        'MPID.DVOL.SUB','MPID.DVOL.SUB.BUY','MPID.DVOL.SUB.SELL',
        'MPID.CANC','MPID.CANC.BUY','MPID.CANC.SELL',
        'EXE.ALL.NUM','EXE.BUY.NUM','EXE.SELL.NUM',
        'MPID.EXE','MPID.EXE.BUY','MPID.EXE.SELL','CANC.ALL.NUM','CANC.BUY.NUM','CANC.SELL.NUM',
        'TMBR.SUB','TMBR.SUB.BUY','TMBR.SUB.SELL',
        'TMBR.VOL.SUB','TMBR.VOL.SUB.BUY','TMBR.VOL.SUB.SELL',
        'TMBR.DVOL.SUB','TMBR.DVOL.SUB.BUY','TMBR.DVOL.SUB.SELL',
        "AGGR.REL.TMBR","ORSZ.DVOL.TMBR",
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

