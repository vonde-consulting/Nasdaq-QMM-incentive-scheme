rm(list=ls(all=TRUE))
library(car)
library(plm)
library(tseries)
library(xlsx)
library(lmtest)
library(sandwich)
library(lfe)
library(plyr)

#options(error=NULL)
#options(error=recover)

#panel <- function(jj){

windows<-c(120,60,30,10)
ticks<-c("AAPL","CSCO","EBAY","FB","GOOG","INTC","MSFT","YHOO")
folder<-"Panel_Post_Ratio_TMBR_2SLS"

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

###remove AAPL

q<-which(mydata$TICK==1)
mydata<-mydata[-q,]
ticks<-ticks[-1]
mydata$TICK<-(mydata$TICK-1)

###calculate instruments

qq<-which(colnames(mydata)%in%c('MPID.SUB.RATIO','MPID.SUB.BUY.RATIO','MPID.SUB.SELL.RATIO',
                                'MPID.CANC.RATIO','MPID.CANC.BUY.RATIO','MPID.CANC.SELL.RATIO',
                                'MPID.EXE.RATIO','MPID.EXE.BUY.RATIO','MPID.EXE.SELL.RATIO'))
qq2<-which(colnames(mydata)%in%c('SUB.ALL.NUM','SUB.BUY.NUM','SUB.SELL.NUM',
                                 'CANC.ALL.NUM','CANC.BUY.NUM','CANC.SELL.NUM',
                                 'EXE.ALL.NUM','EXE.BUY.NUM','EXE.SELL.NUM'))

qq<-c(qq,qq2)

for (jjj in 1:length(qq)){
  
  j<-qq[jjj]  
  
  i<-1
  
  tick<-ticks[i]
  mydatatemp<-mydata[mydata$TICK!=i,]
  MPID<-mydatatemp[,j]
  temp<-rowMeans(matrix(MPID,nrow=length(unique(mydatatemp$TIMEPOINT))),na.rm=T)
  INST<-temp
  rm(temp)
  
  for (i in 2:length(ticks)){
    
    #i<-2
    
    tick<-ticks[i]
    mydatatemp<-mydata[mydata$TICK!=i,]
    MPID<-mydatatemp[,j]
    temp<-rowMeans(matrix(MPID,nrow=length(unique(mydatatemp$TIMEPOINT))),na.rm=T)
    INST<-c(INST,temp)
    rm(temp,mydatatemp)
    
  }
  
  if (jjj==1){mydata$MPID.SUB.INST<-INST}
  if (jjj==2){mydata$MPID.SUB.BUY.INST<-INST}
  if (jjj==3){mydata$MPID.SUB.SELL.INST<-INST}
  if (jjj==4){mydata$MPID.CANC.INST<-INST}
  if (jjj==5){mydata$MPID.CANC.BUY.INST<-INST}
  if (jjj==6){mydata$MPID.CANC.SELL.INST<-INST}
  if (jjj==7){mydata$MPID.EXE.INST<-INST}
  if (jjj==8){mydata$MPID.EXE.BUY.INST<-INST}
  if (jjj==9){mydata$MPID.EXE.SELL.INST<-INST}
  if (jjj==10){mydata$SUB.ALL.NUM.INST<-INST}
  if (jjj==11){mydata$SUB.BUY.NUM.INST<-INST}
  if (jjj==12){mydata$SUB.SELL.NUM.INST<-INST}
  if (jjj==13){mydata$CANC.ALL.NUM.INST<-INST}
  if (jjj==14){mydata$CANC.BUY.NUM.INST<-INST}
  if (jjj==15){mydata$CANC.SELL.NUM.INST<-INST}
  if (jjj==16){mydata$EXE.ALL.NUM.INST<-INST}
  if (jjj==17){mydata$EXE.BUY.NUM.INST<-INST}
  if (jjj==18){mydata$EXE.SELL.NUM.INST<-INST}
  
  rm(INST)
  
}

##instrument validity

qq<-c('MPID.SUB.RATIO','MPID.SUB.BUY.RATIO','MPID.SUB.SELL.RATIO',
      'MPID.CANC.RATIO','MPID.CANC.BUY.RATIO','MPID.CANC.SELL.RATIO',
      'MPID.EXE.RATIO','MPID.EXE.BUY.RATIO','MPID.EXE.SELL.RATIO')
qq2<-c('SUB.ALL.NUM','SUB.BUY.NUM','SUB.SELL.NUM','CANC.ALL.NUM','CANC.BUY.NUM','CANC.SELL.NUM',
       'EXE.ALL.NUM','EXE.BUY.NUM','EXE.SELL.NUM')
qq<-c(qq,qq2)

qq2<-c('MPID.SUB.INST','MPID.SUB.BUY.INST','MPID.SUB.SELL.INST',
       'MPID.CANC.INST','MPID.CANC.BUY.INST','MPID.CANC.SELL.INST',
       'MPID.EXE.INST','MPID.EXE.BUY.INST','MPID.EXE.SELL.INST',
       'SUB.ALL.NUM.INST','SUB.BUY.NUM.INST','SUB.SELL.NUM.INST','CANC.ALL.NUM.INST','CANC.BUY.NUM.INST','CANC.SELL.NUM.INST',
       'EXE.ALL.NUM.INST','EXE.BUY.NUM.INST','EXE.SELL.NUM.INST')
validity<-matrix(NA,length(qq),length(ticks))
confintervals<-matrix(NA,length(qq),length(ticks)*2)
for (jjj in 1:length(qq)){
  v1<-which(colnames(mydata)==qq[jjj])  
  v2<-which(colnames(mydata)==qq2[jjj])  
  for (vv in 1:length(ticks)){
    vv1<-which(mydata$TICK==vv)
    validity[jjj,vv]<-cor(mydata[vv1,v1],mydata[vv1,v2])
    test<-cor.test(mydata[vv1,v1],mydata[vv1,v2])
    confintervals[jjj,(2*(vv-1)+1):(2*(vv-1)+2)]<-test$conf.int
  }  
}

colnames(validity)<-ticks
rownames(validity)<-qq

#filename<-paste(mypath,"Results/", folder, "/INSTRUMENT_VALIDITY_BYFIRM_",w,"sec.xlsx",sep="")
#write.xlsx(validity,file=filename,append=FALSE) 
#write.xlsx(confintervals,sheetName="Sheet2",file=filename,append=TRUE) 

vars<-c('TICK','TIMEPOINT','DAY','MPID.SUB.RATIO','MPID.SUB.BUY.RATIO','MPID.SUB.SELL.RATIO','MPID.SUB.INST',
        'MPID.SUB.BUY.INST','MPID.SUB.SELL.INST','SUB.ALL.NUM','SUB.BUY.NUM','SUB.SELL.NUM',
        'SUB.ALL.NUM.INST','SUB.BUY.NUM.INST','SUB.SELL.NUM.INST',"RELSPR","RELEFFSPR.EXE.REP","RELRLZSPR.EXE.REP.PRE",
        "RELRLZSPR.EXE.REP.POST",
        "VOL.GRID.SHORT.PRE","VOL.SD","VOL.GRID.LONG.PRE","VOL.GRID.SHORT.POST","VOL.GRID.LONG.POST",
        "AGGR.REL.TOTAL","ORSZ.DVOL.TOTAL","AGGR.REL.TMBR",
        "ORSZ.DVOL.TMBR","SUB.ALL.DVOL","EXE.ALL.DVOL","SUB.BUY.DVOL","EXE.BUY.DVOL","SUB.SELL.DVOL","EXE.SELL.DVOL",
        "DEPTH.TOTAL.DVOL","DEPTH.BUY.DVOL","DEPTH.SELL.DVOL","RELDPR.MID","NEG.DUMMY",'OPEN','HASBROUCK.MAX')
mydata<-mydata[,vars]

mydata<-rename(mydata, c("AGGR.REL.TMBR"="AGGR.REL.MPID", "ORSZ.DVOL.TMBR"="ORSZ.DVOL.MPID"))

mydata<-pdata.frame(mydata,index=c("TICK","TIMEPOINT"))

####PANEL REGRESSIONS

numtests<-28

day.f<-factor(mydata$DAY)
DUMMIES<-model.matrix(~day.f)

tests<-matrix(NA,numtests,12)
numtest<-6
stationary<-matrix(NA,numtests,numtest*2*length(ticks))
diagnostics<-matrix(NA,numtests,10)
mpidlags<-1
depvarlags<-5


#####define first stage regressions

for (jj in 1:numtests){
  
  if (jj==1){     
    
    reg1<-MPID.SUB.RATIO~lag(MPID.SUB.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1:depvarlags)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.ALL.NUM.INST,k=1:mpidlags)
    reg2<-SUB.ALL.NUM~lag(MPID.SUB.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1:depvarlags)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.ALL.NUM.INST,k=1:mpidlags)
    
    model<-RELSPR~lag(MPID.SUB.HAT,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1:depvarlags)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.NUM.HAT,k=1:mpidlags)
    
    
  }
  if (jj==2){    
    
    reg1<-MPID.SUB.BUY.RATIO~lag(MPID.SUB.BUY.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1:depvarlags)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.BUY.NUM.INST,k=1:mpidlags)
    reg2<-SUB.BUY.NUM~lag(MPID.SUB.BUY.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1:depvarlags)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.BUY.NUM.INST,k=1:mpidlags)
    
    model<-RELSPR~lag(MPID.SUB.HAT,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1:depvarlags)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.NUM.HAT,k=1:mpidlags)
    
  }    
  
  if (jj==3){    
    
    reg1<-MPID.SUB.SELL.RATIO~lag(MPID.SUB.SELL.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1:depvarlags)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.SELL.NUM.INST,k=1:mpidlags)
    reg2<-SUB.SELL.NUM~lag(MPID.SUB.SELL.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1:depvarlags)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.SELL.NUM.INST,k=1:mpidlags)
    
    model<-RELSPR~lag(MPID.SUB.HAT,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1:depvarlags)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.NUM.HAT,k=1:mpidlags)
    
    
  }    
  if (jj==4){    
    
    reg1<-MPID.SUB.RATIO~lag(MPID.SUB.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELEFFSPR.EXE.REP,k=1:depvarlags)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.ALL.NUM.INST,k=1:mpidlags)
    reg2<-SUB.ALL.NUM~lag(MPID.SUB.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELEFFSPR.EXE.REP,k=1:depvarlags)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.ALL.NUM.INST,k=1:mpidlags)
    
    model<-RELEFFSPR.EXE.REP~lag(MPID.SUB.HAT,k=c(1:mpidlags))+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELEFFSPR.EXE.REP,k=1:depvarlags)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.NUM.HAT,k=1:mpidlags)
    
  }
  if (jj==5){     
    
    reg1<-MPID.SUB.RATIO~lag(MPID.SUB.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELRLZSPR.EXE.REP.PRE,k=1:depvarlags)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.ALL.NUM.INST,k=1:mpidlags)
    reg2<-SUB.ALL.NUM~lag(MPID.SUB.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELRLZSPR.EXE.REP.PRE,k=1:depvarlags)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.ALL.NUM.INST,k=1:mpidlags)
    
    model<-RELRLZSPR.EXE.REP.POST~lag(MPID.SUB.HAT,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELRLZSPR.EXE.REP.PRE,k=1:depvarlags)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.NUM.HAT,k=1:mpidlags) 
    
  }
  ##VOLATILITY
  if (jj==6){   
    
    reg1<-MPID.SUB.RATIO~lag(MPID.SUB.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1:depvarlags)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.ALL.NUM.INST,k=1:mpidlags)
    reg2<-SUB.ALL.NUM~lag(MPID.SUB.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1:depvarlags)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.ALL.NUM.INST,k=1:mpidlags)
    
    
    model<-VOL.GRID.SHORT.POST~lag(MPID.SUB.HAT,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1:depvarlags)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.NUM.HAT,k=1:mpidlags)
    
  }
  if (jj==7){    
    
    reg1<-MPID.SUB.BUY.RATIO~lag(MPID.SUB.BUY.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1:depvarlags)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.BUY.NUM.INST,k=1:mpidlags)
    reg2<-SUB.BUY.NUM~lag(MPID.SUB.BUY.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1:depvarlags)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.BUY.NUM.INST,k=1:mpidlags)
    
    model<-VOL.GRID.SHORT.POST~lag(MPID.SUB.HAT,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1:depvarlags)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.NUM.HAT,k=1:mpidlags)
    
  }    
  if (jj==8){    
    
    reg1<-MPID.SUB.SELL.RATIO~lag(MPID.SUB.SELL.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1:depvarlags)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.SELL.NUM.INST,k=1:mpidlags)
    reg2<-SUB.SELL.NUM~lag(MPID.SUB.SELL.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1:depvarlags)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.SELL.NUM.INST,k=1:mpidlags)
    
    model<-VOL.GRID.SHORT.POST~lag(MPID.SUB.HAT,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1:depvarlags)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.NUM.HAT,k=1:mpidlags)
    
  }    
  
  if (jj==9){     
    
    reg1<-MPID.SUB.RATIO~lag(MPID.SUB.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.SD,k=1:depvarlags)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.ALL.NUM.INST,k=1:mpidlags)
    reg2<-SUB.ALL.NUM~lag(MPID.SUB.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.SD,k=1:depvarlags)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.ALL.NUM.INST,k=1:mpidlags)
    
    model<-VOL.SD~lag(MPID.SUB.HAT,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.SD,k=1:depvarlags)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.NUM.HAT,k=1:mpidlags)
    
  }
  
  if (jj==10){     
    
    reg1<-MPID.SUB.RATIO~lag(MPID.SUB.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.LONG.PRE,k=1:depvarlags)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.ALL.NUM.INST,k=1:mpidlags)
    reg2<-SUB.ALL.NUM~lag(MPID.SUB.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.LONG.PRE,k=1:depvarlags)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.ALL.NUM.INST,k=1:mpidlags)
    
    model<-VOL.GRID.LONG.POST~lag(MPID.SUB.HAT,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.LONG.PRE,k=1:depvarlags)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.NUM.HAT,k=1:mpidlags)
    
  }
  ##SUBMISSION VOLUME
  if (jj==11){     
    
    reg1<-MPID.SUB.RATIO~lag(MPID.SUB.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1:depvarlags)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.ALL.NUM.INST,k=1:mpidlags)
    reg2<-SUB.ALL.NUM~lag(MPID.SUB.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1:depvarlags)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.ALL.NUM.INST,k=1:mpidlags)    
    
    model<-SUB.ALL.DVOL~lag(MPID.SUB.HAT,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1:depvarlags)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.NUM.HAT,k=1:mpidlags)
    
  }
  if (jj==12){    
    
    reg1<-MPID.SUB.BUY.RATIO~lag(MPID.SUB.BUY.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1:depvarlags)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.BUY.NUM.INST,k=1:mpidlags)
    reg2<-SUB.BUY.NUM~lag(MPID.SUB.BUY.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1:depvarlags)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.BUY.NUM.INST,k=1:mpidlags)
    
    model<-SUB.BUY.DVOL~lag(MPID.SUB.HAT,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1:depvarlags)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.NUM.HAT,k=1:mpidlags)
    
  }    
  if (jj==13){    
    
    reg1<-MPID.SUB.SELL.RATIO~lag(MPID.SUB.SELL.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1:depvarlags)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.SELL.NUM.INST,k=1:mpidlags)
    reg2<-SUB.SELL.NUM~lag(MPID.SUB.SELL.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1:depvarlags)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.SELL.NUM.INST,k=1:mpidlags)
    
    model<-SUB.SELL.DVOL~lag(MPID.SUB.HAT,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1:depvarlags)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.NUM.HAT,k=1:mpidlags)
    
  }    
  if (jj==14){  
    
    reg1<-MPID.SUB.SELL.RATIO~lag(MPID.SUB.SELL.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1:depvarlags)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.SELL.NUM.INST,k=1:mpidlags)
    reg2<-SUB.SELL.NUM~lag(MPID.SUB.SELL.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1:depvarlags)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.SELL.NUM.INST,k=1:mpidlags)
    
    model<-SUB.BUY.DVOL~lag(MPID.SUB.HAT,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1:depvarlags)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.NUM.HAT,k=1:mpidlags)
    
    
  }    
  if (jj==15){   
    
    reg1<-MPID.SUB.BUY.RATIO~lag(MPID.SUB.BUY.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1:depvarlags)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.BUY.NUM.INST,k=1:mpidlags)
    reg2<-SUB.BUY.NUM~lag(MPID.SUB.BUY.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1:depvarlags)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.BUY.NUM.INST,k=1:mpidlags)
    
    model<-SUB.SELL.DVOL~lag(MPID.SUB.HAT,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1:depvarlags)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.NUM.HAT,k=1:mpidlags)
    
  } 
  ##EXECUTION VOLUME
  if (jj==16){     
    
    reg1<-MPID.SUB.RATIO~lag(MPID.SUB.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1:depvarlags)+
      lag(DEPTH.TOTAL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.ALL.NUM.INST,k=1:mpidlags)
    reg2<-SUB.ALL.NUM~lag(MPID.SUB.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1:depvarlags)+
      lag(DEPTH.TOTAL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.ALL.NUM.INST,k=1:mpidlags)   
    
    model<-EXE.ALL.DVOL~lag(MPID.SUB.HAT,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1:depvarlags)+
      lag(DEPTH.TOTAL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.NUM.HAT,k=1:mpidlags)
    
  }
  if (jj==17){    
    
    reg1<-MPID.SUB.BUY.RATIO~lag(MPID.SUB.BUY.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1:depvarlags)+
      lag(DEPTH.BUY.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.BUY.NUM.INST,k=1:mpidlags)
    reg2<-SUB.BUY.NUM~lag(MPID.SUB.BUY.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1:depvarlags)+
      lag(DEPTH.BUY.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.BUY.NUM.INST,k=1:mpidlags)
    
    model<-EXE.BUY.DVOL~lag(MPID.SUB.HAT,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1:depvarlags)+
      lag(DEPTH.BUY.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.NUM.HAT,k=1:mpidlags)
    
  }    
  if (jj==18){   
    
    reg1<-MPID.SUB.SELL.RATIO~lag(MPID.SUB.SELL.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1:depvarlags)+
      lag(DEPTH.SELL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.SELL.NUM.INST,k=1:mpidlags)
    reg2<-SUB.SELL.NUM~lag(MPID.SUB.SELL.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1:depvarlags)+
      lag(DEPTH.SELL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.SELL.NUM.INST,k=1:mpidlags)
    
    model<-EXE.SELL.DVOL~lag(MPID.SUB.HAT,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1:depvarlags)+
      lag(DEPTH.SELL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.NUM.HAT,k=1:mpidlags)
    
  }  
  if (jj==19){   
    
    reg1<-MPID.SUB.SELL.RATIO~lag(MPID.SUB.SELL.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1:depvarlags)+
      lag(DEPTH.BUY.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.SELL.NUM.INST,k=1:mpidlags)
    reg2<-SUB.SELL.NUM~lag(MPID.SUB.SELL.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1:depvarlags)+
      lag(DEPTH.BUY.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.SELL.NUM.INST,k=1:mpidlags)
    
    model<-EXE.BUY.DVOL~lag(MPID.SUB.HAT,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1:depvarlags)+
      lag(DEPTH.BUY.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.NUM.HAT,k=1:mpidlags)
    
  }    
  if (jj==20){    
    
    reg1<-MPID.SUB.BUY.RATIO~lag(MPID.SUB.BUY.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1:depvarlags)+
      lag(DEPTH.SELL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.BUY.NUM.INST,k=1:mpidlags)
    reg2<-SUB.BUY.NUM~lag(MPID.SUB.BUY.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1:depvarlags)+
      lag(DEPTH.SELL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.BUY.NUM.INST,k=1:mpidlags)
    
    model<-EXE.SELL.DVOL~lag(MPID.SUB.HAT,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1:depvarlags)+
      lag(DEPTH.SELL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.NUM.HAT,k=1:mpidlags)
    
  }       
  ##DEPTH
  if (jj==21){    
    
    reg1<-MPID.SUB.RATIO~lag(MPID.SUB.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1:depvarlags)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.ALL.NUM.INST,k=1:mpidlags)
    reg2<-SUB.ALL.NUM~lag(MPID.SUB.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1:depvarlags)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.ALL.NUM.INST,k=1:mpidlags)   
    
    model<-DEPTH.TOTAL.DVOL~lag(MPID.SUB.HAT,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1:depvarlags)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.NUM.HAT,k=1:mpidlags)
    
  }
  if (jj==22){    
    
    reg1<-MPID.SUB.BUY.RATIO~lag(MPID.SUB.BUY.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1:depvarlags)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.BUY.NUM.INST,k=1:mpidlags)
    reg2<-SUB.BUY.NUM~lag(MPID.SUB.BUY.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1:depvarlags)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.BUY.NUM.INST,k=1:mpidlags)
    
    model<-DEPTH.BUY.DVOL~lag(MPID.SUB.HAT,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1:depvarlags)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.NUM.HAT,k=1:mpidlags)
    
  }    
  if (jj==23){    
    
    reg1<-MPID.SUB.SELL.RATIO~lag(MPID.SUB.SELL.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1:depvarlags)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.SELL.NUM.INST,k=1:mpidlags)
    reg2<-SUB.SELL.NUM~lag(MPID.SUB.SELL.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1:depvarlags)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.SELL.NUM.INST,k=1:mpidlags)
    
    model<-DEPTH.SELL.DVOL~lag(MPID.SUB.HAT,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1:depvarlags)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.NUM.HAT,k=1:mpidlags)
    
  }  
  if (jj==24){  
    
    reg1<-MPID.SUB.SELL.RATIO~lag(MPID.SUB.SELL.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1:depvarlags)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.SELL.NUM.INST,k=1:mpidlags)
    reg2<-SUB.SELL.NUM~lag(MPID.SUB.SELL.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1:depvarlags)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.SELL.NUM.INST,k=1:mpidlags)
    
    model<-DEPTH.BUY.DVOL~lag(MPID.SUB.HAT,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1:depvarlags)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.NUM.HAT,k=1:mpidlags)
    
  }    
  if (jj==25){    
    
    reg1<-MPID.SUB.RATIO~lag(MPID.SUB.BUY.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1:depvarlags)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.BUY.NUM.INST,k=1:mpidlags)
    reg2<-SUB.BUY.NUM~lag(MPID.SUB.BUY.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1:depvarlags)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.BUY.NUM.INST,k=1:mpidlags)
    
    model<-DEPTH.SELL.DVOL~lag(MPID.SUB.HAT,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1:depvarlags)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.NUM.HAT,k=1:mpidlags)
  }   
  
  ##HASBROUCK
  if (jj==26){    
    
    reg1<-MPID.SUB.RATIO~lag(MPID.SUB.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.ALL.NUM.INST,k=1:mpidlags)+lag(HASBROUCK.MAX,k=1:depvarlags)
    reg2<-SUB.ALL.NUM~lag(MPID.SUB.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.ALL.NUM.INST,k=1:mpidlags)+lag(HASBROUCK.MAX,k=1:depvarlags)   
    
    model<-HASBROUCK.MAX~lag(MPID.SUB.HAT,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.NUM.HAT,k=1:mpidlags)+lag(HASBROUCK.MAX,k=1:depvarlags)
    
  }  
  if (jj==27){  
    
    reg1<-MPID.SUB.BUY.RATIO~lag(MPID.SUB.BUY.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.BUY.NUM.INST,k=1:mpidlags)+lag(HASBROUCK.MAX,k=1:depvarlags)
    reg2<-SUB.BUY.NUM~lag(MPID.SUB.BUY.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.BUY.NUM.INST,k=1:mpidlags)+lag(HASBROUCK.MAX,k=1:depvarlags)
    
    model<-HASBROUCK.MAX~lag(MPID.SUB.HAT,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.NUM.HAT,k=1:mpidlags)+lag(HASBROUCK.MAX,k=1:depvarlags)
    
  }    
  if (jj==28){    
    
    reg1<-MPID.SUB.SELL.RATIO~lag(MPID.SUB.SELL.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.SELL.NUM.INST,k=1:mpidlags)+lag(HASBROUCK.MAX,k=1:depvarlags)
    reg2<-HASBROUCK.MAX~lag(MPID.SUB.SELL.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1)+OPEN+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+lag(SUB.SELL.NUM.INST,k=1:mpidlags)+lag(HASBROUCK.MAX,k=1:depvarlags)
    
    model<-HASBROUCK.MAX~lag(MPID.SUB.HAT,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(SUB.NUM.HAT,k=1:mpidlags)+lag(HASBROUCK.MAX,k=1:depvarlags)
  }   
  
  filename<-paste(mypath,"Results/", folder, "/Panel_Regression_Post_",w,"sec_",jj,".txt",sep="")
  
  
  ##pooled OLS regression, stage 1
  
  fit<-plm(reg1,data=mydata,model="pooling",index=c("TICK","TIMEPOINT")); k<-length(names(fit$coefficients))
  
  #ftest
  
  if (jj%in%c(1,4,5,6,9,10,11,16,21,26)==1){
    m<-update(reg1,.~.-lag(MPID.SUB.INST,k=c(1:mpidlags))-lag(SUB.ALL.NUM.INST,k=1:mpidlags))}
  if (jj%in%c(2,7,12,15,17,20,22,25,27)==1){
    m<-update(reg1,.~.-lag(MPID.SUB.BUY.INST,k=c(1:mpidlags))-lag(SUB.BUY.NUM.INST,k=1:mpidlags))}
  if (jj%in%c(3,8,13,14,18,19,23,24,28)==1){
    m<-update(reg1,.~.-lag(MPID.SUB.SELL.INST,k=c(1:mpidlags))-lag(SUB.SELL.NUM.INST,k=1:mpidlags))}  
  
  fit2<-plm(m,data=mydata,model="pooling",index=c("TICK","TIMEPOINT"))
  q<-length(names(fit$coefficients))-length(names(fit2$coefficients)) #number of restrictions
  n<-length(fit$residuals)
  SSRr<-sum(residuals(fit2)^2); SSRur<-sum(residuals(fit)^2)
  Fstat1<-((SSRr-SSRur)/q)/(SSRur/(n-k+1))
  pval1<-1-pf(Fstat1, df1=q, df2=(n-k+1))
  
  MPID.SUB.HAT<-fit$model[[1]] - fit$residuals; test<-data.frame(attr(MPID.SUB.HAT,"index"))
  MPID.SUB.HAT<-as.numeric(MPID.SUB.HAT); MPID.SUB.HAT<-data.frame(MPID.SUB.HAT)
  MPID.SUB.HAT$TICK<-as.numeric(test$TICK)
  MPID.SUB.HAT$TIMEPOINT<-as.numeric(test$TIMEPOINT)+depvarlags
  mydata<-merge(mydata,MPID.SUB.HAT,by=c("TICK","TIMEPOINT"),all=T)
  
  fit<-plm(reg2,data=mydata,model="pooling",index=c("TICK","TIMEPOINT"))
  
  #ftest
  
  if (jj%in%c(1,4,5,6,9,10,11,16,21,26)==1){
    m<-update(reg2,.~.-lag(MPID.SUB.INST,k=c(1:mpidlags))-lag(SUB.ALL.NUM.INST,k=1:mpidlags))}
  if (jj%in%c(2,7,12,15,17,20,22,25,27)==1){
    m<-update(reg2,.~.-lag(MPID.SUB.BUY.INST,k=c(1:mpidlags))-lag(SUB.BUY.NUM.INST,k=1:mpidlags))}
  if (jj%in%c(3,8,13,14,18,19,23,24,28)==1){
    m<-update(reg2,.~.-lag(MPID.SUB.SELL.INST,k=c(1:mpidlags))-lag(SUB.SELL.NUM.INST,k=1:mpidlags))}  
  
  fit2<-plm(m,data=mydata,model="pooling",index=c("TICK","TIMEPOINT"))
  q<-length(names(fit$coefficients))-length(names(fit2$coefficients)) #number of restrictions
  n<-length(fit$residuals)
  SSRr<-sum(residuals(fit2)^2); SSRur<-sum(residuals(fit)^2)
  Fstat2<-((SSRr-SSRur)/q)/(SSRur/(n-k))
  pval2<-1-pf(Fstat2, df1=q, df2=(n-k))
  
  SUB.NUM.HAT<-fit$model[[1]] - fit$residuals; test<-data.frame(attr(SUB.NUM.HAT,"index"))
  SUB.NUM.HAT<-as.numeric(SUB.NUM.HAT); SUB.NUM.HAT<-data.frame(SUB.NUM.HAT)
  SUB.NUM.HAT$TICK<-as.numeric(test$TICK)
  SUB.NUM.HAT$TIMEPOINT<-as.numeric(test$TIMEPOINT)+depvarlags
  mydata<-merge(mydata,SUB.NUM.HAT,by=c("TICK","TIMEPOINT"),all=T)  
  
  #cat(paste("\n \n","Pooled OLS Regression #", jj, ", Dep Var: ",all.vars(model)[1],"\n \n",sep=""),file=filename,append=FALSE)
  
  #mydata<-pdata.frame(mydata,index=c("TICK","TIMEPOINT"))
  fit1<-plm(model,data=mydata,model="pooling",index=c("TICK","TIMEPOINT"))
  
  s<-summary(fit1)
  capture.output(s, file=filename,append=T)
  #clustered errors
  #<-coeftest(fit1, vcov=vcovHC(fit1,type="HC0",cluster="group"))
  #capture.output(s, file=filename,append=T)
  dw1<-pdwtest(fit1,alternative="two.sided")
  capture.output(dw1, file=filename,append=T)
  
  #test for fixed effects
  test1<-plmtest(fit1,effect="individual")
  capture.output(test1, file=filename,append=T)
  #test2<-plmtest(fit1,effect="individual", type="bp")
  lgtest<-test1
  
  rm(MPID.SUB.HAT,SUB.NUM.HAT)
  q<-which(colnames(mydata)%in%c("MPID.SUB.HAT","SUB.NUM.HAT"))
  mydata<-mydata[,-q]
  
  #####model + dummies, stage 1
  
  reg1<-update(reg1,.~.+DUMMIES)
  reg2<-update(reg2,.~.+DUMMIES)
  model2<-update(model,.~.+DUMMIES)
  
  fit<-plm(reg1,data=mydata,model="pooling",index=c("TICK","TIMEPOINT")); k<-length(names(fit$coefficients))
  
  #ftest
  
  if (jj%in%c(1,4,5,6,9,10,11,16,21,26)==1){
    m<-update(reg1,.~.-lag(MPID.SUB.INST,k=c(1:mpidlags))-lag(SUB.ALL.NUM.INST,k=1:mpidlags))}
  if (jj%in%c(2,7,12,15,17,20,22,25,27)==1){
    m<-update(reg1,.~.-lag(MPID.SUB.BUY.INST,k=c(1:mpidlags))-lag(SUB.BUY.NUM.INST,k=1:mpidlags))}
  if (jj%in%c(3,8,13,14,18,19,23,24,28)==1){
    m<-update(reg1,.~.-lag(MPID.SUB.SELL.INST,k=c(1:mpidlags))-lag(SUB.SELL.NUM.INST,k=1:mpidlags))}  
  
  fit2<-plm(m,data=mydata,model="pooling",index=c("TICK","TIMEPOINT"))
  q<-length(names(fit$coefficients))-length(names(fit2$coefficients)) #number of restrictions
  n<-length(fit$residuals)
  SSRr<-sum(residuals(fit2)^2); SSRur<-sum(residuals(fit)^2)
  Fstat3<-((SSRr-SSRur)/q)/(SSRur/(n-k+1))
  pval3<-1-pf(Fstat3, df1=q, df2=(n-k+1))
  
  MPID.SUB.HAT<-fit$model[[1]] - fit$residuals; test<-data.frame(attr(MPID.SUB.HAT,"index"))
  MPID.SUB.HAT<-as.numeric(MPID.SUB.HAT); MPID.SUB.HAT<-data.frame(MPID.SUB.HAT)
  MPID.SUB.HAT$TICK<-as.numeric(test$TICK)
  MPID.SUB.HAT$TIMEPOINT<-as.numeric(test$TIMEPOINT)+depvarlags
  mydata<-merge(mydata,MPID.SUB.HAT,by=c("TICK","TIMEPOINT"),all=T)
  
  fit<-plm(reg2,data=mydata,model="pooling",index=c("TICK","TIMEPOINT"))
  
  #ftest
  
  if (jj%in%c(1,4,5,6,9,10,11,16,21)==1){
    m<-update(reg2,.~.-lag(MPID.SUB.INST,k=c(1:mpidlags))-lag(SUB.ALL.NUM.INST,k=1:mpidlags))}
  if (jj%in%c(2,7,12,15,17,20,22,25)==1){
    m<-update(reg2,.~.-lag(MPID.SUB.BUY.INST,k=c(1:mpidlags))-lag(SUB.BUY.NUM.INST,k=1:mpidlags))}
  if (jj%in%c(3,8,13,14,18,19,23,24)==1){
    m<-update(reg2,.~.-lag(MPID.SUB.SELL.INST,k=c(1:mpidlags))-lag(SUB.SELL.NUM.INST,k=1:mpidlags))}  
  
  fit2<-plm(m,data=mydata,model="pooling",index=c("TICK","TIMEPOINT"))
  q<-length(names(fit$coefficients))-length(names(fit2$coefficients)) #number of restrictions
  n<-length(fit$residuals)
  SSRr<-sum(residuals(fit2)^2); SSRur<-sum(residuals(fit)^2)
  Fstat4<-((SSRr-SSRur)/q)/(SSRur/(n-k))
  pval4<-1-pf(Fstat4, df1=q, df2=(n-k))
  
  SUB.NUM.HAT<-fit$model[[1]] - fit$residuals; test<-data.frame(attr(SUB.NUM.HAT,"index"))
  SUB.NUM.HAT<-as.numeric(SUB.NUM.HAT); SUB.NUM.HAT<-data.frame(SUB.NUM.HAT)
  SUB.NUM.HAT$TICK<-as.numeric(test$TICK)
  SUB.NUM.HAT$TIMEPOINT<-as.numeric(test$TIMEPOINT)+depvarlags
  mydata<-merge(mydata,SUB.NUM.HAT,by=c("TICK","TIMEPOINT"),all=T)  
  
  ##firm+day fixed effects
  #model2<-update(model, . ~ . + DUMMIES)
  cat(paste("\n \n","Firm+Day Fixed Regression #", jj, ", Dep Var: ",all.vars(model)[1],"\n \n",sep=""),file=filename,append=TRUE)
  fit3<-plm(model2,data=mydata,model="pooling",index=c("TICK","TIMEPOINT"))
  s<-summary(fit3)
  capture.output(s,file=filename,append=T)    
  #clustered errors
  #s<-coeftest(fit3, vcov=vcovHC(fit3,type="HC0",cluster="group"))
  #capture.output(s, file=filename,append=T)
  dw1<-pdwtest(fit3,alternative="two.sided")
  capture.output(dw1, file=filename,append=T)
  
  rm(MPID.SUB.HAT,SUB.NUM.HAT)
  q<-which(colnames(mydata)%in%c("MPID.SUB.HAT","SUB.NUM.HAT"))
  mydata<-mydata[,-q]
  
  filename<-paste(mypath,"Results/", folder, "/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
  save(fit1,fit3,file=filename)
  
  diagnostics[jj,]<-cbind(Fstat1,pval1,Fstat2,pval2,Fstat3,pval3,Fstat4,pval4,lgtest$statistic,lgtest$p.value)
  
  
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

#colnames(stationary)<-c('AAPL',vector(mode="numeric",length=(2*numtest-1)),'CSCO',vector(mode="numeric",length=(2*numtest-1)),
#                        'EBAY',vector(mode="numeric",length=(2*numtest-1)),'FB',vector(mode="numeric",length=(2*numtest-1)),
#                        'GOOG',vector(mode="numeric",length=(2*numtest-1)),
#                        'INTC',vector(mode="numeric",length=(2*numtest-1)),'MSFT',vector(mode="numeric",length=(2*numtest-1)),
#                        'YHOO',vector(mode="numeric",length=(2*numtest-1)))

colnames(stationary)<-c('CSCO',vector(mode="numeric",length=(2*numtest-1)),
                        'EBAY',vector(mode="numeric",length=(2*numtest-1)),'FB',vector(mode="numeric",length=(2*numtest-1)),
                        'GOOG',vector(mode="numeric",length=(2*numtest-1)),
                        'INTC',vector(mode="numeric",length=(2*numtest-1)),'MSFT',vector(mode="numeric",length=(2*numtest-1)),
                        'YHOO',vector(mode="numeric",length=(2*numtest-1)))

filename<-paste(mypath,"Results/", folder, "/POST_PANELERRORS_STATIONARITY_BYFIRM_",w,"sec.xlsx",sep="")
write.xlsx(stationary,file=filename,append=FALSE) 

filename<-paste(mypath,"Results/", folder, "/INST_DIAGNOSTICS_BYFIRM_",w,"sec.xlsx",sep="")
write.xlsx(diagnostics,file=filename,append=FALSE) 


#}


