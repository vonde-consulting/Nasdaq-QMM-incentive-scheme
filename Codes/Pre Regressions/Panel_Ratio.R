rm(list=ls(all=TRUE))
library(car)
library(plm)
library(tseries)
library(xlsx)
library(lmtest)
library(sandwich)

#panel <- function(jj){

#windows<-c(120,60,30,10)
windows<-c(120,60,30)
ticks<-c("AAPL","CSCO","EBAY","FB","GOOG","INTC","MSFT","YHOO")
folder<-"Panel_Ratio"

#  for (k in 3:length(windows)){

k<-3
w<-windows[k]

#unstandardized data
i<-1

mypath<-"C:/Users/Julia/Dropbox/Projects/Trader Anonymity/Data/"

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

mydata<-read.table(file=paste(mypath,ticks[i],"_",w,"sec.rda",sep=""))
hasbrouck<-read.table(file=paste(mypath,"/Hasbrouck/Hasbrouck_",ticks[i],"_",w,"sec.rda",sep=""))
mydata$HASBROUCK.MAX<-hasbrouck$HASBROUCK.MAX
mydata$HASBROUCK.MEAN<-hasbrouck$HASBROUCK.MEAN
mydata$TICK<-rep(ticks[i],nrow(mydata))
mydata$TIMEPOINT<-seq(1,nrow(mydata),1)

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

#<-which(is.nan(mydata_unst$MPID.SUB.RATIO))

##stationarity of regressors

nms<-c("MPID.SUB.RATIO","MPID.SUB.BUY.RATIO","MPID.SUB.SELL.RATIO","RELSPR","RELEFFSPR.EXE.REP","RELRLZSPR.EXE.REP.PRE",
       "VOL.GRID.SHORT.PRE","VOL.SD","VOL.GRID.LONG.PRE","AGGR.REL.TOTAL","ORSZ.DVOL.TOTAL","AGGR.REL.MPID",
       "ORSZ.DVOL.MPID","SUB.ALL.DVOL","EXE.ALL.DVOL","SUB.BUY.DVOL","EXE.BUY.DVOL","SUB.SELL.DVOL","EXE.SELL.DVOL",
       "DEPTH.TOTAL.DVOL","DEPTH.BUY.DVOL","DEPTH.SELL.DVOL","RELDPR.MID","HASBROUCK.MAX")
numtest<-4
stationary<-matrix(NA,length(nms),numtest*2*length(ticks))

for (kk in 1:length(nms)){
  
  #kk<-1
  for (vv in 1:length(ticks)){
    
    q<-which(colnames(mydata)==nms[kk])
    qq<-which(mydata$TICK==vv)
    v<-mydata[qq,q]
    
    pdf(paste(mypath,"/Graphs/",folder , "_Regressors/",ticks[vv],"_",nms[kk],"_Ratio_",w,"sec.pdf",sep=""))
    plot(v,type = "l",ylab=nms[kk],main=ticks[vv],xlab="Date")
    dev.off()
    
    qv<-which(!is.na(v))
    v<-v[qv]
    
    test1<-adf.test(v)
    test2<-PP.test(v,lshort=FALSE)
    test3<-kpss.test(v,null=c("Level"))
    test4<-kpss.test(v,null=c("Trend"))
    
    #<-purtest(v,index=c("TICK","TIMEPOINT"),lags="AIC",exo="trend")
    #test2<-purtest(v,index=c("TICK","TIMEPOINT"),test="hadri",exo="trend",lags="AIC",Hcons=TRUE)
    
    #stationary[kk,(1+4*(vv-1)):(4+4*(vv-1))]<-c(test1$statistic$statistic,test1$statistic$p.value,test2$statistic$statistic,test2$statistic$p.value)
    stationary[kk,(1+(numtest*2)*(vv-1)):((numtest*2)+(numtest*2)*(vv-1))]<-c(test1$statistic,test1$p.value,test2$statistic,test2$p.value,
                                                                              test3$statistic,test3$p.value,test4$statistic,test4$p.value)
  }
}

rownames(stationary)<-nms
colnames(stationary)<-c('AAPL',vector(mode="numeric",length=(2*numtest-1)),'CSCO',vector(mode="numeric",length=(2*numtest-1)),
                        'EBAY',vector(mode="numeric",length=(2*numtest-1)),'FB',vector(mode="numeric",length=(2*numtest-1)),
                        'GOOG',vector(mode="numeric",length=(2*numtest-1)),
                        'INTC',vector(mode="numeric",length=(2*numtest-1)),'MSFT',vector(mode="numeric",length=(2*numtest-1)),
                        'YHOO',vector(mode="numeric",length=(2*numtest-1)))

filename<-paste(mypath,"Results/", folder ,"/REGRESSOR_STATIONARITY_BYFIRM_",w,"sec.xlsx",sep="")
write.xlsx(stationary,file=filename,append=FALSE) 

v<-mydata_unst$MPID.SUB.RATIO
v<-matrix(v,nrow=length(unique(mydata$TIMEPOINT)),ncol=length(ticks))
v<-cbind(mydata$DAY[mydata$TICK==1],mydata$TIMEPOINT[mydata$TICK==1],v)
filename<-paste(mypath,"MPID_SUB_RATIO_",w,"sec.xlsx",sep="")
#write.xlsx(v,file=filename,append=FALSE) 

v<-mydata_unst$MPID.SUB.BUY.RATIO
v<-matrix(v,nrow=length(unique(mydata$TIMEPOINT)),ncol=length(ticks))
v<-cbind(mydata$DAY[mydata$TICK==1],mydata$TIMEPOINT[mydata$TICK==1],v)
filename<-paste(mypath,"MPID_SUB_BUY_RATIO_",w,"sec.xlsx",sep="")
#write.xlsx(v,file=filename,append=FALSE) 

v<-mydata_unst$MPID.SUB.SELL.RATIO
v<-matrix(v,nrow=length(unique(mydata$TIMEPOINT)),ncol=length(ticks))
v<-cbind(mydata$DAY[mydata$TICK==1],mydata$TIMEPOINT[mydata$TICK==1],v)
filename<-paste(mypath,"MPID_SUB_SELL_RATIO_",w,"sec.xlsx",sep="")
#write.xlsx(v,file=filename,append=FALSE) 

vars<-c('TICK','TIMEPOINT','DAY','MPID.SUB.RATIO','MPID.SUB.BUY.RATIO','MPID.SUB.SELL.RATIO','MPID.SUB.INST',
        'MPID.SUB.BUY.INST','MPID.SUB.SELL.INST','SUB.ALL.NUM','SUB.BUY.NUM','SUB.SELL.NUM',
        'SUB.ALL.NUM.INST','SUB.BUY.NUM.INST','SUB.SELL.NUM.INST',"RELSPR","RELEFFSPR.EXE.REP","RELRLZSPR.EXE.REP.PRE",
        "RELRLZSPR.EXE.REP.POST",
        "VOL.GRID.SHORT.PRE","VOL.SD","VOL.GRID.LONG.PRE","VOL.GRID.SHORT.POST","VOL.GRID.LONG.POST",
        "AGGR.REL.TOTAL","ORSZ.DVOL.TOTAL","AGGR.REL.MPID",
        "ORSZ.DVOL.MPID","SUB.ALL.DVOL","EXE.ALL.DVOL","SUB.BUY.DVOL","EXE.BUY.DVOL","SUB.SELL.DVOL","EXE.SELL.DVOL",
        "DEPTH.TOTAL.DVOL","DEPTH.BUY.DVOL","DEPTH.SELL.DVOL","RELDPR.MID","NEG.DUMMY",'POS.DUMMY','OPEN','HASBROUCK.MAX')
mydata<-mydata[,vars]

mydata<-pdata.frame(mydata,index=c("TICK","TIMEPOINT"))

rm(mydata_unst,hasbrouck,dummy_neg,dummy_pos)

##RUN THROUGH DIFFERENT MODELS###################################

numtests<-14
numtest<-6
mpidlags<-5
depvarlags<-1

tests<-matrix(NA,numtests,12)
stationary<-matrix(NA,numtests,numtest*2*length(ticks))

for (jj in 1:numtests){
  
  #jj<-1
  
  if (jj==1){     #negative dummy
    model<-MPID.SUB.RATIO~lag(MPID.SUB.RATIO,k=c(1:mpidlags))+AGGR.REL.MPID+ORSZ.DVOL.MPID+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(HASBROUCK.MAX,k=1)
  }
  if (jj==2){    #positive dummy
    model<-MPID.SUB.RATIO~lag(MPID.SUB.RATIO,k=c(1:mpidlags))+AGGR.REL.MPID+ORSZ.DVOL.MPID+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(POS.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(POS.DUMMY,k=1))+OPEN+lag(HASBROUCK.MAX,k=1)
  }      
  if (jj==3){    #negative dummy, buy side on buy side
    model<-MPID.SUB.BUY.RATIO~lag(MPID.SUB.BUY.RATIO,k=c(1:mpidlags))+AGGR.REL.MPID+ORSZ.DVOL.MPID+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(HASBROUCK.MAX,k=1)
  }  
  if (jj==4){    #negative dummy, buy side on sell side
    model<-MPID.SUB.BUY.RATIO~lag(MPID.SUB.BUY.RATIO,k=c(1:mpidlags))+AGGR.REL.MPID+ORSZ.DVOL.MPID+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(HASBROUCK.MAX,k=1)
  }              
  if (jj==5){    #positive dummy, buy side on buy side
    model<-MPID.SUB.BUY.RATIO~lag(MPID.SUB.BUY.RATIO,k=c(1:mpidlags))+AGGR.REL.MPID+ORSZ.DVOL.MPID+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(POS.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(POS.DUMMY,k=1))+OPEN+lag(HASBROUCK.MAX,k=1)
  }  
  if (jj==6){    #positive dummy, buy side on sell side
    model<-MPID.SUB.BUY.RATIO~lag(MPID.SUB.BUY.RATIO,k=c(1:mpidlags))+AGGR.REL.MPID+ORSZ.DVOL.MPID+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(POS.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(POS.DUMMY,k=1))+OPEN+lag(HASBROUCK.MAX,k=1)
  }         
  if (jj==7){    #negative dummy, sell side on sell side
    model<-MPID.SUB.SELL.RATIO~lag(MPID.SUB.SELL.RATIO,k=c(1:mpidlags))+AGGR.REL.MPID+ORSZ.DVOL.MPID+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(HASBROUCK.MAX,k=1)
  }  
  if (jj==8){    #negative dummy, sell side on buy side
    model<-MPID.SUB.SELL.RATIO~lag(MPID.SUB.SELL.RATIO,k=c(1:mpidlags))+AGGR.REL.MPID+ORSZ.DVOL.MPID+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(HASBROUCK.MAX,k=1)
  }              
  if (jj==9){    #positive dummy, sell side on sell side
    model<-MPID.SUB.SELL.RATIO~lag(MPID.SUB.SELL.RATIO,k=c(1:mpidlags))+AGGR.REL.MPID+ORSZ.DVOL.MPID+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(POS.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(POS.DUMMY,k=1))+OPEN+lag(HASBROUCK.MAX,k=1)
  }  
  if (jj==10){    #positive dummy, sell side on buy side
    model<-MPID.SUB.SELL.RATIO~lag(MPID.SUB.SELL.RATIO,k=c(1:mpidlags))+AGGR.REL.MPID+ORSZ.DVOL.MPID+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(POS.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(POS.DUMMY,k=1))+OPEN+lag(HASBROUCK.MAX,k=1)
  }        
  if (jj==11){     #relative spreads
    model<-MPID.SUB.RATIO~lag(MPID.SUB.RATIO,k=c(1:mpidlags))+AGGR.REL.MPID+ORSZ.DVOL.MPID+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELEFFSPR.EXE.REP,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(HASBROUCK.MAX,k=1)
  }      
  if (jj==12){     #realized spreads
    model<-MPID.SUB.RATIO~lag(MPID.SUB.RATIO,k=c(1:mpidlags))+AGGR.REL.MPID+ORSZ.DVOL.MPID+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELRLZSPR.EXE.REP.PRE,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(HASBROUCK.MAX,k=1)
  }            
  if (jj==13){     #volatility #1
    model<-MPID.SUB.RATIO~lag(MPID.SUB.RATIO,k=c(1:mpidlags))+AGGR.REL.MPID+ORSZ.DVOL.MPID+
      lag(VOL.SD,k=1)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(HASBROUCK.MAX,k=1)
  }        
  if (jj==14){     #volatility #2
    model<-MPID.SUB.RATIO~lag(MPID.SUB.RATIO,k=c(1:mpidlags))+AGGR.REL.MPID+ORSZ.DVOL.MPID+
      lag(VOL.GRID.LONG.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+lag(abs(RELDPR.MID),k=1)+lag(NEG.DUMMY,k=1)+
      (lag(abs(RELDPR.MID),k=1)*lag(NEG.DUMMY,k=1))+OPEN+lag(HASBROUCK.MAX,k=1)
  }      
  

  #########################################################################################      
  
  filename<-paste(mypath,"Results/", folder, "/Panel_Regression_Ratio_",w,"sec_",jj,".txt",sep="")
  
  ##pooled OLS regression
  cat(paste("\n \n","Pooled OLS Regression #", jj, ", Dep Var: ",all.vars(model)[1],"\n \n",sep=""),file=filename,append=FALSE)
  fit1<-plm(model,data=mydata,model="pooling",index=c("TICK","TIMEPOINT"))
  s<-summary(fit1)
  capture.output(s, file=filename,append=T)
  #clustered errors
#  s<-coeftest(fit1, vcov=vcovHC(fit1,type="HC0",cluster="group"))
#  capture.output(s, file=filename,append=T)
  dw1<-pdwtest(fit1,alternative="two.sided")
  capture.output(dw1, file=filename,append=T)
  
  ##firm fixed effects
  cat(paste("\n \n","Firm Fixed Regression #", jj, ", Dep Var: ",all.vars(model)[1],"\n \n",sep=""),file=filename,append=TRUE)
  fit2<-plm(model,data=mydata,model="within",index=c("TICK","TIMEPOINT"))
  s<-summary(fit2)
  capture.output(s, file=filename,append=T)
  #clustered errors
#  s<-coeftest(fit2, vcov=vcovHC(fit2,type="HC0",cluster="group"))
#  capture.output(s, file=filename,append=T)
  dw1<-pdwtest(fit2,alternative="two.sided")
  capture.output(dw1, file=filename,append=T)
  
  ##firm+day fixed effects
  day.f<-factor(mydata$DAY)
  DUMMIES<-model.matrix(~day.f)
  model2<-update(model, . ~ . + DUMMIES)
  cat(paste("\n \n","Firm+Day Fixed Regression #", jj, ", Dep Var: ",all.vars(model)[1],"\n \n",sep=""),file=filename,append=TRUE)
  fit3<-plm(model2,data=mydata,model="within",index=c("TICK","TIMEPOINT"))
  s<-summary(fit3)
  capture.output(s,file=filename,append=T)    
  #clustered errors
#  s<-coeftest(fit3, vcov=vcovHC(fit3,type="HC0",cluster="group"))
#  capture.output(s, file=filename,append=T)
  dw1<-pdwtest(fit3,alternative="two.sided")
  capture.output(dw1, file=filename,append=T)
  
  filename<-paste(mypath,"Results/", folder, "/Panel_Regression_Ratio_",w,"sec_",jj,".rda",sep="")
  save(fit1,fit2,fit3,file=filename)
  
  ##test errors for stationarity 
  
  u<-residuals(fit3)
  uindex<-attr(u,"index")

  for (vv in 1:length(ticks)){
    
    q<-which(uindex$TICK==vv)
    v<-u[q]
    
    pdf(paste(mypath,"/Graphs/",folder ,"_Errors/",ticks[vv],"_PanelErrors_Ratio_",jj,"_",w,"sec.pdf",sep=""))
    plot(v,type = "l",ylab="Panel Errors",main=ticks[vv],xlab="Date")
    dev.off()
    
    qv<-which(!is.na(v))
    v<-v[qv]
    
    test1<-adf.test(v)
    test2<-PP.test(v,lshort=FALSE)
    test3<-kpss.test(v,null=c("Level"))
    test4<-kpss.test(v,null=c("Trend"))
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

filename<-paste(mypath,"Results/",folder,"/PANELERRORS_STATIONARITY_BYFIRM_",mpidlags,"MPIDLags_",depvarlags,"DepVarLags_",w,"sec.xlsx",sep="")
write.xlsx(stationary,file=filename,append=FALSE) 

#  return(jj)

#}

# CLUSTER ----------------------------------------------------------------

#ID <- as.integer(Sys.getenv("SGE_TASK_ID"))
#s <- panel(ID)


