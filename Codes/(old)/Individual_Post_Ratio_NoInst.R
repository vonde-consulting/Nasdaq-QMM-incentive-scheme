rm(list=ls(all=TRUE))
library(car)
library(plm)
library(tseries)
library(xlsx)
library(lmtest)
library(sandwich)
library(quantmod)
library(AER)

#individual <- function(jj){
  
  windows<-c(120,60,30,10)
  ticks<-c("AAPL","CSCO","EBAY","FB","GOOG","INTC","MSFT","YHOO")
  folder<-"Individual_Post_Ratio_NoInst"
  
  #for (k in 1:length(windows)){
  
  k<-3
  w<-windows[k]
  
  #unstandardized data
  i<-1
  
  mypath<-"C:/Users/user/Dropbox/Projects/Trader Anonymity/Data/"
  #mypath<-"Data/"
  
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
  
  ##RUN THROUGH DIFFERENT MODELS###################################
  
  #  for (l in 1:20){
  
  numregs<-25
  numtest<-6
  stationary<-matrix(NA,numregs,numtest*2*length(ticks))
  ##########
  mpidlags<-1
  depvarlags<-5
  #lags<-l
  ##########
  day.f<-factor(mydata$DAY)
  DDUMMIES<-model.matrix(~day.f); DDUMMIES<-DDUMMIES[,2:ncol(DDUMMIES)]
  
  for (jj in 1:numregs){
    
    l.relspr1<-1
    l.relspr2<-1
    l.relspr3<-1
    l.vol1<-1
    l.vol2<-1
    l.vol3<-1
    l.sub<-1
    l.sub.buy<-1
    l.sub.sell<-1
    l.exe<-1
    l.exe.buy<-1
    l.exe.sell<-1
    l.d<-1
    l.d.buy<-1
    l.d.sell<-1
  
  
  if(jj==1){
    l.relspr1<-depvarlags
    model<-RELSPR~LAG.MPID.SUB.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+
      LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.ALL.DVOL+LAG.EXE.ALL.DVOL+LAG.DEPTH.TOTAL.DVOL+OPEN+abs(LAG.RELDPR.MID)+
      LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)
    model2<-RELSPR~LAG.MPID.SUB.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+
      LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.ALL.DVOL+LAG.EXE.ALL.DVOL+LAG.DEPTH.TOTAL.DVOL+OPEN+abs(LAG.RELDPR.MID)+
      LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+DUMMIES
  }
  if(jj==2){
    l.relspr1<-depvarlags
    model<-RELSPR~LAG.MPID.SUB.BUY.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.BUY.DVOL+LAG.EXE.BUY.DVOL+LAG.DEPTH.BUY.DVOL+OPEN+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)
    model2<-RELSPR~LAG.MPID.SUB.BUY.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.BUY.DVOL+LAG.EXE.BUY.DVOL+LAG.DEPTH.BUY.DVOL+OPEN+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+DUMMIES
  }
  
  if(jj==3){
    l.relspr1<-depvarlags
    model<-RELSPR~LAG.MPID.SUB.SELL.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.SELL.DVOL+LAG.EXE.SELL.DVOL+LAG.DEPTH.SELL.DVOL+OPEN+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)
    model2<-RELSPR~LAG.MPID.SUB.SELL.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.SELL.DVOL+LAG.EXE.SELL.DVOL+LAG.DEPTH.SELL.DVOL+OPEN+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+DUMMIES
    
  }
  if(jj==4){
    l.relspr2<-depvarlags
    model<-RELEFFSPR.EXE.REP~LAG.MPID.SUB.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.VOL.GRID.SHORT.PRE+LAG.RELEFFSPR.EXE.REP+LAG.SUB.ALL.DVOL+LAG.EXE.ALL.DVOL+LAG.DEPTH.TOTAL.DVOL+OPEN+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)
    model2<-RELEFFSPR.EXE.REP~LAG.MPID.SUB.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.VOL.GRID.SHORT.PRE+LAG.RELEFFSPR.EXE.REP+LAG.SUB.ALL.DVOL+LAG.EXE.ALL.DVOL+LAG.DEPTH.TOTAL.DVOL+OPEN+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+DUMMIES
    
    
  }
  if(jj==5){
    l.relspr3<-depvarlags
    model<-RELRLZSPR.EXE.REP.POST~LAG.MPID.SUB.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELRLZSPR.EXE.REP.PRE+LAG.SUB.ALL.DVOL+LAG.EXE.ALL.DVOL+LAG.DEPTH.TOTAL.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN
    model2<-RELRLZSPR.EXE.REP.POST~LAG.MPID.SUB.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELRLZSPR.EXE.REP.PRE+LAG.SUB.ALL.DVOL+LAG.EXE.ALL.DVOL+LAG.DEPTH.TOTAL.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN+DUMMIES
    
  }
  ##VOLATILITY
  if(jj==6){
    l.vol1<-depvarlags
    model<-VOL.GRID.SHORT.POST~LAG.MPID.SUB.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.ALL.DVOL+LAG.EXE.ALL.DVOL+LAG.DEPTH.TOTAL.DVOL+OPEN+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)
    model2<-VOL.GRID.SHORT.POST~LAG.MPID.SUB.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.ALL.DVOL+LAG.EXE.ALL.DVOL+LAG.DEPTH.TOTAL.DVOL+OPEN+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+DUMMIES
    
  }
  if(jj==7){
    l.vol1<-depvarlags
    model<-VOL.GRID.SHORT.POST~LAG.MPID.SUB.BUY.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.BUY.DVOL+LAG.EXE.BUY.DVOL+LAG.DEPTH.BUY.DVOL+OPEN+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)
    model2<-VOL.GRID.SHORT.POST~LAG.MPID.SUB.BUY.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.BUY.DVOL+LAG.EXE.BUY.DVOL+LAG.DEPTH.BUY.DVOL+OPEN+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+DUMMIES
    
  }
  if(jj==8){
    l.vol1<-depvarlags
    model<-VOL.GRID.SHORT.POST~LAG.MPID.SUB.SELL.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.SELL.DVOL+LAG.EXE.SELL.DVOL+LAG.DEPTH.SELL.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN
    model2<-VOL.GRID.SHORT.POST~LAG.MPID.SUB.SELL.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.SELL.DVOL+LAG.EXE.SELL.DVOL+LAG.DEPTH.SELL.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN+DUMMIES
  }
  
  if(jj==9){
    l.vol2<-depvarlags
    model<-VOL.SD~LAG.MPID.SUB.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.SD+LAG.RELSPR+LAG.SUB.ALL.DVOL+LAG.EXE.ALL.DVOL+LAG.DEPTH.TOTAL.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN
    model2<-VOL.SD~LAG.MPID.SUB.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.SD+LAG.RELSPR+LAG.SUB.ALL.DVOL+LAG.EXE.ALL.DVOL+LAG.DEPTH.TOTAL.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN+DUMMIES
  }
  
  if(jj==10){
    l.vol3<-depvarlags
    model<-VOL.GRID.LONG.POST~LAG.MPID.SUB.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.LONG.PRE+LAG.RELSPR+LAG.SUB.ALL.DVOL+LAG.EXE.ALL.DVOL+LAG.DEPTH.TOTAL.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN
    model2<-VOL.GRID.LONG.POST~LAG.MPID.SUB.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.LONG.PRE+LAG.RELSPR+LAG.SUB.ALL.DVOL+LAG.EXE.ALL.DVOL+LAG.DEPTH.TOTAL.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN+DUMMIES
  }
  ##SUBMISSIONVOLUME
  if(jj==11){
    l.sub<-depvarlags
    model<-SUB.ALL.DVOL~LAG.MPID.SUB.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.ALL.DVOL+LAG.EXE.ALL.DVOL+LAG.DEPTH.TOTAL.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN
    model2<-SUB.ALL.DVOL~LAG.MPID.SUB.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.ALL.DVOL+LAG.EXE.ALL.DVOL+LAG.DEPTH.TOTAL.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN+DUMMIES
    
  }
  if(jj==12){
    l.sub.buy<-depvarlags
    model<-SUB.BUY.DVOL~LAG.MPID.SUB.BUY.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.BUY.DVOL+LAG.EXE.BUY.DVOL+LAG.DEPTH.BUY.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN
    model2<-SUB.BUY.DVOL~LAG.MPID.SUB.BUY.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.BUY.DVOL+LAG.EXE.BUY.DVOL+LAG.DEPTH.BUY.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN+DUMMIES
    
  }
  if(jj==13){
    l.sub.sell<-depvarlags
    model<-SUB.SELL.DVOL~LAG.MPID.SUB.SELL.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.SELL.DVOL+LAG.EXE.SELL.DVOL+LAG.DEPTH.SELL.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN
    model2<-SUB.SELL.DVOL~LAG.MPID.SUB.SELL.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.SELL.DVOL+LAG.EXE.SELL.DVOL+LAG.DEPTH.SELL.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN+DUMMIES
  }
  if(jj==14){
    l.sub.buy<-depvarlags
    model<-SUB.BUY.DVOL~LAG.MPID.SUB.SELL.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.BUY.DVOL+LAG.EXE.BUY.DVOL+LAG.DEPTH.BUY.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN
    model2<-SUB.BUY.DVOL~LAG.MPID.SUB.SELL.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.BUY.DVOL+LAG.EXE.BUY.DVOL+LAG.DEPTH.BUY.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN+DUMMIES  
  }
  if(jj==15){
    l.sub.sell<-depvarlags
    model<-SUB.SELL.DVOL~LAG.MPID.SUB.BUY.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.SELL.DVOL+LAG.EXE.SELL.DVOL+LAG.DEPTH.SELL.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN
    model2<-SUB.SELL.DVOL~LAG.MPID.SUB.BUY.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.SELL.DVOL+LAG.EXE.SELL.DVOL+LAG.DEPTH.SELL.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN+DUMMIES  
  }
  ##EXECUTIONVOLUME
  if(jj==16){
    l.exe<-depvarlags
    model<-EXE.ALL.DVOL~LAG.MPID.SUB.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.ALL.DVOL+LAG.EXE.ALL.DVOL+LAG.DEPTH.TOTAL.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN
    model2<-EXE.ALL.DVOL~LAG.MPID.SUB.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.ALL.DVOL+LAG.EXE.ALL.DVOL+LAG.DEPTH.TOTAL.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN+DUMMIES  
  }
  if(jj==17){
    l.exe.buy<-depvarlags
    model<-EXE.BUY.DVOL~LAG.MPID.SUB.BUY.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.BUY.DVOL+LAG.EXE.BUY.DVOL+LAG.DEPTH.BUY.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN
    model2<-EXE.BUY.DVOL~LAG.MPID.SUB.BUY.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.BUY.DVOL+LAG.EXE.BUY.DVOL+LAG.DEPTH.BUY.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN+DUMMIES  
  }
  if(jj==18){
    l.exe.sell<-depvarlags
    model<-EXE.SELL.DVOL~LAG.MPID.SUB.SELL.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.SELL.DVOL+LAG.EXE.SELL.DVOL+LAG.DEPTH.SELL.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN
    model2<-EXE.SELL.DVOL~LAG.MPID.SUB.SELL.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.SELL.DVOL+LAG.EXE.SELL.DVOL+LAG.DEPTH.SELL.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN+DUMMIES  
  }
  if(jj==19){
    l.exe.buy<-depvarlags
    model<-EXE.BUY.DVOL~LAG.MPID.SUB.SELL.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.BUY.DVOL+LAG.EXE.BUY.DVOL+LAG.DEPTH.BUY.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN
    model2<-EXE.BUY.DVOL~LAG.MPID.SUB.SELL.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.BUY.DVOL+LAG.EXE.BUY.DVOL+LAG.DEPTH.BUY.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN+DUMMIES  
  }
  if(jj==20){
    l.exe.sell<-depvarlags
    model<-EXE.SELL.DVOL~LAG.MPID.SUB.BUY.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.SELL.DVOL+LAG.EXE.SELL.DVOL+LAG.DEPTH.SELL.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN
    model2<-EXE.SELL.DVOL~LAG.MPID.SUB.BUY.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.SELL.DVOL+LAG.EXE.SELL.DVOL+LAG.DEPTH.SELL.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN+DUMMIES
  }  
  ####DEPTH
  if(jj==21){
    l.d<-depvarlags
    model<-DEPTH.TOTAL.DVOL~LAG.MPID.SUB.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.ALL.DVOL+LAG.EXE.ALL.DVOL+LAG.DEPTH.TOTAL.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN
    model2<-DEPTH.TOTAL.DVOL~LAG.MPID.SUB.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.ALL.DVOL+LAG.EXE.ALL.DVOL+LAG.DEPTH.TOTAL.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN+DUMMIES  
  }
  if(jj==22){
    l.d.buy<-depvarlags
    model<-DEPTH.BUY.DVOL~LAG.MPID.SUB.BUY.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.BUY.DVOL+LAG.EXE.BUY.DVOL+LAG.DEPTH.BUY.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN
    model2<-DEPTH.BUY.DVOL~LAG.MPID.SUB.BUY.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.BUY.DVOL+LAG.EXE.BUY.DVOL+LAG.DEPTH.BUY.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN+DUMMIES  
  }
  if(jj==23){
    l.d.sell<-depvarlags
    model<-DEPTH.SELL.DVOL~LAG.MPID.SUB.SELL.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.SELL.DVOL+LAG.EXE.SELL.DVOL+LAG.DEPTH.SELL.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN
    model2<-DEPTH.SELL.DVOL~LAG.MPID.SUB.SELL.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.SELL.DVOL+LAG.EXE.SELL.DVOL+LAG.DEPTH.SELL.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN+DUMMIES  
  }
  if(jj==24){
    l.d.buy<-depvarlags
    model<-DEPTH.BUY.DVOL~LAG.MPID.SUB.SELL.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.BUY.DVOL+LAG.EXE.BUY.DVOL+LAG.DEPTH.BUY.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN
    model2<-DEPTH.BUY.DVOL~LAG.MPID.SUB.SELL.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.BUY.DVOL+LAG.EXE.BUY.DVOL+LAG.DEPTH.BUY.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN+DUMMIES  
  }
  if(jj==25){
    l.d.sell<-depvarlags
    model<-DEPTH.SELL.DVOL~LAG.MPID.SUB.BUY.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.SELL.DVOL+LAG.EXE.SELL.DVOL+LAG.DEPTH.SELL.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN
    model2<-DEPTH.SELL.DVOL~LAG.MPID.SUB.BUY.RATIO+LAG.AGGR.REL.TOTAL+LAG.ORSZ.DVOL.TOTAL+LAG.AGGR.REL.MPID+LAG.ORSZ.DVOL.MPID+LAG.VOL.GRID.SHORT.PRE+LAG.RELSPR+LAG.SUB.SELL.DVOL+LAG.EXE.SELL.DVOL+LAG.DEPTH.SELL.DVOL+abs(LAG.RELDPR.MID)+LAG.NEG.DUMMY+(abs(LAG.RELDPR.MID)*LAG.NEG.DUMMY)+OPEN+DUMMIES
  }  
  
  filename<-paste(mypath,"Results/", folder, "/Post_Regression_Ratio_",w,"sec_",jj,".txt",sep="")
  cat(paste("\n \n","Firm-Level Regression #", jj, ", Dep Var: ",all.vars(model)[2],"\n \n",sep=""),file=filename,append=FALSE)
  
  ii<-1
  
  mydata_temp<-mydata[mydata$TICK==ii,]
  
  SUB.ALL.NUM<-Lag(mydata_temp$SUB.ALL.NUM,k=1:mpidlags)
  SUB.BUY.NUM<-Lag(mydata_temp$SUB.BUY.NUM,k=1:mpidlags)
  SUB.SELL.NUM<-Lag(mydata_temp$SUB.SELL.NUM,k=1:mpidlags)
  
  MPID.SUB.RATIO<-mydata_temp$MPID.SUB.RATIO
  MPID.SUB.BUY.RATIO<-mydata_temp$MPID.SUB.BUY.RATIO
  MPID.SUB.SELL.RATIO<-mydata_temp$MPID.SUB.BUY.RATIO
  
  LAG.MPID.SUB.RATIO<-Lag(mydata_temp$MPID.SUB.RATIO,k=1:mpidlags)
  LAG.MPID.SUB.BUY.RATIO<-Lag(mydata_temp$MPID.SUB.BUY.RATIO,k=1:mpidlags)
  LAG.MPID.SUB.SELL.RATIO<-Lag(mydata_temp$MPID.SUB.BUY.RATIO,k=1:mpidlags)
  
  AGGR.REL.MPID<-mydata_temp$AGGR.REL.MPID
  ORSZ.DVOL.MPID<-mydata_temp$ORSZ.DVOL.MPID
  AGGR.REL.TOTAL<-mydata_temp$AGGR.REL.TOTAL
  ORSZ.DVOL.TOTAL<-mydata_temp$ORSZ.DVOL.TOTAL
  VOL.GRID.SHORT.PRE<-mydata_temp$VOL.GRID.SHORT.PRE
  VOL.GRID.LONG.PRE<-mydata_temp$VOL.GRID.LONG.PRE
  VOL.GRID.SHORT.POST<-mydata_temp$VOL.GRID.SHORT.POST
  VOL.GRID.LONG.POST<-mydata_temp$VOL.GRID.LONG.POST
  VOL.SD<-mydata_temp$VOL.SD 
  RELSPR<-mydata_temp$RELSPR
  RELEFFSPR.EXE.REP<-mydata_temp$RELEFFSPR.EXE.REP
  RELRLZSPR.EXE.REP.PRE<-mydata_temp$RELRLZSPR.EXE.REP.PRE
  RELRLZSPR.EXE.REP.POST<-mydata_temp$RELRLZSPR.EXE.REP.POST
  SUB.ALL.DVOL<-mydata_temp$SUB.ALL.DVOL
  EXE.ALL.DVOL<-mydata_temp$EXE.ALL.DVOL
  SUB.BUY.DVOL<-mydata_temp$SUB.BUY.DVOL
  EXE.BUY.DVOL<-mydata_temp$EXE.BUY.DVOL
  SUB.SELL.DVOL<-mydata_temp$SUB.SELL.DVOL
  EXE.SELL.DVOL<-mydata_temp$EXE.SELL.DVOL
  DEPTH.TOTAL.DVOL<-mydata_temp$DEPTH.TOTAL.DVOL
  DEPTH.BUY.DVOL<-mydata_temp$DEPTH.TOTAL.DVOL
  DEPTH.SELL.DVOL<-mydata_temp$DEPTH.TOTAL.DVOL
  RELDPR.MID<-mydata_temp$RELDPR.MID
  NEG.DUMMY<-mydata_temp$NEG.DUMMY
  HASBROUCK.MAX<-mydata_temp$HASBROUCK.MAX
  
  LAG.AGGR.REL.MPID<-Lag(mydata_temp$AGGR.REL.MPID,k=1)
  LAG.ORSZ.DVOL.MPID<-Lag(mydata_temp$ORSZ.DVOL.MPID,k=1)
  LAG.AGGR.REL.TOTAL<-Lag(mydata_temp$AGGR.REL.TOTAL,k=1)
  LAG.ORSZ.DVOL.TOTAL<-Lag(mydata_temp$ORSZ.DVOL.TOTAL,k=1)
  LAG.VOL.GRID.SHORT.PRE<-Lag(mydata_temp$VOL.GRID.SHORT.PRE,k=1:l.vol1)
  LAG.VOL.GRID.LONG.PRE<-Lag(mydata_temp$VOL.GRID.LONG.PRE,k=1:l.vol3)
  LAG.VOL.GRID.SHORT.POST<-Lag(mydata_temp$VOL.GRID.SHORT.POST,k=1:l.vol1)
  LAG.VOL.GRID.LONG.POST<-Lag(mydata_temp$VOL.GRID.LONG.POST,k=1:l.vol3)
  LAG.VOL.SD<-Lag(mydata_temp$VOL.SD,k=1:l.vol2)
  LAG.RELSPR<-Lag(mydata_temp$RELSPR,k=1:l.relspr1)
  LAG.RELEFFSPR.EXE.REP<-Lag(mydata_temp$RELEFFSPR.EXE.REP,k=1:l.relspr2)
  LAG.RELRLZSPR.EXE.REP.PRE<-Lag(mydata_temp$RELRLZSPR.EXE.REP.PRE,k=1:l.relspr3)
  LAG.RELRLZSPR.EXE.REP.POST<-Lag(mydata_temp$RELRLZSPR.EXE.REP.POST,k=1:l.relspr3)
  LAG.SUB.ALL.DVOL<-Lag(mydata_temp$SUB.ALL.DVOL,k=1:l.sub)
  LAG.EXE.ALL.DVOL<-Lag(mydata_temp$EXE.ALL.DVOL,k=1:l.exe)
  LAG.SUB.BUY.DVOL<-Lag(mydata_temp$SUB.BUY.DVOL,k=1:l.sub.buy)
  LAG.EXE.BUY.DVOL<-Lag(mydata_temp$EXE.BUY.DVOL,k=1:l.exe.buy)
  LAG.SUB.SELL.DVOL<-Lag(mydata_temp$SUB.SELL.DVOL,k=1:l.sub.sell)
  LAG.EXE.SELL.DVOL<-Lag(mydata_temp$EXE.SELL.DVOL,k=1:l.exe.sell)
  LAG.DEPTH.TOTAL.DVOL<-Lag(mydata_temp$DEPTH.TOTAL.DVOL,k=1:l.d)
  LAG.DEPTH.BUY.DVOL<-Lag(mydata_temp$DEPTH.TOTAL.DVOL,k=1:l.d.buy)
  LAG.DEPTH.SELL.DVOL<-Lag(mydata_temp$DEPTH.TOTAL.DVOL,k=1:l.d.sell)
  LAG.RELDPR.MID<-Lag(mydata_temp$RELDPR.MID,k=1)
  LAG.NEG.DUMMY<-Lag(mydata_temp$NEG.DUMMY,k=1)
  LAG.HASBROUCK.MAX<-Lag(mydata_temp$HASBROUCK.MAX,k=1)
  
  OPEN<-mydata_temp$OPEN
  CLOSE<-mydata_temp$CLOSE
  
  DUMMIES<-DDUMMIES[mydata$TICK==ii,]
  fit<-lm(model2)
  s<-summary(fit)
  
  rr<-nrow(s$coefficients)
  t<-length(ticks)
  
  formatlab<-matrix(NA,t*rr,4*4)
  colnames(formatlab)<-c("COEF1","SD.ERR1","T.STAT1","P.VAL1","COEF2","SD.ERR2","T.STAT2","P.VAL2","COEF3","SD.ERR3","T.STAT3","P.VAL3","COEF4","SD.ERR4","T.STAT4","P.VAL4")
  rowns<-matrix(NA,t*rr)
  
  for (ii in 1:length(ticks)){   
    
    mydata_temp<-mydata[mydata$TICK==ii,]
    
    SUB.ALL.NUM<-Lag(mydata_temp$SUB.ALL.NUM,k=1:mpidlags)
    SUB.BUY.NUM<-Lag(mydata_temp$SUB.BUY.NUM,k=1:mpidlags)
    SUB.SELL.NUM<-Lag(mydata_temp$SUB.SELL.NUM,k=1:mpidlags)
    
    MPID.SUB.RATIO<-mydata_temp$MPID.SUB.RATIO
    MPID.SUB.BUY.RATIO<-mydata_temp$MPID.SUB.BUY.RATIO
    MPID.SUB.SELL.RATIO<-mydata_temp$MPID.SUB.BUY.RATIO
    
    LAG.MPID.SUB.RATIO<-Lag(mydata_temp$MPID.SUB.RATIO,k=1:mpidlags)
    LAG.MPID.SUB.BUY.RATIO<-Lag(mydata_temp$MPID.SUB.BUY.RATIO,k=1:mpidlags)
    LAG.MPID.SUB.SELL.RATIO<-Lag(mydata_temp$MPID.SUB.BUY.RATIO,k=1:mpidlags)
    
    AGGR.REL.MPID<-mydata_temp$AGGR.REL.MPID
    ORSZ.DVOL.MPID<-mydata_temp$ORSZ.DVOL.MPID
    AGGR.REL.TOTAL<-mydata_temp$AGGR.REL.TOTAL
    ORSZ.DVOL.TOTAL<-mydata_temp$ORSZ.DVOL.TOTAL
    VOL.GRID.SHORT.PRE<-mydata_temp$VOL.GRID.SHORT.PRE
    VOL.GRID.LONG.PRE<-mydata_temp$VOL.GRID.LONG.PRE
    VOL.GRID.SHORT.POST<-mydata_temp$VOL.GRID.SHORT.POST
    VOL.GRID.LONG.POST<-mydata_temp$VOL.GRID.LONG.POST
    VOL.SD<-mydata_temp$VOL.SD 
    RELSPR<-mydata_temp$RELSPR
    RELEFFSPR.EXE.REP<-mydata_temp$RELEFFSPR.EXE.REP
    RELRLZSPR.EXE.REP.PRE<-mydata_temp$RELRLZSPR.EXE.REP.PRE
    RELRLZSPR.EXE.REP.POST<-mydata_temp$RELRLZSPR.EXE.REP.POST
    SUB.ALL.DVOL<-mydata_temp$SUB.ALL.DVOL
    EXE.ALL.DVOL<-mydata_temp$EXE.ALL.DVOL
    SUB.BUY.DVOL<-mydata_temp$SUB.BUY.DVOL
    EXE.BUY.DVOL<-mydata_temp$EXE.BUY.DVOL
    SUB.SELL.DVOL<-mydata_temp$SUB.SELL.DVOL
    EXE.SELL.DVOL<-mydata_temp$EXE.SELL.DVOL
    DEPTH.TOTAL.DVOL<-mydata_temp$DEPTH.TOTAL.DVOL
    DEPTH.BUY.DVOL<-mydata_temp$DEPTH.TOTAL.DVOL
    DEPTH.SELL.DVOL<-mydata_temp$DEPTH.TOTAL.DVOL
    RELDPR.MID<-mydata_temp$RELDPR.MID
    NEG.DUMMY<-mydata_temp$NEG.DUMMY
    HASBROUCK.MAX<-mydata_temp$HASBROUCK.MAX
    
    LAG.AGGR.REL.MPID<-Lag(mydata_temp$AGGR.REL.MPID,k=1)
    LAG.ORSZ.DVOL.MPID<-Lag(mydata_temp$ORSZ.DVOL.MPID,k=1)
    LAG.AGGR.REL.TOTAL<-Lag(mydata_temp$AGGR.REL.TOTAL,k=1)
    LAG.ORSZ.DVOL.TOTAL<-Lag(mydata_temp$ORSZ.DVOL.TOTAL,k=1)
    LAG.VOL.GRID.SHORT.PRE<-Lag(mydata_temp$VOL.GRID.SHORT.PRE,k=1:l.vol1)
    LAG.VOL.GRID.LONG.PRE<-Lag(mydata_temp$VOL.GRID.LONG.PRE,k=1:l.vol3)
    LAG.VOL.GRID.SHORT.POST<-Lag(mydata_temp$VOL.GRID.SHORT.POST,k=1:l.vol1)
    LAG.VOL.GRID.LONG.POST<-Lag(mydata_temp$VOL.GRID.LONG.POST,k=1:l.vol3)
    LAG.VOL.SD<-Lag(mydata_temp$VOL.SD,k=1:l.vol2)
    LAG.RELSPR<-Lag(mydata_temp$RELSPR,k=1:l.relspr1)
    LAG.RELEFFSPR.EXE.REP<-Lag(mydata_temp$RELEFFSPR.EXE.REP,k=1:l.relspr2)
    LAG.RELRLZSPR.EXE.REP.PRE<-Lag(mydata_temp$RELRLZSPR.EXE.REP.PRE,k=1:l.relspr3)
    LAG.RELRLZSPR.EXE.REP.POST<-Lag(mydata_temp$RELRLZSPR.EXE.REP.POST,k=1:l.relspr3)
    LAG.SUB.ALL.DVOL<-Lag(mydata_temp$SUB.ALL.DVOL,k=1:l.sub)
    LAG.EXE.ALL.DVOL<-Lag(mydata_temp$EXE.ALL.DVOL,k=1:l.exe)
    LAG.SUB.BUY.DVOL<-Lag(mydata_temp$SUB.BUY.DVOL,k=1:l.sub.buy)
    LAG.EXE.BUY.DVOL<-Lag(mydata_temp$EXE.BUY.DVOL,k=1:l.exe.buy)
    LAG.SUB.SELL.DVOL<-Lag(mydata_temp$SUB.SELL.DVOL,k=1:l.sub.sell)
    LAG.EXE.SELL.DVOL<-Lag(mydata_temp$EXE.SELL.DVOL,k=1:l.exe.sell)
    LAG.DEPTH.TOTAL.DVOL<-Lag(mydata_temp$DEPTH.TOTAL.DVOL,k=1:l.d)
    LAG.DEPTH.BUY.DVOL<-Lag(mydata_temp$DEPTH.TOTAL.DVOL,k=1:l.d.buy)
    LAG.DEPTH.SELL.DVOL<-Lag(mydata_temp$DEPTH.TOTAL.DVOL,k=1:l.d.sell)
    LAG.RELDPR.MID<-Lag(mydata_temp$RELDPR.MID,k=1)
    LAG.NEG.DUMMY<-Lag(mydata_temp$NEG.DUMMY,k=1)
    LAG.HASBROUCK.MAX<-Lag(mydata_temp$HASBROUCK.MAX,k=1)
    
    OPEN<-mydata_temp$OPEN
    CLOSE<-mydata_temp$CLOSE  
    DUMMIES<-DDUMMIES[mydata$TICK==ii,]
    
    fit<-lm(model)
    s<-summary(fit)
    ss<-coeftest(fit, vcov=vcovHAC(fit))
    
    r<-length(names(fit$coefficients))
    formatlab[(rr*(ii-1)+1):(rr*(ii-1)+r),1:4]<-unname(s$coefficients)
    formatlab[(rr*(ii-1)+1):(rr*(ii-1)+r),5:8]<-unname(ss)
    
    cat(paste("\n \n",ticks[ii], " Regression #", jj, ", Dep Var: ",all.vars(model)[2],"\n \n",sep=""),file=filename,append=TRUE)
    capture.output(s, file=filename,append=TRUE)
    capture.output(ss, file=filename,append=T)
    
    fit<-lm(model2)
    s<-summary(fit)
    ss<-coeftest(fit, vcov=vcovHAC(fit))
    
    cat(paste("\n \n",ticks[ii], " Regression + Day Dummies #", jj, ", Dep Var: ",all.vars(model)[2],"\n \n",sep=""),file=filename,append=TRUE)
    capture.output(s, file=filename,append=TRUE)
    capture.output(ss, file=filename,append=T)
    
    formatlab[(rr*(ii-1)+1):(rr*(ii-1)+rr),9:12]<-unname(s$coefficients)
    formatlab[(rr*(ii-1)+1):(rr*(ii-1)+rr),13:16]<-unname(ss)
    row<-paste(ticks[ii],".",names(fit$coefficients),sep="")
    rowns[(rr*(ii-1)+1):(rr*(ii-1)+rr)]<-row  
    
    v<-unname(residuals(fit))    
    #pdf(paste(mypath,"/Graphs/", folder, "_Errors/",ticks[ii],"_IndividualErrors_Ratio_",jj,"_",w,"sec.pdf",sep=""))
    #plot(v,type = "l",ylab="Panel Errors",main=ticks[ii],xlab="Date")
    #dev.off()
    
    test1<-adf.test(v)
    test2<-PP.test(v,lshort=FALSE)
    test3<-kpss.test(v,null=c("Level"))
    test4<-kpss.test(v,null=c("Trend"))
    test5<-dwtest(model,alternative="two.sided")
    #test5<-Box.test(v, lag=(2*mpidlags), type = c("Ljung-Box"),fitdf=(mpidlags))
    test6<-summary(lm(v[-length(v)]~v[-1]))
    #test7<-Box.test(v, lag=(2*lags), type = c("Ljung-Box"),fitdf=(lags))
    
    capture.output(test5, file=filename,append=T)
    
    stationary[jj,(1+(numtest*2)*(ii-1)):((numtest*2)+(numtest*2)*(ii-1))]<-c(test1$statistic,test1$p.value,test2$statistic,test2$p.value,
                                                                             test3$statistic,test3$p.value,test4$statistic,test4$p.value,
                                                                             test5$statistic,test5$p.value,
                                                                             test6$coefficients[2,3],test6$coefficients[2,4])
    #filename2<-paste(mypath,"Results/", folder, "/Regression_Ratio_",ticks[ii],"_",w,"sec_",jj,".rda",sep="")
    #save(fit,file=filename2)
    
  }
  
  
  rownames(formatlab)<-rowns
  write.xlsx(formatlab, paste(mypath,"Results/", folder, "/Post_Regression_Ratio_",w,"sec_",jj,".xlsx",sep=""))
  
  #rownames(formatlab)<-rowns
  #write.table(formatlab, paste(mypath,"Results/", folder, "/Summary_Post_Regression_Ratio_",w,"sec_",jj,".txt",sep=""))
  
}

  colnames(stationary)<-c('AAPL',vector(mode="numeric",length=(2*numtest-1)),'CSCO',vector(mode="numeric",length=(2*numtest-1)),
                          'EBAY',vector(mode="numeric",length=(2*numtest-1)),'FB',vector(mode="numeric",length=(2*numtest-1)),
                          'GOOG',vector(mode="numeric",length=(2*numtest-1)),
                          'INTC',vector(mode="numeric",length=(2*numtest-1)),'MSFT',vector(mode="numeric",length=(2*numtest-1)),
                          'YHOO',vector(mode="numeric",length=(2*numtest-1)))

  filename<-paste(mypath,"Results/", folder, "/POST_INDIVIDUALERRORS_",mpidlags,"Lags_",w,"sec.xlsx",sep="")
  write.xlsx(stationary,file=filename,append=FALSE) 
  
  #filename<-paste(mypath,"Results/", folder, "/POST_INDIVIDUALERRORS_",mpidlags,"Lags_",w,"sec_",jj,".txt",sep="")
  #write.table(stationary,file=filename,append=FALSE) 


# CLUSTER ----------------------------------------------------------------

#ID <- as.integer(Sys.getenv("SGE_TASK_ID"))
#s <- individual(ID)



