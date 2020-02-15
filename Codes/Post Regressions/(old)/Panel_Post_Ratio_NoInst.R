rm(list=ls(all=TRUE))
library(car)
library(plm)
library(tseries)
library(openxlsx)
library(lmtest)
library(sandwich)
library(lfe)

head<-"C:/DatiLocali/"
windows<-c(120,60,30,10)
ticks<-c("AAPL","CSCO","EBAY","FB","GOOG","INTC","MSFT","YHOO")
folder<-"Panel_Post_Ratio_NoInst"
mypath<-paste(head,"Dropbox/Projects/Trader Anonymity/Data/",sep="")

k<-3
w<-windows[k]

##SAVE FILE
filename<-paste(mypath,"DATA_INST_",w,"sec.Rds",sep="")
load(file=filename)
mydata<-pdata.frame(mydata,index=c("TICK","TIMEPOINT"))

####PANEL REGRESSIONS

numtests<-31

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

  if (jj==29){    
      
      model<-RELDPR.MID~lag(MPID.SUB.RATIO,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
        lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
        lag(DEPTH.TOTAL.DVOL,k=1)+lag(HASBROUCK.MAX,k=1)+OPEN+lag(SUB.ALL.NUM,k=1:mpidlags)+lag(RELDPR.MID,k=1:depvarlags)
      model2<-RELDPR.MID~lag(MPID.SUB.RATIO,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
        lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
        lag(DEPTH.TOTAL.DVOL,k=1)+lag(HASBROUCK.MAX,k=1)+OPEN+DUMMIES+lag(SUB.ALL.NUM,k=1:mpidlags)+lag(RELDPR.MID,k=1:depvarlags)
            
          }  
    if (jj==30){  
      
      model<-RELDPR.MID~lag(MPID.SUB.BUY.RATIO,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
        lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1)+
        lag(DEPTH.TOTAL.DVOL,k=1)+lag(HASBROUCK.MAX,k=1)+OPEN+lag(SUB.BUY.NUM,k=1:mpidlags)+lag(RELDPR.MID,k=1:depvarlags)
      model2<-RELDPR.MID~lag(MPID.SUB.RATIO,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
        lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1)+
        lag(DEPTH.TOTAL.DVOL,k=1)+lag(HASBROUCK.MAX,k=1)+OPEN+DUMMIES+lag(SUB.BUY.NUM,k=1:mpidlags)+lag(RELDPR.MID,k=1:depvarlags)
      
    }    
    if (jj==31){    
      
      model<-RELDPR.MID~lag(MPID.SUB.SELL.RATIO,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
        lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1)+
        lag(DEPTH.TOTAL.DVOL,k=1)+lag(HASBROUCK.MAX,k=1)+OPEN+lag(SUB.SELL.NUM,k=1:mpidlags)+lag(RELDPR.MID,k=1:depvarlags)
      model2<-RELDPR.MID~lag(MPID.SUB.RATIO,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
        lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1)+
        lag(DEPTH.TOTAL.DVOL,k=1)+lag(HASBROUCK.MAX,k=1)+OPEN+DUMMIES+lag(SUB.SELL.NUM,k=1:mpidlags)+lag(RELDPR.MID,k=1:depvarlags)
    }  
    
        
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

filename<-paste(mypath,"Results/", folder, "/POST_PANELERRORS_STATIONARITY_BYFIRM_",w,"sec.txt",sep="")
write.table(stationary,file=filename,append=FALSE) 


#}


