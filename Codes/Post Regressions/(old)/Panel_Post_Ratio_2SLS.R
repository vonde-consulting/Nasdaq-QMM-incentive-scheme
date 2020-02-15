rm(list=ls(all=TRUE))
library(car)
library(plm)
library(tseries)
library(xlsx)
library(lmtest)
library(sandwich)
library(lfe)

#options(error=NULL)
#options(error=recover)

#panel <- function(jj){

head<-"C:/DatiLocali/"
windows<-c(120,60,30,10)
ticks<-c("AAPL","CSCO","EBAY","FB","GOOG","INTC","MSFT","YHOO")
#folder<-"Panel_Post_Ratio_2SLS"
folder<-"Panel_Post_Ratio_TMBR_2SLS"
mypath<-paste(head,"Dropbox/Projects/Trader Anonymity/Data/",sep="")

#for (k in 1:length(windows)){

k<-3
w<-windows[k]

##SAVE FILE
filename<-paste(mypath,"DATA_TMBR_INST_NOAAPL_",w,"sec.Rds",sep="")
load(file=filename)
ticks<-ticks [-1]

####PANEL REGRESSIONS

numtests<-31

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
  ##PRICE CHANGES
  if (jj==29){    
    
    reg1<-MPID.SUB.RATIO~lag(MPID.SUB.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+OPEN+lag(HASBROUCK.MAX,k=1)+
      lag(SUB.ALL.NUM.INST,k=1:mpidlags)+lag(RELDPR.MID,k=1:depvarlags)
    reg2<-SUB.ALL.NUM~lag(MPID.SUB.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+OPEN+lag(HASBROUCK.MAX,k=1)+
      lag(SUB.ALL.NUM.INST,k=1:mpidlags)+lag(RELDPR.MID,k=1:depvarlags)   
    
    model<-RELDPR.MID~lag(MPID.SUB.HAT,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+lag(HASBROUCK.MAX,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.ALL.DVOL,k=1)+lag(EXE.ALL.DVOL,k=1)+
      lag(DEPTH.TOTAL.DVOL,k=1)+OPEN+lag(SUB.NUM.HAT,k=1:mpidlags)+lag(RELDPR.MID,k=1:depvarlags)
    
  }  
  if (jj==30){  
    
    reg1<-MPID.SUB.BUY.RATIO~lag(MPID.SUB.BUY.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1)+OPEN+lag(HASBROUCK.MAX,k=1)+
      lag(SUB.BUY.NUM.INST,k=1:mpidlags)+lag(RELDPR.MID,k=1:depvarlags)
    reg2<-RELDPR.MID~lag(MPID.SUB.BUY.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1)+OPEN+lag(HASBROUCK.MAX,k=1)+
      lag(SUB.BUY.NUM.INST,k=1:mpidlags)+lag(RELDPR.MID,k=1:depvarlags)
    
    model<-RELDPR.MID~lag(MPID.SUB.HAT,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.BUY.DVOL,k=1)+lag(EXE.BUY.DVOL,k=1)+
      lag(DEPTH.BUY.DVOL,k=1)+lag(HASBROUCK.MAX,k=1)+
      OPEN+lag(SUB.NUM.HAT,k=1:mpidlags)+lag(RELDPR.MID,k=1:depvarlags)    
  }    
  if (jj==31){    
    
    reg1<-MPID.SUB.SELL.RATIO~lag(MPID.SUB.SELL.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1)+OPEN+lag(HASBROUCK.MAX,k=1)+
      lag(SUB.SELL.NUM.INST,k=1:mpidlags)+lag(RELDPR.MID,k=1:depvarlags)
    reg2<-RELDPR.MID~lag(MPID.SUB.SELL.INST,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1)+OPEN+lag(HASBROUCK.MAX,k=1)+lag(SUB.SELL.NUM.INST,k=1:mpidlags)+lag(RELDPR.MID,k=1:depvarlags)
    
    model<-RELDPR.MID~lag(MPID.SUB.HAT,k=c(1:mpidlags))+lag(AGGR.REL.MPID,k=1)+lag(ORSZ.DVOL.MPID,k=1)+
      lag(VOL.GRID.SHORT.PRE,k=1)+lag(RELSPR,k=1)+lag(SUB.SELL.DVOL,k=1)+lag(EXE.SELL.DVOL,k=1)+
      lag(DEPTH.SELL.DVOL,k=1)+lag(HASBROUCK.MAX,k=1)+
      OPEN+lag(SUB.NUM.HAT,k=1:mpidlags)+lag(RELDPR.MID,k=1:depvarlags)
  }  
  
  
  filename<-paste(mypath,"Results/", folder, "/Panel_Regression_Post_",w,"sec_",jj,".txt",sep="")
  
  
  ##pooled OLS regression, stage 1
  
  fit<-plm(reg1,data=mydata,model="pooling",index=c("TICK","TIMEPOINT")); k<-length(names(fit$coefficients))
  
  #ftest
  
  if (jj%in%c(1,4,5,6,9,10,11,16,21,26,29)==1){
    m<-update(reg1,.~.-lag(MPID.SUB.INST,k=c(1:mpidlags))-lag(SUB.ALL.NUM.INST,k=1:mpidlags))}
  if (jj%in%c(2,7,12,15,17,20,22,25,27,30)==1){
    m<-update(reg1,.~.-lag(MPID.SUB.BUY.INST,k=c(1:mpidlags))-lag(SUB.BUY.NUM.INST,k=1:mpidlags))}
  if (jj%in%c(3,8,13,14,18,19,23,24,28,31)==1){
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

filename<-paste(mypath,"Results/", folder, "/POST_PANELERRORS_STATIONARITY_BYFIRM_",w,"sec.txt",sep="")
write.table(stationary,file=filename,append=FALSE) 

filename<-paste(mypath,"Results/", folder, "/INST_DIAGNOSTICS_BYFIRM_",w,"sec.txt",sep="")
write.table(diagnostics,file=filename,append=FALSE) 


#}


