rm(list=ls(all=TRUE))
library(car)
library(plm)
library(tseries)
library(xlsx)
library(lmtest)
library(sandwich)
library(quantmod)

#panel <- function(jj){

windows<-c(120,60,30,10)
ticks<-c("AAPL","CSCO","EBAY","FB","GOOG","INTC","MSFT","YHOO")
folder<-"Individual_Ratio"

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

#nms<-c("MPID.SUB.RATIO","MPID.SUB.BUY.RATIO","MPID.SUB.SELL.RATIO","RELSPR","RELEFFSPR.EXE.REP","RELRLZSPR.EXE.REP.PRE",
#       "VOL.GRID.SHORT.PRE","VOL.SD","VOL.GRID.LONG.PRE","AGGR.REL.TOTAL","ORSZ.DVOL.TOTAL","AGGR.REL.MPID",
#       "ORSZ.DVOL.MPID","SUB.ALL.DVOL","EXE.ALL.DVOL","SUB.BUY.DVOL","EXE.BUY.DVOL","SUB.SELL.DVOL","EXE.SELL.DVOL",
#       "DEPTH.TOTAL.DVOL","DEPTH.BUY.DVOL","DEPTH.SELL.DVOL","RELDPR.MID","HASBROUCK.MAX")
#numtest<-4
#stationary<-matrix(NA,length(nms),numtest*2*length(ticks))

#for (kk in 1:length(nms)){
  
  #kk<-1
#  for (vv in 1:length(ticks)){
    
#    q<-which(colnames(mydata)==nms[kk])
#    qq<-which(mydata$TICK==vv)
#    v<-mydata[qq,q]
    
#    pdf(paste(mypath,"/Graphs/",ticks[vv],"_",nms[kk],"_Ratio_",w,"sec.pdf",sep=""))
#    plot(v,type = "l",ylab=nms[kk],main=ticks[vv],xlab="Date")
#    dev.off()
    
#    qv<-which(!is.na(v))
#    v<-v[qv]
    
#    test1<-adf.test(v)
#    test2<-PP.test(v,lshort=FALSE)
#    test3<-kpss.test(v,null=c("Level"))
#    test4<-kpss.test(v,null=c("Trend"))
    
#   stationary[kk,(1+(numtest*2)*(vv-1)):((numtest*2)+(numtest*2)*(vv-1))]<-c(test1$statistic,test1$p.value,test2$statistic,test2$p.value,
#                                                                              test3$statistic,test3$p.value,test4$statistic,test4$p.value)
#  }
#}

#rownames(stationary)<-nms
#colnames(stationary)<-c('AAPL',vector(mode="numeric",length=(2*numtest-1)),'CSCO',vector(mode="numeric",length=(2*numtest-1)),
#                        'EBAY',vector(mode="numeric",length=(2*numtest-1)),'FB',vector(mode="numeric",length=(2*numtest-1)),
#                        'GOOG',vector(mode="numeric",length=(2*numtest-1)),
#                        'INTC',vector(mode="numeric",length=(2*numtest-1)),'MSFT',vector(mode="numeric",length=(2*numtest-1)),
#                        'YHOO',vector(mode="numeric",length=(2*numtest-1)))

#filename<-paste(mypath,"REGRESSOR_STATIONARITY_BYFIRM_",w,"sec.xlsx",sep="")
#write.xlsx(stationary,file=filename,append=FALSE) 

#v<-mydata_unst$MPID.SUB.RATIO
#v<-matrix(v,nrow=length(unique(mydata$TIMEPOINT)),ncol=length(ticks))
#v<-cbind(mydata$DAY[mydata$TICK==1],mydata$TIMEPOINT[mydata$TICK==1],v)
#filename<-paste(mypath,"MPID_SUB_RATIO_",w,"sec.xlsx",sep="")
#write.xlsx(v,file=filename,append=FALSE) 

#v<-mydata_unst$MPID.SUB.BUY.RATIO
#v<-matrix(v,nrow=length(unique(mydata$TIMEPOINT)),ncol=length(ticks))
#v<-cbind(mydata$DAY[mydata$TICK==1],mydata$TIMEPOINT[mydata$TICK==1],v)
#filename<-paste(mypath,"MPID_SUB_BUY_RATIO_",w,"sec.xlsx",sep="")
#write.xlsx(v,file=filename,append=FALSE) 

#v<-mydata_unst$MPID.SUB.SELL.RATIO
#v<-matrix(v,nrow=length(unique(mydata$TIMEPOINT)),ncol=length(ticks))
#v<-cbind(mydata$DAY[mydata$TICK==1],mydata$TIMEPOINT[mydata$TICK==1],v)
#filename<-paste(mypath,"MPID_SUB_SELL_RATIO_",w,"sec.xlsx",sep="")
#write.xlsx(v,file=filename,append=FALSE) 
  
  ##get min and max timepoints for lagging
  #ii<-1
  #mydata_temp<-mydata[mydata$TICK==ticks[ii],]
  #d<-unique(mydata_temp$DAY)
  #qmin<-matrix(NA,length(d))
  #qmax<-matrix(NA,length(d))
  
  #for (zz in 1:length(d)){
  #  mydata_ttemp<-mydata_temp[mydata_temp$DAY==d[zz],]  
  #  qmin[zz]<-min(mydata_ttemp$TIMEPOINT) 
  #  qmax[zz]<-max(mydata_ttemp$TIMEPOINT)
  #}
  
  ##RUN THROUGH DIFFERENT MODELS###################################

#  for (l in 1:20){

  numregs<-20
  numtest<-7
  stationary<-matrix(NA,numregs,numtest*2*length(ticks))
  ##########
  lags<-5
  #lags<-l
  ##########
  
  for (jj in 5:5){    #1:numregs
    
    if (jj==1){     #negative dummy
      model<-MPID.SUB.RATIO~LAG.MPID.SUB.RATIO+AGGR.REL.MPID+ORSZ.DVOL.MPID+AGGR.REL.TOTAL+ORSZ.DVOL.TOTAL+
        VOL.GRID.SHORT.PRE+RELSPR+SUB.ALL.DVOL+EXE.ALL.DVOL+
        DEPTH.TOTAL.DVOL+abs(RELDPR.MID)+NEG.DUMMY+
        (abs(RELDPR.MID)*NEG.DUMMY)+OPEN
    }
    if (jj==2){    #positive dummy
      model<-MPID.SUB.RATIO~LAG.MPID.SUB.RATIO+AGGR.REL.MPID+ORSZ.DVOL.MPID+AGGR.REL.TOTAL+ORSZ.DVOL.TOTAL+
        VOL.GRID.SHORT.PRE+RELSPR+SUB.ALL.DVOL+EXE.ALL.DVOL+
        DEPTH.TOTAL.DVOL+abs(RELDPR.MID)+POS.DUMMY+
        (abs(RELDPR.MID)*POS.DUMMY)+OPEN
    }      
    if (jj==3){    #negative dummy, buy side on buy side
      model<-MPID.SUB.BUY.RATIO~LAG.MPID.SUB.BUY.RATIO+AGGR.REL.MPID+ORSZ.DVOL.MPID+AGGR.REL.TOTAL+ORSZ.DVOL.TOTAL+
        VOL.GRID.SHORT.PRE+RELSPR+SUB.BUY.DVOL+EXE.BUY.DVOL+
        DEPTH.BUY.DVOL+abs(RELDPR.MID)+NEG.DUMMY+
        (abs(RELDPR.MID)*NEG.DUMMY)+OPEN
    }  
    if (jj==4){    #negative dummy, buy side on sell side
      model<-MPID.SUB.BUY.RATIO~LAG.MPID.SUB.BUY.RATIO+AGGR.REL.MPID+ORSZ.DVOL.MPID+AGGR.REL.TOTAL+ORSZ.DVOL.TOTAL+
        VOL.GRID.SHORT.PRE+RELSPR+SUB.SELL.DVOL+EXE.SELL.DVOL+
        DEPTH.SELL.DVOL+abs(RELDPR.MID)+NEG.DUMMY+
        (abs(RELDPR.MID)*NEG.DUMMY)+OPEN
    }              
    if (jj==5){    #positive dummy, buy side on buy side
      model<-MPID.SUB.BUY.RATIO~LAG.MPID.SUB.BUY.RATIO+AGGR.REL.MPID+ORSZ.DVOL.MPID+AGGR.REL.TOTAL+ORSZ.DVOL.TOTAL+
        VOL.GRID.SHORT.PRE+RELSPR+SUB.BUY.DVOL+EXE.BUY.DVOL+
        DEPTH.BUY.DVOL+abs(RELDPR.MID)+POS.DUMMY+
        (abs(RELDPR.MID)*POS.DUMMY)+OPEN
    }  
    if (jj==6){    #positive dummy, buy side on sell side
      model<-MPID.SUB.BUY.RATIO~LAG.MPID.SUB.BUY.RATIO+AGGR.REL.MPID+ORSZ.DVOL.MPID+AGGR.REL.TOTAL+ORSZ.DVOL.TOTAL+
        VOL.GRID.SHORT.PRE+RELSPR+SUB.SELL.DVOL+EXE.SELL.DVOL+
        DEPTH.SELL.DVOL+abs(RELDPR.MID)+POS.DUMMY+
        (abs(RELDPR.MID)*POS.DUMMY)+OPEN
    }         
    if (jj==7){    #negative dummy, sell side on sell side
      model<-MPID.SUB.SELL.RATIO~LAG.MPID.SUB.SELL.RATIO+AGGR.REL.MPID+ORSZ.DVOL.MPID+AGGR.REL.TOTAL+ORSZ.DVOL.TOTAL+
        VOL.GRID.SHORT.PRE+RELSPR+SUB.SELL.DVOL+EXE.SELL.DVOL+
        DEPTH.SELL.DVOL+abs(RELDPR.MID)+NEG.DUMMY+
        (abs(RELDPR.MID)*NEG.DUMMY)+OPEN
    }  
    if (jj==8){    #negative dummy, sell side on buy side
      model<-MPID.SUB.SELL.RATIO~LAG.MPID.SUB.SELL.RATIO+AGGR.REL.MPID+ORSZ.DVOL.MPID+AGGR.REL.TOTAL+ORSZ.DVOL.TOTAL+
        VOL.GRID.SHORT.PRE+RELSPR+SUB.BUY.DVOL+EXE.BUY.DVOL+
        DEPTH.BUY.DVOL+abs(RELDPR.MID)+NEG.DUMMY+
        (abs(RELDPR.MID)*NEG.DUMMY)+OPEN
    }              
    if (jj==9){    #positive dummy, sell side on sell side
      model<-MPID.SUB.SELL.RATIO~LAG.MPID.SUB.SELL.RATIO+AGGR.REL.MPID+ORSZ.DVOL.MPID+AGGR.REL.TOTAL+ORSZ.DVOL.TOTAL+
        VOL.GRID.SHORT.PRE+RELSPR+SUB.SELL.DVOL+EXE.SELL.DVOL+
        DEPTH.SELL.DVOL+abs(RELDPR.MID)+POS.DUMMY+
        (abs(RELDPR.MID)*POS.DUMMY)+OPEN
    }  
    if (jj==10){    #positive dummy, sell side on buy side
      model<-MPID.SUB.SELL.RATIO~LAG.MPID.SUB.SELL.RATIO+AGGR.REL.MPID+ORSZ.DVOL.MPID+AGGR.REL.TOTAL+ORSZ.DVOL.TOTAL+
        VOL.GRID.SHORT.PRE+RELSPR+SUB.BUY.DVOL+EXE.BUY.DVOL+
        DEPTH.BUY.DVOL+abs(RELDPR.MID)+POS.DUMMY+
        (abs(RELDPR.MID)*POS.DUMMY)+OPEN
    }        
    if (jj==11){     #relative spreads
      model<-MPID.SUB.RATIO~LAG.MPID.SUB.RATIO+AGGR.REL.MPID+ORSZ.DVOL.MPID+AGGR.REL.TOTAL+ORSZ.DVOL.TOTAL+
        VOL.GRID.SHORT.PRE+RELEFFSPR.EXE.REP+SUB.ALL.DVOL+EXE.ALL.DVOL+
        DEPTH.TOTAL.DVOL+abs(RELDPR.MID)+NEG.DUMMY+
        (abs(RELDPR.MID)*NEG.DUMMY)+OPEN
    }      
    if (jj==12){     #realized spreads
      model<-MPID.SUB.RATIO~LAG.MPID.SUB.RATIO+AGGR.REL.MPID+ORSZ.DVOL.MPID+AGGR.REL.TOTAL+ORSZ.DVOL.TOTAL+
        VOL.GRID.SHORT.PRE+RELRLZSPR.EXE.REP.PRE+SUB.ALL.DVOL+EXE.ALL.DVOL+
        DEPTH.TOTAL.DVOL+abs(RELDPR.MID)+NEG.DUMMY+
        (abs(RELDPR.MID)*NEG.DUMMY)+OPEN
    }            
    if (jj==13){     #volatility #1
      model<-MPID.SUB.RATIO~LAG.MPID.SUB.RATIO+AGGR.REL.MPID+ORSZ.DVOL.MPID+AGGR.REL.TOTAL+ORSZ.DVOL.TOTAL+
        VOL.SD+RELSPR+SUB.ALL.DVOL+EXE.ALL.DVOL+
        DEPTH.TOTAL.DVOL+abs(RELDPR.MID)+NEG.DUMMY+
        (abs(RELDPR.MID)*NEG.DUMMY)+OPEN
    }        
    if (jj==14){     #volatility #2
      model<-MPID.SUB.RATIO~LAG.MPID.SUB.RATIO+AGGR.REL.MPID+ORSZ.DVOL.MPID+AGGR.REL.TOTAL+ORSZ.DVOL.TOTAL+
        VOL.GRID.LONG.PRE+RELSPR+SUB.ALL.DVOL+EXE.ALL.DVOL+
        DEPTH.TOTAL.DVOL+abs(RELDPR.MID)+NEG.DUMMY+
        (abs(RELDPR.MID)*NEG.DUMMY)+OPEN
    }      
    if (jj==15){     #just total
      model<-MPID.SUB.RATIO~LAG.MPID.SUB.RATIO+SUB.ALL.DVOL+EXE.ALL.DVOL+VOL.GRID.SHORT.PRE+RELSPR
    }
    if (jj==16){    #just buy side
      model<-MPID.SUB.BUY.RATIO~LAG.MPID.SUB.BUY.RATIO+SUB.BUY.DVOL+EXE.BUY.DVOL+
        VOL.GRID.SHORT.PRE+RELSPR
    }      
    if (jj==17){    #just sell side
      model<-MPID.SUB.SELL.RATIO~LAG.MPID.SUB.SELL.RATIO+SUB.SELL.DVOL+EXE.SELL.DVOL+
        VOL.GRID.SHORT.PRE+RELSPR
    }  
    if (jj==18){     #negative dummy
      model<-MPID.SUB.RATIO~LAG.MPID.SUB.RATIO+AGGR.REL.MPID+ORSZ.DVOL.MPID+
        VOL.GRID.SHORT.PRE+RELSPR+SUB.ALL.DVOL+EXE.ALL.DVOL+
        DEPTH.TOTAL.DVOL+OPEN
    }    
    if (jj==19){     #negative dummy
      model<-MPID.SUB.RATIO~LAG.MPID.SUB.RATIO+AGGR.REL.MPID+ORSZ.DVOL.MPID+AGGR.REL.TOTAL+ORSZ.DVOL.TOTAL+
        VOL.GRID.SHORT.PRE+RELSPR+SUB.ALL.DVOL+EXE.ALL.DVOL+
        DEPTH.TOTAL.DVOL+abs(RELDPR.MID)+NEG.DUMMY+
        (abs(RELDPR.MID)*NEG.DUMMY)+OPEN+HASBROUCK.MAX
    }
    
    #########################################################################################      
    
    #filename<-paste(mypath,"Results/",folder,"/Regression_Ratio_",w,"sec_",jj,".txt",sep="")
    #cat(paste("\n \n","Firm-Level Regression #", jj, ", Dep Var: ",all.vars(model)[2],"\n \n",sep=""),file=filename,append=FALSE)

    ii<-1
    
    mydata_temp<-mydata[mydata$TICK==ii,]
    
    MPID.SUB.RATIO<-mydata_temp$MPID.SUB.RATIO
    MPID.SUB.BUY.RATIO<-mydata_temp$MPID.SUB.BUY.RATIO
    MPID.SUB.SELL.RATIO<-mydata_temp$MPID.SUB.BUY.RATIO
    LAG.MPID.SUB.RATIO<-Lag(mydata_temp$MPID.SUB.RATIO,k=1:lags)
    LAG.MPID.SUB.BUY.RATIO<-Lag(mydata_temp$MPID.SUB.BUY.RATIO,k=1:lags)
    LAG.MPID.SUB.SELL.RATIO<-Lag(mydata_temp$MPID.SUB.BUY.RATIO,k=1:lags)
    AGGR.REL.MPID<-Lag(mydata_temp$AGGR.REL.MPID,k=1)
    ORSZ.DVOL.MPID<-Lag(mydata_temp$ORSZ.DVOL.MPID,k=1)
    AGGR.REL.TOTAL<-Lag(mydata_temp$AGGR.REL.TOTAL,k=1)
    ORSZ.DVOL.TOTAL<-Lag(mydata_temp$ORSZ.DVOL.TOTAL,k=1)
    VOL.GRID.SHORT.PRE<-Lag(mydata_temp$VOL.GRID.SHORT.PRE,k=1)
    VOL.GRID.LONG.PRE<-Lag(mydata_temp$VOL.GRID.LONG.PRE,k=1)
    VOL.SD<-Lag(mydata_temp$VOL.SD,k=1) 
    RELSPR<-Lag(mydata_temp$RELSPR,k=1)
    RELEFFSPR.EXE.REP<-Lag(mydata_temp$RELEFFSPR.EXE.REP,k=1)
    RELRLZSPR.EXE.REP.PRE<-Lag(mydata_temp$RELRLZSPR.EXE.REP.PRE,k=1)
    SUB.ALL.DVOL<-Lag(mydata_temp$SUB.ALL.DVOL,k=1)
    EXE.ALL.DVOL<-Lag(mydata_temp$EXE.ALL.DVOL,k=1)
    SUB.BUY.DVOL<-Lag(mydata_temp$SUB.BUY.DVOL,k=1)
    EXE.BUY.DVOL<-Lag(mydata_temp$EXE.BUY.DVOL,k=1)
    SUB.SELL.DVOL<-Lag(mydata_temp$SUB.SELL.DVOL,k=1)
    EXE.SELL.DVOL<-Lag(mydata_temp$EXE.SELL.DVOL,k=1)
    DEPTH.TOTAL.DVOL<-Lag(mydata_temp$DEPTH.TOTAL.DVOL,k=1)
    DEPTH.BUY.DVOL<-Lag(mydata_temp$DEPTH.TOTAL.DVOL,k=1)
    DEPTH.SELL.DVOL<-Lag(mydata_temp$DEPTH.TOTAL.DVOL,k=1)
    RELDPR.MID<-Lag(mydata_temp$RELDPR.MID,k=1)
    NEG.DUMMY<-Lag(mydata_temp$NEG.DUMMY,k=1)
    POS.DUMMY<-Lag(mydata_temp$POS.DUMMY,k=1)
    HASBROUCK.MAX<-Lag(mydata_temp$HASBROUCK.MAX,k=1)
    OPEN<-mydata_temp$OPEN
    CLOSE<-mydata_temp$CLOSE
    
    day.f<-factor(mydata_temp$DAY)
    DUMMIES<-model.matrix(~day.f); DUMMIES<-DUMMIES[,2:ncol(DUMMIES)]
    model2<-update(model, . ~ . + DUMMIES)
    fit<-lm(model2)
    s<-summary(fit)
    
    rr<-nrow(s$coefficients)
    t<-length(ticks)
    
    formatlab<-matrix(NA,t*rr,4*4)
    colnames(formatlab)<-c("COEF","SD.ERR","T.STAT","P.VAL","COEF","SD.ERR","T.STAT","P.VAL","COEF","SD.ERR","T.STAT","P.VAL","COEF","SD.ERR","T.STAT","P.VAL")
    rowns<-matrix(NA,t*rr)
        
    for (ii in 1:length(ticks)){   
      
    mydata_temp<-mydata[mydata$TICK==ii,]
    
    MPID.SUB.RATIO<-mydata_temp$MPID.SUB.RATIO
    MPID.SUB.BUY.RATIO<-mydata_temp$MPID.SUB.BUY.RATIO
    MPID.SUB.SELL.RATIO<-mydata_temp$MPID.SUB.BUY.RATIO
    LAG.MPID.SUB.RATIO<-Lag(mydata_temp$MPID.SUB.RATIO,k=1:lags)
    LAG.MPID.SUB.BUY.RATIO<-Lag(mydata_temp$MPID.SUB.BUY.RATIO,k=1:lags)
    LAG.MPID.SUB.SELL.RATIO<-Lag(mydata_temp$MPID.SUB.BUY.RATIO,k=1:lags)
    AGGR.REL.MPID<-Lag(mydata_temp$AGGR.REL.MPID,k=1)
    ORSZ.DVOL.MPID<-Lag(mydata_temp$ORSZ.DVOL.MPID,k=1)
    AGGR.REL.TOTAL<-Lag(mydata_temp$AGGR.REL.TOTAL,k=1)
    ORSZ.DVOL.TOTAL<-Lag(mydata_temp$ORSZ.DVOL.TOTAL,k=1)
    VOL.GRID.SHORT.PRE<-Lag(mydata_temp$VOL.GRID.SHORT.PRE,k=1)
    VOL.GRID.LONG.PRE<-Lag(mydata_temp$VOL.GRID.LONG.PRE,k=1)
    VOL.SD<-Lag(mydata_temp$VOL.SD,k=1) 
    RELSPR<-Lag(mydata_temp$RELSPR,k=1)
    RELEFFSPR.EXE.REP<-Lag(mydata_temp$RELEFFSPR.EXE.REP,k=1)
    RELRLZSPR.EXE.REP.PRE<-Lag(mydata_temp$RELRLZSPR.EXE.REP.PRE,k=1)
    SUB.ALL.DVOL<-Lag(mydata_temp$SUB.ALL.DVOL,k=1)
    EXE.ALL.DVOL<-Lag(mydata_temp$EXE.ALL.DVOL,k=1)
    SUB.BUY.DVOL<-Lag(mydata_temp$SUB.BUY.DVOL,k=1)
    EXE.BUY.DVOL<-Lag(mydata_temp$EXE.BUY.DVOL,k=1)
    SUB.SELL.DVOL<-Lag(mydata_temp$SUB.SELL.DVOL,k=1)
    EXE.SELL.DVOL<-Lag(mydata_temp$EXE.SELL.DVOL,k=1)
    DEPTH.TOTAL.DVOL<-Lag(mydata_temp$DEPTH.TOTAL.DVOL,k=1)
    DEPTH.BUY.DVOL<-Lag(mydata_temp$DEPTH.TOTAL.DVOL,k=1)
    DEPTH.SELL.DVOL<-Lag(mydata_temp$DEPTH.TOTAL.DVOL,k=1)
    RELDPR.MID<-Lag(mydata_temp$RELDPR.MID,k=1)
    NEG.DUMMY<-Lag(mydata_temp$NEG.DUMMY,k=1)
    POS.DUMMY<-Lag(mydata_temp$POS.DUMMY,k=1)
    HASBROUCK.MAX<-Lag(mydata_temp$HASBROUCK.MAX,k=1)
    OPEN<-mydata_temp$OPEN
    CLOSE<-mydata_temp$CLOSE
    
  #  fit<-lm(model)
  #  s<-summary(fit)
  #  ss<-coeftest(fit, vcov=vcovHAC(fit))
      
    #  r<-length(names(fit$coefficients))
    #  formatlab[(rr*(ii-1)+1):(rr*(ii-1)+r),1:4]<-unname(s$coefficients)
    #  formatlab[(rr*(ii-1)+1):(rr*(ii-1)+r),5:8]<-unname(ss)

     # cat(paste("\n \n",ticks[ii], " Regression #", jj, ", Dep Var: ",all.vars(model)[2],"\n \n",sep=""),file=filename,append=TRUE)
    #  capture.output(s, file=filename,append=TRUE)
    #  capture.output(ss, file=filename,append=T)
      
      day.f<-factor(mydata_temp$DAY)
      DUMMIES<-model.matrix(~day.f)
      model2<-update(model, . ~ . + DUMMIES); DUMMIES<-DUMMIES[,2:ncol(DUMMIES)]
      fit<-lm(model2)
      #s<-summary(fit)
      #ss<-coeftest(fit, vcov=vcovHAC(fit))
      
      #cat(paste("\n \n",ticks[ii], " Regression + Day Dummies #", jj, ", Dep Var: ",all.vars(model)[2],"\n \n",sep=""),file=filename,append=TRUE)
      #capture.output(s, file=filename,append=TRUE)
      #capture.output(ss, file=filename,append=T)
      
      #formatlab[(rr*(ii-1)+1):(rr*(ii-1)+rr),9:12]<-unname(s$coefficients)
      #formatlab[(rr*(ii-1)+1):(rr*(ii-1)+rr),13:16]<-unname(ss)
      #row<-paste(ticks[ii],".",names(fit$coefficients),sep="")
      #rowns[(rr*(ii-1)+1):(rr*(ii-1)+rr)]<-row
       
      v<-unname(residuals(fit))

        #pdf(paste(mypath,"/Graphs/",folder,"_Errors/",ticks[ii],"_IndividualErrors_Ratio_",jj,"_",w,"sec.pdf",sep=""))
        #plot(v,type = "l",ylab="Panel Errors",main=ticks[ii],xlab="Date")
        #dev.off()
        
        test1<-adf.test(v)
        test2<-PP.test(v,lshort=FALSE)
        test3<-kpss.test(v,null=c("Level"))
        test4<-kpss.test(v,null=c("Trend"))
        test5<-dwtest(model,alternative="two.sided")
        test6<-summary(lm(v[-length(v)]~v[-1]))
        test7<-Box.test(v, lag=(2*lags), type = c("Ljung-Box"),fitdf=(lags))
        
        stationary[jj,(1+(numtest*2)*(ii-1)):((numtest*2)+(numtest*2)*(ii-1))]<-c(test1$statistic,test1$p.value,test2$statistic,test2$p.value,
                                                                                  test3$statistic,test3$p.value,test4$statistic,test4$p.value,
                                                                                  test5$statistic,test5$p.value,
                                                                                  test6$coefficients[2,3],test6$coefficients[2,4],
                                                                                  test7$statistic,test7$p.value)
        
        #filename2<-paste(mypath,"Results/", folder,"/Regression_Ratio_",ticks[ii],"_",w,"sec_",jj,".rda",sep="")
        #save(fit,file=filename2)
        
         }


      #rownames(formatlab)<-rowns
      #write.xlsx(formatlab, paste(mypath,"Results/", folder, "/Regression_Ratio_",w,"sec_",jj,".xlsx",sep=""))
      
  }
  
  
  colnames(stationary)<-c('AAPL',vector(mode="numeric",length=(2*numtest-1)),'CSCO',vector(mode="numeric",length=(2*numtest-1)),
                          'EBAY',vector(mode="numeric",length=(2*numtest-1)),'FB',vector(mode="numeric",length=(2*numtest-1)),
                          'GOOG',vector(mode="numeric",length=(2*numtest-1)),
                          'INTC',vector(mode="numeric",length=(2*numtest-1)),'MSFT',vector(mode="numeric",length=(2*numtest-1)),
                          'YHOO',vector(mode="numeric",length=(2*numtest-1)))
  
  filename2<-paste(mypath,"Results/", folder, "/INDIVIDUALERRORS_",lags,"Lags_",w,"sec.xlsx",sep="")
  write.xlsx(stationary,file=filename2,append=FALSE) 
  
#  }
    
    #  return(jj)
    
#  }
#}

# CLUSTER ----------------------------------------------------------------

#ID <- as.integer(Sys.getenv("SGE_TASK_ID"))
#s <- panel(ID)


