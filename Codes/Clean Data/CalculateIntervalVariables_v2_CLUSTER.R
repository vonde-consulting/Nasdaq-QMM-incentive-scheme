rm(list=ls(all=TRUE))
library(plyr)
library(mgcv)
#library(pracma)

calculateVariables<-function(j){
  
  ticks<-c("AAPL","CSCO","EBAY","FB","GOOG","INTC","MSFT","YHOO")
  dates<-c("04","05","06","07","08","12","13","14","15","18","19","20","21","22")
  #dates<-c("08","12","13","14","15","18","19","20","21","22")
   #windows<-c(10,30,60,120)
   windows<-c(10,30,60)

for (i in 1:length(ticks)){

#  for (j in 1:length(dates)){
    
    #i<-7
    #j<-1
    #k<-1
    
  #subsec<-1000
  
  #mypath="C:/Projects/Trader Anonymity/Data/"
  mypath="Data/"
  mydata<-read.csv(paste(mypath,ticks[i],"_2013-11-",dates[j],".csv",sep=""),header=FALSE)
  seconds<-mydata[,1]
  nanoseconds<-mydata[,2]
  event<-mydata[,3]
  order_id<-mydata[,4]
  sz<-mydata[,5]
  price<-mydata[,6]
  direction<-mydata[,7]
  mpid<-mydata[,8]
  ask<-mydata[,9]
  asksz<-mydata[,10]
  bid<-mydata[,11]
  bidsz<-mydata[,12]
  ddvol<-sz*price
  
  time<-seconds+(nanoseconds*(10^(-9))) 
  open<-time; open<-replace(open,open<=9.75*60*60,1); open<-replace(open,open!=1,0)
  close<-time; close<-replace(close,close>=15.75*60*60,1); close<-replace(close,close!=1,0)
  
  #calculate depth, etc.
  depth_buy<-bid*bidsz
  depth_sell<-ask*asksz
  depth_all<-depth_buy+depth_sell
  ssquotes<-rep(0,length(bid))
  g<-which(direction==1)
  ssquotes[g]<-bid[g]
  g<-which(direction==-1)
  ssquotes[g]<-ask[g]
  
  for (k in 1:length(windows)){
    
    w<-windows[k]
    ##calculate intervals: start at 9:30 and go until 4pm, take every 
    intervals=seq(9.5*60*60,16*60*60,w)  
    
    ## Cycle Through Intervals
    
    mpid_all<-matrix(NA,length(intervals)-1,27)
    mpid_tmbr<-matrix(NA,length(intervals)-1,27)
    aggr<-matrix(NA,length(intervals)-1,8)
    orsz<-matrix(NA,length(intervals)-1,8)

    for (q in 1:(length(intervals)-1)){
      
      cat(ticks[i],w,"iteration = ", q, "out of " , (length(intervals)-1)  , "\n")
      
      #q<-3
      
      st<-min(which(time>intervals[q]))
      en<-max(which(time<=intervals[q+1]))
      
      ##MPID's
      mpid_total<-sum(mpid[st:en]!=0&event[st:en]==1)
      mpid_buy<-sum(mpid[st:en]!=0&event[st:en]==1&direction[st:en]==1)
      mpid_sell<-sum(mpid[st:en]!=0&event[st:en]==1&direction[st:en]==-1)  
      if(mpid_buy+mpid_sell!=mpid_total){stop("MPID buy and sell orders don't add up")}
      
      mpid_canc<-sum(mpid[st:en]!=0&(event[st:en]==2|event[st:en]==3))
      mpid_cancbuy<-sum(mpid[st:en]!=0&(event[st:en]==2|event[st:en]==3)&direction[st:en]==1)
      mpid_cancsell<-sum(mpid[st:en]!=0&(event[st:en]==2|event[st:en]==3)&direction[st:en]==-1)  
      if(mpid_cancbuy+mpid_cancsell!=mpid_canc){stop("MPID buy and sell cancellations don't add up")}
      
      mpid_exe<-sum(mpid[st:en]!=0&(event[st:en]==4|event[st:en]==5))
      mpid_exebuy<-sum(mpid[st:en]!=0&(event[st:en]==4|event[st:en]==5)&direction[st:en]==1)
      mpid_exesell<-sum(mpid[st:en]!=0&(event[st:en]==4|event[st:en]==5)&direction[st:en]==-1)  
      if(mpid_exebuy+mpid_exesell!=mpid_exe){stop("MPID buy and sell executions don't add up")}  
      
      mpid_vol_total<-sum((mpid[st:en]!=0&event[st:en]==1)*sz[st:en])
      mpid_vol_buy<-sum((mpid[st:en]!=0&event[st:en]==1&direction[st:en]==1)*sz[st:en])
      mpid_vol_sell<-sum((mpid[st:en]!=0&event[st:en]==1&direction[st:en]==-1)*sz[st:en])  
      if(mpid_vol_buy+mpid_vol_sell!=mpid_vol_total){stop("MPID buy and sell orders don't add up")}
      
      mpid_vol_canc<-sum((mpid[st:en]!=0&(event[st:en]==2|event[st:en]==3))*sz[st:en])
      mpid_vol_cancbuy<-sum((mpid[st:en]!=0&(event[st:en]==2|event[st:en]==3)&direction[st:en]==1)*sz[st:en])
      mpid_vol_cancsell<-sum((mpid[st:en]!=0&(event[st:en]==2|event[st:en]==3)&direction[st:en]==-1)*sz[st:en])  
      if(mpid_vol_cancbuy+mpid_vol_cancsell!=mpid_vol_canc){stop("MPID buy and sell cancellations don't add up")}
      
      mpid_vol_exe<-sum((mpid[st:en]!=0&(event[st:en]==4|event[st:en]==5))*sz[st:en])
      mpid_vol_exebuy<-sum((mpid[st:en]!=0&(event[st:en]==4|event[st:en]==5)&direction[st:en]==1)*sz[st:en])
      mpid_vol_exesell<-sum((mpid[st:en]!=0&(event[st:en]==4|event[st:en]==5)&direction[st:en]==-1)*sz[st:en])  
      if(mpid_vol_exebuy+mpid_vol_exesell!=mpid_vol_exe){stop("MPID buy and sell executions don't add up")}  
      
      mpid_dvol_total<-sum((mpid[st:en]!=0&event[st:en]==1)*ddvol[st:en])
      mpid_dvol_buy<-sum((mpid[st:en]!=0&event[st:en]==1&direction[st:en]==1)*ddvol[st:en])
      mpid_dvol_sell<-sum((mpid[st:en]!=0&event[st:en]==1&direction[st:en]==-1)*ddvol[st:en])  
      if(abs(mpid_dvol_buy+mpid_dvol_sell-mpid_dvol_total)>10^{-6}){stop("MPID buy and sell orders don't add up")}
       
      mpid_dvol_canc<-sum((mpid[st:en]!=0&(event[st:en]==2|event[st:en]==3))*ddvol[st:en])
      mpid_dvol_cancbuy<-sum((mpid[st:en]!=0&(event[st:en]==2|event[st:en]==3)&direction[st:en]==1)*ddvol[st:en])
      mpid_dvol_cancsell<-sum((mpid[st:en]!=0&(event[st:en]==2|event[st:en]==3)&direction[st:en]==-1)*ddvol[st:en])  
      if(mpid_dvol_cancbuy+mpid_dvol_cancsell-mpid_dvol_canc>10^{-6}){stop("MPID buy and sell cancellations don't add up")}
      
      mpid_dvol_exe<-sum((mpid[st:en]!=0&(event[st:en]==4|event[st:en]==5))*ddvol[st:en])
      mpid_dvol_exebuy<-sum((mpid[st:en]!=0&(event[st:en]==4|event[st:en]==5)&direction[st:en]==1)*ddvol[st:en])
      mpid_dvol_exesell<-sum((mpid[st:en]!=0&(event[st:en]==4|event[st:en]==5)&direction[st:en]==-1)*ddvol[st:en])  
      if(mpid_dvol_exebuy+mpid_dvol_exesell-mpid_dvol_exe>10^{-6}){stop("MPID buy and sell executions don't add up")}  

      #number of unique MPID's
      
      mpid_all[q,]<-c(mpid_total,mpid_buy,mpid_sell,mpid_canc,mpid_cancbuy,mpid_cancsell,mpid_exe,mpid_exebuy,mpid_exesell,
                      mpid_vol_total,mpid_vol_buy,mpid_vol_sell,mpid_vol_canc,mpid_vol_cancbuy,mpid_vol_cancsell,mpid_vol_exe,
                      mpid_vol_exebuy,mpid_vol_exesell,mpid_dvol_total,mpid_dvol_buy,mpid_dvol_sell,mpid_dvol_canc,mpid_dvol_cancbuy,mpid_dvol_cancsell,mpid_dvol_exe,
                      mpid_dvol_exebuy,mpid_dvol_exesell)
      
      ##TMBR Hill
      
      ##MPID's
      tmbr_total<-sum(mpid[st:en]==12&event[st:en]==1)
      tmbr_buy<-sum(mpid[st:en]==12&event[st:en]==1&direction[st:en]==1)
      tmbr_sell<-sum(mpid[st:en]==12&event[st:en]==1&direction[st:en]==-1)  
      if(tmbr_buy+tmbr_sell!=tmbr_total){stop("MPID buy and sell orders don't add up")}
      
      tmbr_canc<-sum(mpid[st:en]==12&(event[st:en]==2|event[st:en]==3))
      tmbr_cancbuy<-sum(mpid[st:en]==12&(event[st:en]==2|event[st:en]==3)&direction[st:en]==1)
      tmbr_cancsell<-sum(mpid[st:en]==12&(event[st:en]==2|event[st:en]==3)&direction[st:en]==-1)  
      if(tmbr_cancbuy+tmbr_cancsell!=tmbr_canc){stop("MPID buy and sell cancellations don't add up")}
      
      tmbr_exe<-sum(mpid[st:en]==12&(event[st:en]==4|event[st:en]==5))
      tmbr_exebuy<-sum(mpid[st:en]==12&(event[st:en]==4|event[st:en]==5)&direction[st:en]==1)
      tmbr_exesell<-sum(mpid[st:en]==12&(event[st:en]==4|event[st:en]==5)&direction[st:en]==-1)  
      if(tmbr_exebuy+tmbr_exesell!=tmbr_exe){stop("MPID buy and sell executions don't add up")}  
      
      tmbr_vol_total<-sum((mpid[st:en]==12&event[st:en]==1)*sz[st:en])
      tmbr_vol_buy<-sum((mpid[st:en]==12&event[st:en]==1&direction[st:en]==1)*sz[st:en])
      tmbr_vol_sell<-sum((mpid[st:en]==12&event[st:en]==1&direction[st:en]==-1)*sz[st:en])  
      if(tmbr_vol_buy+tmbr_vol_sell!=tmbr_vol_total){stop("MPID buy and sell orders don't add up")}
      
      tmbr_vol_canc<-sum((mpid[st:en]==12&(event[st:en]==2|event[st:en]==3))*sz[st:en])
      tmbr_vol_cancbuy<-sum((mpid[st:en]==12&(event[st:en]==2|event[st:en]==3)&direction[st:en]==1)*sz[st:en])
      tmbr_vol_cancsell<-sum((mpid[st:en]==12&(event[st:en]==2|event[st:en]==3)&direction[st:en]==-1)*sz[st:en])  
      if(tmbr_vol_cancbuy+tmbr_vol_cancsell!=tmbr_vol_canc){stop("MPID buy and sell cancellations don't add up")}
      
      tmbr_vol_exe<-sum((mpid[st:en]==12&(event[st:en]==4|event[st:en]==5))*sz[st:en])
      tmbr_vol_exebuy<-sum((mpid[st:en]==12&(event[st:en]==4|event[st:en]==5)&direction[st:en]==1)*sz[st:en])
      tmbr_vol_exesell<-sum((mpid[st:en]==12&(event[st:en]==4|event[st:en]==5)&direction[st:en]==-1)*sz[st:en])  
      if(tmbr_vol_exebuy+tmbr_vol_exesell!=tmbr_vol_exe){stop("MPID buy and sell executions don't add up")}  

      tmbr_dvol_total<-sum((mpid[st:en]==12&event[st:en]==1)*ddvol[st:en])
      tmbr_dvol_buy<-sum((mpid[st:en]==12&event[st:en]==1&direction[st:en]==1)*ddvol[st:en])
      tmbr_dvol_sell<-sum((mpid[st:en]==12&event[st:en]==1&direction[st:en]==-1)*ddvol[st:en])  
      if(abs(tmbr_dvol_buy+tmbr_dvol_sell-tmbr_dvol_total)>10^{-6}){stop("tmbr buy and sell orders don't add up")}
      
      tmbr_dvol_canc<-sum((mpid[st:en]==12&(event[st:en]==2|event[st:en]==3))*ddvol[st:en])
      tmbr_dvol_cancbuy<-sum((mpid[st:en]==12&(event[st:en]==2|event[st:en]==3)&direction[st:en]==1)*ddvol[st:en])
      tmbr_dvol_cancsell<-sum((mpid[st:en]==12&(event[st:en]==2|event[st:en]==3)&direction[st:en]==-1)*ddvol[st:en])  
      if(tmbr_dvol_cancbuy+tmbr_dvol_cancsell-tmbr_dvol_canc>10^{-6}){stop("tmbr buy and sell cancellations don't add up")}
      
      tmbr_dvol_exe<-sum((mpid[st:en]==12&(event[st:en]==4|event[st:en]==5))*ddvol[st:en])
      tmbr_dvol_exebuy<-sum((mpid[st:en]==12&(event[st:en]==4|event[st:en]==5)&direction[st:en]==1)*ddvol[st:en])
      tmbr_dvol_exesell<-sum((mpid[st:en]==12&(event[st:en]==4|event[st:en]==5)&direction[st:en]==-1)*ddvol[st:en])  
      if(tmbr_dvol_exebuy+tmbr_dvol_exesell-tmbr_dvol_exe>10^{-6}){stop("tmbr buy and sell executions don't add up")}  
      
      #number of unique MPID's
      
      mpid_tmbr[q,]<-c(tmbr_total,tmbr_buy,tmbr_sell,tmbr_canc,tmbr_cancbuy,tmbr_cancsell,tmbr_exe,tmbr_exebuy,tmbr_exesell,
                      tmbr_vol_total,tmbr_vol_buy,tmbr_vol_sell,tmbr_vol_canc,tmbr_vol_cancbuy,tmbr_vol_cancsell,tmbr_vol_exe,
                      tmbr_vol_exebuy,tmbr_vol_exesell,tmbr_dvol_total,tmbr_dvol_buy,tmbr_dvol_sell,tmbr_dvol_canc,tmbr_dvol_cancbuy,tmbr_dvol_cancsell,tmbr_dvol_exe,
                      tmbr_dvol_exebuy,tmbr_dvol_exesell)
      
      ## Aggressiveness
      qq<-which(event==1); qq<-qq[(qq!=1)]
      qq<-qq[qq>=st&qq<=en]
      mid<-0.5*(ask[qq-1]+bid[qq-1])
      temp<-(price[qq]-ssquotes[qq-1])*direction[qq]
      aggr_all<-cbind(mean(temp,na.rm=T),mean((temp/mid)*100,na.rm=T)); aggr_all<-replace(aggr_all,is.nan(aggr_all),NA)
      
      qq<-which(event==1&mpid!=0); qq<-qq[(qq!=1)]
      qq<-qq[qq>=st&qq<=en]
      mid<-0.5*(ask[qq-1]+bid[qq-1])
      temp<-(price[qq]-ssquotes[qq-1])*direction[qq]
      aggr_mpid<-cbind(mean(temp,na.rm=T),mean((temp/mid)*100,na.rm=T)); aggr_mpid<-replace(aggr_mpid,is.nan(aggr_mpid),NA)
      
      qq<-which(event==1&mpid==12); qq<-qq[(qq!=1)]
      qq<-qq[qq>=st&qq<=en]
      mid<-0.5*(ask[qq-1]+bid[qq-1])
      temp<-(price[qq]-ssquotes[qq-1])*direction[qq]
      aggr_tmbr<-cbind(mean(temp,na.rm=T),mean((temp/mid)*100,na.rm=T)); aggr_tmbr<-replace(aggr_tmbr,is.nan(aggr_tmbr),NA)
      
      qq<-which(event==1&mpid==0); qq<-qq[(qq!=1)]
      qq<-qq[qq>=st&qq<=en]
      mid<-0.5*(ask[qq-1]+bid[qq-1])
      temp<-(price[qq]-ssquotes[qq-1])*direction[qq]
      aggr_anon<-cbind(mean(temp,na.rm=T),mean((temp/mid)*100,na.rm=T)); aggr_anon<-replace(aggr_anon,is.nan(aggr_anon),NA)      
      
      aggr[q,]<-c(aggr_all,aggr_mpid,aggr_tmbr,aggr_anon)
      
      ## Order Size
      qq<-which(event==1); qq<-qq[(qq!=1)]
      qq<-qq[qq>=st&qq<=en]
      orsz_all<-c(mean(sz[qq],na.rm=T),mean(sz[qq]*price[qq],na.rm=T)); orsz_all<-replace(orsz_all,is.nan(orsz_all),NA)
      
      qq<-which(event==1&mpid!=0); qq<-qq[(qq!=1)]
      qq<-qq[qq>=st&qq<=en]      
      orsz_mpid<-c(mean(sz[qq],na.rm=T),mean(sz[qq]*price[qq],na.rm=T)); orsz_mpid<-replace(orsz_mpid,is.nan(orsz_mpid),NA)
      
      qq<-which(event==1&mpid==12); qq<-qq[(qq!=1)]
      qq<-qq[qq>=st&qq<=en]      
      orsz_tmbr<-c(mean(sz[qq],na.rm=T),mean(sz[qq]*price[qq],na.rm=T)); orsz_tmbr<-replace(orsz_tmbr,is.nan(orsz_tmbr),NA)
      
      qq<-which(event==1&mpid==0); qq<-qq[(qq!=1)]
      qq<-qq[qq>=st&qq<=en]      
      orsz_anon<-c(mean(sz[qq],na.rm=T),mean(sz[qq]*price[qq],na.rm=T)); orsz_anon<-replace(orsz_anon,is.nan(orsz_anon),NA)
      
      orsz[q,]<-cbind(orsz_all,orsz_mpid,orsz_tmbr,orsz_anon)
      
     }
    
    ##column titles (very important)
    ##save data
    
    col1<-c("MPID.SUB","MPID.SUB.BUY","MPID.SUB.SELL","MPID.CANC","MPID.CANC.BUY","MPID.CANC.SELL",
            "MPID.EXE","MPID.EXE.BUY","MPID.EXE.SELL","MPID.VOL.SUB","MPID.VOL.SUB.BUY","MPID.VOL.SUB.SELL",
            "MPID.VOL.CANC","MPID.VOL.CANC.BUY","MPID.VOL.CANC.SELL",
            "MPID.VOL.EXE","MPID.VOL.EXE.BUY","MPID.VOL.EXE.SELL","MPID.DVOL.SUB","MPID.DVOL.SUB.BUY","MPID.DVOL.SUB.SELL",
            "MPID.DVOL.CANC","MPID.DVOL.CANC.BUY","MPID.DVOL.CANC.SELL",
            "MPID.DVOL.EXE","MPID.DVOL.EXE.BUY","MPID.DVOL.EXE.SELL") #mpid_all
    col12<-c("TMBR.SUB","TMBR.SUB.BUY","TMBR.SUB.SELL","TMBR.CANC","TMBR.CANC.BUY","TMBR.CANC.SELL",
             "TMBR.EXE","TMBR.EXE.BUY","TMBR.EXE.SELL","TMBR.VOL.SUB","TMBR.VOL.SUB.BUY","TMBR.VOL.SUB.SELL",
             "TMBR.VOL.CANC","TMBR.VOL.CANC.BUY","TMBR.VOL.CANC.SELL",
             "TMBR.VOL.EXE","TMBR.VOL.EXE.BUY","TMBR.VOL.EXE.SELL","TMBR.DVOL.SUB","TMBR.DVOL.SUB.BUY","TMBR.DVOL.SUB.SELL",
             "TMBR.DVOL.CANC","TMBR.DVOL.CANC.BUY","TMBR.DVOL.CANC.SELL",
             "TMBR.DVOL.EXE","TMBR.DVOL.EXE.BUY","TMBR.DVOL.EXE.SELL") #mpid_tmbr
    col2<-c("AGGR.TOTAL","AGGR.REL.TOTAL","AGGR.MPID","AGGR.REL.MPID","AGGR.TMBR","AGGR.REL.TMBR","AGGR.ANON","AGGR.REL.ANON") #aggressiveness
    col3<-c("ORSZ.SVOL.TOTAL","ORSZ.DVOL.TOTAL","ORSZ.SVOL.MPID","ORSZ.DVOL.MPID","ORSZ.SVOL.TMBR","ORSZ.DVOL.TMBR","ORSZ.SVOL.ANON","ORSZ.DVOL.ANON") #ordersize
 
    cols<-c(col1,col12,col2,col3)
    data<-cbind(mpid_all,mpid_tmbr,aggr,orsz)
    colnames(data)<-cols
    rownames(data)<-intervals[1:(length(intervals)-1)]
    
    write.table(data,file=paste(mypath,ticks[i],"_2013-11-",dates[j],"_",w,"sec2.rda",sep=""))
      
    }
    }
#  }
  
  return(j)
  
}

# CLUSTER ----------------------------------------------------------------

ID <- as.integer(Sys.getenv("SGE_TASK_ID"))
s <- calculateVariables(ID)
