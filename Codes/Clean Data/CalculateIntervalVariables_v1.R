rm(list=ls(all=TRUE))
library(plyr)
library(mgcv)
#library(pracma)

calculateVariables<-function(i){
  
  ticks<-c("AAPL","CSCO","EBAY","FB","GOOG","INTC","MSFT","YHOO")
  #dates<-c("04","05","06","07","08","12","13","14","15","18","19","20","21","22")
  dates<-c("07","08","12","13","14","15","18","19","20","21","22")
  #windows<-c(10,30,60,120)
  windows<-c(30,60,120) 

  for (j in 1:length(dates)){
    
    #i<-1
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
      
      mpid_all<-matrix(NA,length(intervals)-1,18)
      mpid_tmbr<-matrix(NA,length(intervals)-1,18)
      aggr<-matrix(NA,length(intervals)-1,8)
      orsz<-matrix(NA,length(intervals)-1,8)
      vol<-matrix(NA,length(intervals)-1,5)      
      spreads<-matrix(NA,length(intervals)-1,20)
      pricemov<-matrix(NA,length(intervals)-1,4)
      orimb<-matrix(NA,length(intervals)-1,27)
      depth<-matrix(NA,length(intervals)-1,6)
      exe_time<-matrix(NA,length(intervals)-1,2)
      ad_select<-matrix(NA,length(intervals)-1,6)
      
      #Define Temporary Variables that are updated with each submission
      effspr_temp<-NA; releffspr_temp<-NA
      rlzspr_pre_temp<-NA; relrlzspr_pre_temp<-NA
      rlzspr_post_temp<-NA; relrlzspr_post_temp<-NA
      ad_select_pre_temp<-NA; ad_select_post_temp<-NA; 
      
      for (q in 1:(length(intervals)-1)){
        
        cat("iteration = ", q, "\n")
        
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
        
        #number of unique MPID's
        
        mpid_all[q,]<-c(mpid_total,mpid_buy,mpid_sell,mpid_canc,mpid_cancbuy,mpid_cancsell,mpid_exe,mpid_exebuy,mpid_exesell,
                        mpid_vol_total,mpid_vol_buy,mpid_vol_sell,mpid_vol_canc,mpid_vol_cancbuy,mpid_vol_cancsell,mpid_vol_exe,
                        mpid_vol_exebuy,mpid_vol_exesell)
        
        
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
        
        #number of unique MPID's
        
        mpid_tmbr[q,]<-c(tmbr_total,tmbr_buy,tmbr_sell,tmbr_canc,tmbr_cancbuy,tmbr_cancsell,tmbr_exe,tmbr_exebuy,tmbr_exesell,
                        tmbr_vol_total,tmbr_vol_buy,tmbr_vol_sell,tmbr_vol_canc,tmbr_vol_cancbuy,tmbr_vol_cancsell,tmbr_vol_exe,
                        tmbr_vol_exebuy,tmbr_vol_exesell)
        
        
        ## Aggressiveness
        qq<-which(event==1)
        qq<-qq[qq>=st&qq<=en]
        mid<-0.5*(ask[qq-1]+bid[qq-1])
        temp<-(price[qq]-ssquotes[qq-1])*direction[qq]
        aggr_all<-cbind(mean(temp,na.rm=T),mean((temp/mid)*100,na.rm=T)); aggr_all<-replace(aggr_all,is.nan(aggr_all),NA)
        
        qq<-which(event==1&mpid!=0)
        qq<-qq[qq>=st&qq<=en]
        mid<-0.5*(ask[qq-1]+bid[qq-1])
        temp<-(price[qq]-ssquotes[qq-1])*direction[qq]
        aggr_mpid<-cbind(mean(temp,na.rm=T),mean((temp/mid)*100,na.rm=T)); aggr_mpid<-replace(aggr_mpid,is.nan(aggr_mpid),NA)

        qq<-which(event==1&mpid==12)
        qq<-qq[qq>=st&qq<=en]
        mid<-0.5*(ask[qq-1]+bid[qq-1])
        temp<-(price[qq]-ssquotes[qq-1])*direction[qq]
        aggr_tmbr<-cbind(mean(temp,na.rm=T),mean((temp/mid)*100,na.rm=T)); aggr_tmbr<-replace(aggr_tmbr,is.nan(aggr_tmbr),NA)
        
        qq<-which(event==1&mpid==0)
        qq<-qq[qq>=st&qq<=en]
        mid<-0.5*(ask[qq-1]+bid[qq-1])
        temp<-(price[qq]-ssquotes[qq-1])*direction[qq]
        aggr_anon<-cbind(mean(temp,na.rm=T),mean((temp/mid)*100,na.rm=T)); aggr_anon<-replace(aggr_anon,is.nan(aggr_anon),NA)      
        
        aggr[q,]<-c(aggr_all,aggr_mpid,aggr_tmbr,aggr_anon)
        
        ## Order Size
        qq<-which(event==1)
        qq<-qq[qq>=st&qq<=en]
        orsz_all<-c(mean(sz[qq],na.rm=T),mean(sz[qq]*price[qq],na.rm=T)); orsz_all<-replace(orsz_all,is.nan(orsz_all),NA)
        
        qq<-which(event==1&mpid!=0)
        qq<-qq[qq>=st&qq<=en]      
        orsz_mpid<-c(mean(sz[qq],na.rm=T),mean(sz[qq]*price[qq],na.rm=T)); orsz_mpid<-replace(orsz_mpid,is.nan(orsz_mpid),NA)

        qq<-which(event==1&mpid==12)
        qq<-qq[qq>=st&qq<=en]      
        orsz_tmbr<-c(mean(sz[qq],na.rm=T),mean(sz[qq]*price[qq],na.rm=T)); orsz_tmbr<-replace(orsz_tmbr,is.nan(orsz_tmbr),NA)
        
        qq<-which(event==1&mpid==0)
        qq<-qq[qq>=st&qq<=en]      
        orsz_anon<-c(mean(sz[qq],na.rm=T),mean(sz[qq]*price[qq],na.rm=T)); orsz_anon<-replace(orsz_anon,is.nan(orsz_anon),NA)
        
        orsz[q,]<-cbind(orsz_all,orsz_mpid,orsz_tmbr,orsz_anon)
        
        ## Volatility 
        
        vol1<-NA
        vol2<-NA      
        vol3<-NA
        vol4<-NA  
        
        ##PRE VOLATILITY
        
        ##use grid method, for n=5 grids that are of length delta=w
        if (q>6){
          
          mid<-log(0.5*(ask+bid))
          K<-10; grid<-1; n<-5; delta<-w #define variables for calculating volatility 
          
          RV_full<-matrix(NA,1,K)
          
          for (xx in 1:K){
            
            intervaltemp<-seq(intervals[q-5]-(xx-1)*grid,intervals[q+1]-(xx-1)*grid,delta) #split this interval into n intervals
            
            RV<-matrix(NA,1,n)
            
            for (ii in 1:(length(intervaltemp)-1)){
              
              iia<-min(which(time>intervaltemp[ii]))
              iib<-max(which(time<=intervaltemp[ii+1]))
              
              if (is.infinite(iia)|is.infinite(iib)){next}
              
              RV[ii]<-(mid[iib]-mid[iia])^2
              
            }
            
            RV_full[xx]<-sum(RV,na.rm=T)
            
          }
          
          vol1<-mean(RV_full,na.rm=T)
          
        }
        
        ##use grid method, for n=5 grids that are of lenth delta=w/n
        if (q>1){
          
          mid<-log(0.5*(ask+bid))
          K<-10; grid<-1; n<-5; delta<-w/n #define variables for calculating volatility 
          
          RV_full<-matrix(NA,1,K)
          
          for (xx in 1:K){
            
            intervaltemp<-seq(intervals[q]-(xx-1)*grid,intervals[q+1]-(xx-1)*grid,delta) #split this interval into n intervals
            
            RV<-matrix(NA,1,n)
            
            for (ii in 1:(length(intervaltemp)-1)){
              
              iia<-min(which(time>intervaltemp[ii]))
              iib<-max(which(time<=intervaltemp[ii+1]))
              
              if (is.infinite(iia)|is.infinite(iib)){next}
              
              RV[ii]<-(mid[iib]-mid[iia])^2
              
            }
            
            RV_full[xx]<-sum(RV,na.rm=T)
            
          }
          
          vol2<-mean(RV_full,na.rm=T)
          
        }
        
        ##POST VOLATILITY
        
        ##use grid method, for n=5 grids that are of length delta=w
        if (q<length(intervals)-5){
          
          mid<-log(0.5*(ask+bid))
          K<-10; grid<-1; n<-5; delta<-w #define variables for calculating volatility 
          
          RV_full<-matrix(NA,1,K)
          
          for (xx in 1:K){
            
            intervaltemp<-seq(intervals[q]+(xx-1)*grid,intervals[q+5]+(xx-1)*grid,delta) #split this interval into n intervals
            
            RV<-matrix(NA,1,n)
            
            for (ii in 1:(length(intervaltemp)-1)){
              
              iia<-min(which(time>intervaltemp[ii]))
              iib<-max(which(time<=intervaltemp[ii+1]))
              
              if (is.infinite(iia)|is.infinite(iib)){next}
              
              RV[ii]<-(mid[iib]-mid[iia])^2
              
            }
            
            RV_full[xx]<-sum(RV,na.rm=T)
            
          }
          
          vol3<-mean(RV_full,na.rm=T)
          
        }
        
        ##use grid method, for n=5 grids that are of lenth delta=w/n
        if (q<length(intervals)-1){
          
          mid<-log(0.5*(ask+bid))
          K<-10; grid<-1; n<-5; delta<-w/n #define variables for calculating volatility 
          
          RV_full<-matrix(NA,1,K)
          
          for (xx in 1:K){
            
            intervaltemp<-seq(intervals[q]+(xx-1)*grid,intervals[q+1]+(xx-1)*grid,delta) #split this interval into n intervals
            
            RV<-matrix(NA,1,n)
            
            for (ii in 1:(length(intervaltemp)-1)){
              
              iia<-min(which(time>intervaltemp[ii]))
              iib<-max(which(time<=intervaltemp[ii+1]))
              
              if (is.infinite(iia)|is.infinite(iib)){next}
              
              RV[ii]<-(mid[iib]-mid[iia])^2
              
            }
            
            RV_full[xx]<-sum(RV,na.rm=T)
            
          }
          
          vol4<-mean(RV_full,na.rm=T)
          
        }      
        
        vol5<-sd(mid[(st+1):(en)]-mid[(st):(en-1)])  #standard deviation of midquote returns
        
        vol[q,]<-c(vol1,vol2,vol3,vol4,vol5)
        
        ##need to actually separate pre- and post-volatility
        
        ## Bid-Ask Spreads
        #Quoted Spreads
        midtemp<-0.5*(ask[(st):(en)]+bid[(st):(en)]) #find midquotes at time of order book update
        abspr<-mean(ask[(st):(en)]-bid[(st):(en)]);
        relspr<-mean(abspr/midtemp)*100;
        
        #Effective Spreads
        prtemp<-price[st:en]
        evtemp<-event[st:en]
        dirtemp<-direction[st:en]*(-1); #direction (from the perspective of a buyer)
        
        #Defined for All Events
        effspr_1<-mean((prtemp-midtemp)*dirtemp)
        releffspr_1<-mean((((prtemp-midtemp)*dirtemp)/midtemp)*100)
        #Defined for Executions
        effspr<-(prtemp-midtemp)*dirtemp; 
        effspr_2<-mean(effspr[evtemp==4|evtemp==5])
        releffspr_2<-mean((effspr[evtemp==4|evtemp==5]/midtemp[evtemp==4|evtemp==5])*100)
        #Replacing EffSpr when no executions are present with the previously available variable
        effspr_3<-effspr_2;
        effspr_3<-replace(effspr_3,is.na(effspr_3)==TRUE,effspr_temp)
        releffspr_3<-releffspr_2;
        releffspr_3<-replace(releffspr_3,is.na(releffspr_3)==TRUE,releffspr_temp)
        effspr_temp<-effspr_3; releffspr_temp<-releffspr_3
        
        #Realized Spreads (Pre)
        timetemp=time[(st-1):(en-1)] 
        ia<-findInterval(timetemp-w,time) #index of t
        if (sum(as.numeric(ia))==0){     
          rlzspr_1<-NA; relrlzspr_1<-NA
          rlzspr_2<-NA; relrlzspr_2<-NA
          rlzspr_3<-NA; relrlzspr_3<-NA
        } else{
          iia<-which(ia!=0) #anything with iia is associated with the PRICE IMPACT (time t+5)
          ia<-ia[iia] #anything with ia is associated with the ORDER (time t)
          midtemp<-midtemp[iia] #midquote t+5   
          prtemp<-price[ia] #price t
          evtemp<-event[ia] #type of event t
          midtemp2<-0.5*(ask[ia-1]+bid[ia-1]); #midquote t
          dirtemp<-direction[ia]*(-1); #direction t
          
          #Defined for All Events
          rlzspr_1<-mean((prtemp-midtemp)*dirtemp);     
          relrlzspr_1=mean((((prtemp-midtemp)/midtemp2)*dirtemp)*100);
          #Defined for Executions
          rlzspr<-(prtemp-midtemp)*dirtemp;
          rlzspr_2<-mean(rlzspr[evtemp==4|evtemp==5])    
          relrlzspr_2=mean((rlzspr[evtemp==4|evtemp==5]/midtemp2[evtemp==4|evtemp==5])*100);    
          #Replacing RlzSpr when no executions are present with the previously available variable
          rlzspr_3<-rlzspr_2;
          rlzspr_3<-replace(rlzspr_3,is.na(rlzspr_3)==TRUE,rlzspr_pre_temp)
          relrlzspr_3<-relrlzspr_2;
          relrlzspr_3<-replace(relrlzspr_3,is.na(relrlzspr_3)==TRUE,relrlzspr_pre_temp)
          rlzspr_pre_temp-rlzspr_3; relrlzspr_pre_temp<-relrlzspr_3
        }
        
        ##need to actually separate pre- and post-realized spreads
        
        temp<-cbind(abspr,relspr,effspr_1,releffspr_1,effspr_2,releffspr_2,
                    effspr_3,releffspr_3,rlzspr_1,relrlzspr_1,rlzspr_2,
                    relrlzspr_2,rlzspr_3,relrlzspr_3)
        
        #Realized Spreads (Post)  
        prtemp=price[st:en]; #price of order t
        dirtemp=direction[st:en]*(-1); #direction of order t
        midtemp2=0.5*(ask[(st-1):(en-1)]+bid[(st-1):(en-1)]); #midquote of order t
        evtemp=event[st:en]; #event type of order
        timetemp=time[st:en];
        ia<-findInterval(timetemp+w,time) #index of t+5
        if (sum(as.numeric(ia))==0){     
          rlzspr_1<-NA; relrlzspr_1<-NA
          rlzspr_2<-NA; relrlzspr_2<-NA
          rlzspr_3<-NA; relrlzspr_3<-NA
        } else{
          iia<-which(ia!=0) #anything with iia is associated with the ORDER (time t)
          ia<-ia[iia] #anything with ia is associated with the PRICE IMPACT (time t+5)
          midtemp<-0.5*(ask[ia]+bid[ia]) #midquote t+5   
          prtemp<-prtemp[iia] #price of order t 
          evtemp<-evtemp[iia] #event type of order t
          midtemp2<-midtemp2[iia] #midquote of order t
          dirtemp<-dirtemp[iia] #direction of order t
          
          #Defined for All Events
          rlzspr_1<-mean((prtemp-midtemp)*dirtemp);     
          relrlzspr_1=mean((((prtemp-midtemp)/midtemp2)*dirtemp)*100);
          #Defined for Executions
          rlzspr<-(prtemp-midtemp)*dirtemp;
          rlzspr_2<-mean(rlzspr[evtemp==4|evtemp==5])    
          relrlzspr_2=mean((rlzspr[evtemp==4|evtemp==5]/midtemp2[evtemp==4|evtemp==5])*100);    
          #Replacing RlzSpr when no executions are present with the previously available variable
          rlzspr_3<-rlzspr_2;
          rlzspr_3<-replace(rlzspr_3,is.na(rlzspr_3)==TRUE,rlzspr_post_temp)
          relrlzspr_3<-relrlzspr_2;
          relrlzspr_3<-replace(relrlzspr_3,is.na(relrlzspr_3)==TRUE,relrlzspr_post_temp)
          rlzspr_post_temp<-rlzspr_3; relrlzspr_post_temp<-relrlzspr_3
        }
        
        spreads[q,]<-c(temp,rlzspr_1,relrlzspr_1,rlzspr_2,
                       relrlzspr_2,rlzspr_3,relrlzspr_3)
        
        ## Price Movements
        
        mid<-0.5*(ask+bid)
        
        temp1<-price[(en)]-price[(st)]
        temp2<-((price[(en)]-price[(st)])/price[(st)])*100
        temp3<-mid[(en)]-mid[(st)]
        temp4<-((mid[(en)]-mid[(st)])/mid[(st)])*100
        
        pricemov[q,]<-cbind(temp1,temp2,temp3,temp4)
        
        ## Order Imbalance
        
        evtemp=event[st:en]
        dirtemp=direction[st:en]
        
        #Numbers        
        sub_all<-sum(evtemp==1); exe_all<-sum(evtemp==4|evtemp==5); canc_all<-sum(evtemp==2|evtemp==3); 
        sub_buy<-sum(evtemp==1&dirtemp==1); exe_buy<-sum((evtemp==4|evtemp==5)&dirtemp==1); canc_buy<-sum((evtemp==2|evtemp==3)&dirtemp==1); 
        sub_sell<-sum(evtemp==1&dirtemp==-1); exe_sell<-sum((evtemp==4|evtemp==5)&dirtemp==-1); canc_sell<-sum((evtemp==2|evtemp==3)&dirtemp==-1); 
        orimb_num<-cbind(sub_all,exe_all,canc_all,sub_buy,exe_buy,canc_buy,sub_sell,exe_sell,canc_sell)
        #Dollar Volumes
        orsztemp<-sz[st:en]*price[st:en]
        sub_all<-sum(orsztemp[evtemp==1]); exe_all<-sum(orsztemp[evtemp==4|evtemp==5]); canc_all<-sum(orsztemp[evtemp==2|evtemp==3]);
        sub_buy<-sum(orsztemp[evtemp==1&dirtemp==1]); exe_buy<-sum(orsztemp[(evtemp==4|evtemp==5)&dirtemp==1]); canc_buy<-sum(orsztemp[(evtemp==2|evtemp==3)&dirtemp==1]);
        sub_sell<-sum(orsztemp[evtemp==1&dirtemp==-1]); exe_sell<-sum(orsztemp[(evtemp==4|evtemp==5)&dirtemp==-1]); canc_sell<-sum(orsztemp[(evtemp==2|evtemp==3)&dirtemp==-1]);
        orimb_dvol<-cbind(sub_all,exe_all,canc_all,sub_buy,exe_buy,canc_buy,sub_sell,exe_sell,canc_sell)
        #Share Volumes
        orsztemp<-sz[st:en]
        sub_all<-sum(orsztemp[evtemp==1]); exe_all<-sum(orsztemp[evtemp==4|evtemp==5]); canc_all<-sum(orsztemp[evtemp==2|evtemp==3]);
        sub_buy<-sum(orsztemp[evtemp==1&dirtemp==1]); exe_buy<-sum(orsztemp[(evtemp==4|evtemp==5)&dirtemp==1]); canc_buy<-sum(orsztemp[(evtemp==2|evtemp==3)&dirtemp==1]);
        sub_sell<-sum(orsztemp[evtemp==1&dirtemp==-1]); exe_sell<-sum(orsztemp[(evtemp==4|evtemp==5)&dirtemp==-1]); canc_sell<-sum(orsztemp[(evtemp==2|evtemp==3)&dirtemp==-1]);
        orimb_svol<-cbind(sub_all,exe_all,canc_all,sub_buy,exe_buy,canc_buy,sub_sell,exe_sell,canc_sell)
        
        orimb[q,]<-cbind(orimb_num,orimb_dvol,orimb_svol)
        
        ## Depth
        depth[q,]<-cbind(mean(bidsz[(st):(en)]+asksz[(st):(en)]),
                         mean(bidsz[(st):(en)]), mean(asksz[(st):(en)]),
                         mean(depth_all[(st):(en)]), mean(depth_buy[(st):(en)]),
                         mean(depth_sell[(st):(en)]))
        
        ##Execution Time
        qq<-which(event==4|event==5)
        qq<-qq[qq>=st&qq<=en]
        exe_id<-order_id[qq]
        
        exe_time_temp<-matrix(NA,length(exe_id),1)
        for (ii in 1:length(exe_id)){
          temp<-exe_id[ii]  
          ind<-qq[ii]
          temp2<-which(order_id==temp&event==1)
          if (length(temp2)==0){next}
          if (length(temp2)>1){stop("Multiple Submissions")}
          if (temp2>qq[ii]){stop("Submission time after execution time")}
          exe_time_temp[ii]<-time[qq[ii]]-time[temp2]
          if (exe_time_temp[ii]<0){stop("Negative time-to-execution")}
        }        
        exe_time_pre<-mean(exe_time_temp,na.rm=T)
        
        qq<-which(event==1)
        qq<-qq[qq>=st&qq<=en]
        sub_id<-order_id[qq]
        exe_time_temp<-matrix(NA,length(exe_id),1)
        
        for (ii in 1:length(sub_id)){
          temp<-sub_id[ii]  
          temp2<-which(order_id==temp&(event==4|event==5))
          if (length(temp2)==0){next}
          if (mean(temp2)<qq[ii]){stop("Submission time after execution time")}
          exe_time_temp[ii]<-mean(time[temp2],na.rm=T)-time[qq[ii]]
          if (exe_time_temp[ii]<0){stop("Negative time-to-execution")}
        }       
        exe_time_post<-mean(exe_time_temp,na.rm=T)
        
        exe_time[q,]<-cbind(exe_time_pre,exe_time_post)
        
        ## Adverse Selection Cost (Pre)
        midtemp2<-0.5*(ask[(st-1):(en-1)]+bid[(st-1):(en-1)]) #midquote of t+5
        timetemp=time[(st-1):(en-1)] 
        ia<-findInterval(timetemp-w,time) #index of t
        if (sum(as.numeric(ia))==0){     
          ad_select_1<-NA; ad_select_2<-NA; ad_select_3<-NA;
        } else{
          iia<-which(ia!=0) #anything with iia is associated with the PRICE IMPACT (time t+5)
          ia<-ia[iia] #anything with ia is associated with the ORDER (time t)
          midtemp2<-midtemp2[iia] #midquote t+5    
          midtemp<-0.5*(ask[ia]+bid[ia]); #midquote t
          evtemp<-event[ia] #event t
          dirtemp<-direction[ia]*(-1); #direction t
          
          ad<-((midtemp2-midtemp)/midtemp)*dirtemp
          
          #Defined for All Events
          ad_select_1<-mean(ad)
          #Defined for Executions
          ad_select_2<-mean(ad[evtemp==4|evtemp==5])
          #Replacing adselect when no executions are present with the previously available variable
          ad_select_3<-ad_select_2
          ad_select_3<-replace(ad_select_3,is.na(ad_select_3)==TRUE,ad_select_pre_temp)
          ad_select_pre_temp<-ad_select_3
          
        }
        
        temp<-cbind(ad_select_1,ad_select_2,ad_select_3)
        
        ## Adverse Selection Cost (Post)
        midtemp<-0.5*(ask[(st):(en)]+bid[(st):(en)]) #midquote t
        dirtemp=direction[st:en]*(-1); #direction t
        evtemp<-event[(st):(en)] #event type t
        timetemp=time[(st):(en)] 
        ia<-findInterval(timetemp+w,time) #index of t+5
        if (sum(as.numeric(ia))==0){     
          ad_select_1<-NA; ad_select_2<-NA; ad_select_3<-NA;
        } else{
          iia<-which(ia!=0) #anything with iia is associated with the ORDER (time t)
          ia<-ia[iia] #anything with ia is associated with the PRICE IMPACT (time t+5)
          midtemp<-midtemp[iia] #midquote of t
          evtemp<-evtemp[iia] #type of order t
          dirtemp<-dirtemp[iia]*(-1); #direction t
          midtemp2<-0.5*(ask[ia]+bid[ia]); #midquote t+5
          
          ad<-((midtemp2-midtemp)/midtemp)*dirtemp          
          
          #Defined for All Events
          ad_select_1<-mean(ad)
          #Defined for Executions
          ad_select_2<-mean(ad[evtemp==4|evtemp==5])
          #Replacing adselect when no executions are present with the previously available variable
          ad_select_3<-ad_select_2
          ad_select_3<-replace(ad_select_3,is.na(ad_select_3)==TRUE,ad_select_post_temp)
          ad_select_post_temp<-ad_select_3
          
        }
        
        ad_select[q,]<-cbind(temp,ad_select_1,ad_select_2,ad_select_3)
        
        
        
        
      }
      
      ##column titles (very important)
      ##save data
      
      col1<-c("MPID.SUB","MPID.SUB.BUY","MPID.SUB.SELL","MPID.CANC","MPID.CANC.BUY","MPID.CANC.SELL",
              "MPID.EXE","MPID.EXE.BUY","MPID.EXE.SELL","MPID.VOL.SUB","MPID.VOL.SUB.BUY","MPID.VOL.SUB.SELL",
              "MPID.VOL.CANC","MPID.VOL.CANC.BUY","MPID.VOL.CANC.SELL",
              "MPID.VOL.EXE","MPID.VOL.EXE.BUY","MPID.VOL.EXE.SELL") #mpid_all
      col12<-c("TMBR.SUB","TMBR.SUB.BUY","TMBR.SUB.SELL","TMBR.CANC","TMBR.CANC.BUY","TMBR.CANC.SELL",
              "TMBR.EXE","TMBR.EXE.BUY","TMBR.EXE.SELL","TMBR.VOL.SUB","TMBR.VOL.SUB.BUY","TMBR.VOL.SUB.SELL",
              "TMBR.VOL.CANC","TMBR.VOL.CANC.BUY","TMBR.VOL.CANC.SELL",
              "TMBR.VOL.EXE","TMBR.VOL.EXE.BUY","TMBR.VOL.EXE.SELL") #mpid_tmbr
      col2<-c("AGGR.TOTAL","AGGR.REL.TOTAL","AGGR.MPID","AGGR.REL.MPID","AGGR.TMBR","AGGR.REL.TMBR","AGGR.ANON","AGGR.REL.ANON") #aggressiveness
      col3<-c("ORSZ.SVOL.TOTAL","ORSZ.DVOL.TOTAL","ORSZ.SVOL.MPID","ORSZ.DVOL.MPID","ORSZ.SVOL.TMBR","ORSZ.DVOL.TMBR","ORSZ.SVOL.ANON","ORSZ.DVOL.ANON") #ordersize
      col4<-c("VOL.GRID.LONG.PRE","VOL.GRID.SHORT.PRE","VOL.GRID.LONG.POST","VOL.GRID.SHORT.POST","VOL.SD")
      col5<-c("ABSPR","RELSPR","EFFSPR.ALL","RELEFFSPR.ALL","EFFSPR.EXE","RELEFFSPR.EXE","EFFSPR.EXE.REP",
              "RELEFFSPR.EXE.REP","RLZSPR.ALL.PRE","RELRLZSPR.ALL.PRE","RLZSPR.EXE.PRE","RELRLZSPR.EXE.PRE",
              "RLZSPR.EXE.REP.PRE","RELRLZSPR.EXE.REP.PRE","RLZSPR.ALL.POST","RELRLZSPR.ALL.POST","RLZSPR.EXE.POST",
              "RELRLZSPR.EXE.POST","RLZSPR.EXE.REP.POST","RELRLZSPR.EXE.REP.POST")
      col6<-c("DPR.PRICE","RELDPR.PRICE","DPR.MID","RELDPR.MID")
      col7<-c("SUB.ALL.NUM","EXE.ALL.NUM","CANC.ALL.NUM","SUB.BUY.NUM","EXE.BUY.NUM","CANC.BUY.NUM",
              "SUB.SELL.NUM","EXE.SELL.NUM","CANC.SELL.NUM","SUB.ALL.DVOL","EXE.ALL.DVOL","CANC.ALL.DVOL",
              "SUB.BUY.DVOL","EXE.BUY.DVOL","CANC.BUY.DVOL","SUB.SELL.DVOL","EXE.SELL.DVOL","CANC.SELL.DVOL",
              "SUB.ALL.SVOL","EXE.ALL.SVOL","CANC.ALL.SVOL","SUB.BUY.SVOL","EXE.BUY.SVOL","CANC.BUY.SVOL",
              "SUB.SELL.SVOL","EXE.SELL.SVOL","CANC.SELL.SVOL")
      col8<-c("DEPTH.TOTAL.SVOL","DEPTH.BUY.SVOL","DEPTH.SELL.SVOL","DEPTH.TOTAL.DVOL","DEPTH.BUY.DVOL","DEPTH.SELL.DVOL")
      col9<-c("EXE.TIME.PRE","EXE.TIME.POST")
      col10<-c("ADVSEL.ALL.PRE","ADVSEL.EXE.PRE","ADVSEL.EXE.REP.PRE","ADVSEL.ALL.POST","ADVSEL.EXE.POST",
               "ADVSEL.EXE.REP.POST")
      
      
      cols<-c(col1,col12,col2,col3,col4,col5,col6,col7,col8,col9,col10)
      data<-cbind(mpid_all,mpid_tmbr,aggr,orsz,vol,spreads,pricemov,orimb,depth,exe_time,ad_select)
      colnames(data)<-cols
      rownames(data)<-intervals[1:(length(intervals)-1)]
      
      write.table(data,file=paste(mypath,w,"/",ticks[i],"_2013-11-",dates[j],"_",w,"sec.rda",sep=""))
      
    }
  }
  
  return(i)
  
}

# CLUSTER ----------------------------------------------------------------

ID <- as.integer(Sys.getenv("SGE_TASK_ID"))
s <- calculateVariables(ID)
