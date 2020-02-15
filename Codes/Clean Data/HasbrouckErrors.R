rm(list=ls(all=TRUE))
library(plyr)
library(mgcv)
library(quantmod)
#library(pracma)

#calculateVariables<-function(j){

  ticks<-c("AAPL","CSCO","EBAY","FB","GOOG","INTC","MSFT","YHOO")
  dates<-c("04","05","06","07","08","12","13","14","15","18","19","20","21","22")
  windows<-c(10,30,60,120)
  
  for (i in 1:length(ticks)){
  for (j in 1:length(dates)){
  
  #i<-1
  #j<-1
  #k<-2
  
  #subsec<-1000
  
  mypath="D:/Data/Cleaned_Data/Data/"
  #mypath="C:/Projects/Trader Anonymity/Data/"
  #mypath="Data/"
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
    lags<-5
    
    q<-which(event==4|event==5) #index of transactions
    
    
    y<-diff(log(price[q]))
    x1<-direction[q]; x1<-x1[-1]
    x2<-sz[q]; x2<-x2[-1]*x1
    x3<-sz[q]; x3<-sqrt(x3[-1])*x1
    q<-q[-1]
    
    model<-cbind(y,x1,x2,x3)
    
    ##step one: get residuals from VAR
    test<-VAR(model,p=5,type = c("none"))
    res<-residuals(test)
    ##step two: regress returns on lagged VAR
    model<-res
    for (ii in 1:ncol(res)){
      for (jj in 1:5){
      temp<-Lag(res[,ii],k=jj)
      model<-cbind(model,temp)
      }
    }
    y<-y[-(1:lags)]
    fit<-lm(y~model-1)
    coefs<-fit$coefficients
    ##step three: get sums of coefficients
    alpha_0<--sum(coefs[5:9])
    alpha_1<--sum(coefs[6:9])
    alpha_2<--sum(coefs[7:9])    
    alpha_3<--sum(coefs[8:9])  
    alpha_4<--sum(coefs[9:9]) 
    beta_10<--sum(coefs[10:14])
    beta_11<--sum(coefs[11:14])
    beta_12<--sum(coefs[12:14])    
    beta_13<--sum(coefs[13:14])  
    beta_14<--sum(coefs[14:14]) 
    beta_20<--sum(coefs[15:19])
    beta_21<--sum(coefs[16:19])
    beta_22<--sum(coefs[17:19])    
    beta_23<--sum(coefs[18:19])  
    beta_24<--sum(coefs[19:19]) 
    beta_30<--sum(coefs[20:24])
    beta_31<--sum(coefs[21:24])
    beta_32<--sum(coefs[22:24])    
    beta_33<--sum(coefs[23:24])  
    beta_34<--sum(coefs[24:24]) 
    
    model<-replace(model,is.na(model)==1,0)
    model<-model[,-c(9,14,19,24)]
    
    test<-c(alpha_0,beta_10,beta_20,beta_30,alpha_1,alpha_2,alpha_3,
            beta_11,beta_12,beta_13,beta_21,beta_22,beta_23,
            beta_31,beta_32,beta_33)
    
    st<-matrix(0,nrow(model),1)
    
    for (ii in 1:length(test)){
      st<-st+(test[ii]*model[,ii])
    }
    
    q<-q[-(1:lags)]
    timepoints<-time[q]
    rm(q)
    
    ####cycle through intervals
    
    hasbrouck<-matrix(NA,length(intervals)-1,2)    

    for (q in 1:(length(intervals)-1)){
      
      cat("iteration = ", q, "\n")
      
      #q<-1
      
      sta<-min(which(timepoints>intervals[q]))
      en<-max(which(timepoints<=intervals[q+1]))
      
      if(length(sta)<1|length(en)<1){next}
      
      hasbrouck[q,]<-cbind(max(st[sta:en]),mean(st[sta:en]))

    }
    
    ##column titles (very important)
    ##save data
    
    col1<-c('HASBROUCK.MAX','HASBROUCK.MEAN')
    colnames(hasbrouck)<-col1
    rownames(hasbrouck)<-intervals[1:(length(intervals)-1)]
    mypath2<-"C:/Users/user/Dropbox/Projects/Trader Anonymity/Data/Hasbrouck/"
    write.table(hasbrouck,file=paste(mypath2,"Hasbrouck_",ticks[i],"_2013-11-",dates[j],"_",w,"sec.rda",sep=""))
    
  }

  }
  }

# CLUSTER ----------------------------------------------------------------

#ID <- as.integer(Sys.getenv("SGE_TASK_ID"))
#s <- calculateVariables(ID)
  
  rm(list=ls(all=TRUE))
  library(foreign)
  library(plyr)
  
#  consolidateVariables<-function(i){
    
    ticks<-c("AAPL","CSCO","EBAY","FB","GOOG","INTC","MSFT","YHOO")
    dates<-c("04","05","06","07","08","12","13","14","15","18","19","20","21","22")
    windows<-c(120,60,30,10)
 
    for (i in 1:length(ticks)){   
    for (k in 1:length(windows)){
      
      #i<-1
      #k<-1
      w<-windows[k]
      j<-1
      
      #mypath<-paste("D:/Data/Cleaned_Data/Data/Variables/")
      #mypath<-"C:/Projects/Trader Anonymity/Data/"
      #mypath<-paste("Data/",w,"/",sep="")
      mypath<-"C:/Users/user/Dropbox/Projects/Trader Anonymity/Data/Hasbrouck/"
      mydata<-read.table(file=paste(mypath,"Hasbrouck_",ticks[i],"_2013-11-",dates[j],"_",w,"sec.rda",sep=""))
      
      mydata$DAY<-rep(as.numeric(dates[j]),nrow(mydata))
      
      for (j in 2:length(dates)){
        
        mydata2<-read.table(file=paste(mypath,"Hasbrouck_",ticks[i],"_2013-11-",dates[j],"_",w,"sec.rda",sep=""))
        mydata2$DAY<-rep(as.numeric(dates[j]),nrow(mydata2))
        
        mydata<-rbind(mydata,mydata2)
        
      }
      
      write.table(mydata,file=paste(mypath,"Hasbrouck_",ticks[i],"_",w,"sec.rda",sep=""))
      
    }
    
#    return(i)
    
  }
  
  # CLUSTER ----------------------------------------------------------------
  
#  ID <- as.integer(Sys.getenv("SGE_TASK_ID"))
#  s <- consolidateVariables(ID)  
  
  
  
  
