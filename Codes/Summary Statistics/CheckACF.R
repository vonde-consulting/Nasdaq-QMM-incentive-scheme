rm(list=ls(all=TRUE))
library(plyr)
library(mgcv)
#library(pracma)

head<-"C:/DatiLocali/"
ticks<-c("AAPL","CSCO","EBAY","FB","GOOG","INTC","MSFT","YHOO")
dates<-c("04","05","06","07","08","12","13","14","15","18","19","20","21","22")

tests<-list()
acfs<-list()

tests_dir<-list()
acfs_dir<-list()

tests_buy<-list()
acfs_buy<-list()

tests_sell<-list()
acfs_sell<-list()


for (i in 1:length(ticks)){

mydataf<-as.data.frame(matrix(NA,0,15))
colnames(mydataf)<-c("seconds","nanoseconds","event","order_id","sz","price","direction","mpid","ask","asksz",
                   "bid","bidsz","time","open","close")

for (j in 1:length(dates)){

mypath<-paste(head,"Dropbox/Projects/Trader Anonymity/Data/",sep="")
mydata<-read.csv(paste(mypath,"CleanedData/",ticks[i],"_2013-11-",dates[j],".csv",sep=""),header=FALSE)

colnames(mydata)<-c("seconds","nanoseconds","event","order_id","sz","price","direction","mpid","ask","asksz",
                    "bid","bidsz")
time<-mydata$seconds+(mydata$nanoseconds*(10^(-9))) 
mydata$time<-time
open<-time; open<-replace(open,open<=9.75*60*60,1); open<-replace(open,open!=1,0)
mydata$open<-open
close<-time; close<-replace(close,close>=15.75*60*60,1); close<-replace(close,close!=1,0)
mydata$close<-close

mydataf<-rbind(mydataf,mydata)

}

mydata_sub_all<-mydataf[mydataf$event==1,]
mydata_canc_all<-mydataf[(mydataf$event==2|mydataf$event==3),]
mydata_sub_buy<-mydataf[(mydataf$event==1&mydataf$direction==1),]
mydata_canc_buy<-mydataf[((mydataf$event==2|mydataf$event==3)&mydataf$direction==1),]
mydata_sub_sell<-mydataf[(mydataf$event==1&mydataf$direction==-1),]
mydata_canc_sell<-mydataf[((mydataf$event==2|mydataf$event==3)&mydataf$direction==-1),]
rm(mydataf)

mydata<-mydata_sub_all
mpid_all<-mydata$mpid; mpid_all<-replace(mpid_all,mpid_all!=0,1)
mpid_dir<-mpid_all*mydata$direction

mpid<-mpid_all
T<-nrow(mydata)
l<-floor(4*(T/100)^(2/9));
test1<-Box.test(mpid,lag=l,type=c("Box-Pierce"))
test2<-Box.test(mpid,lag=l,type=c("Ljung-Box"))
test3<-acf(mpid,type=c("correlation"),lag.max=l,demean=F)
tests[[i]]<-cbind(test1,test2)
acfs[[i]]<-test3

mpid<-mpid_dir
T<-nrow(mydata)
l<-floor(4*(T/100)^(2/9));
test1<-Box.test(mpid,lag=l,type=c("Box-Pierce"))
test2<-Box.test(mpid,lag=l,type=c("Ljung-Box"))
test3<-acf(mpid,type=c("correlation"),lag.max=l,demean=F)
tests_dir[[i]]<-cbind(test1,test2)
acfs_dir[[i]]<-test3

mydata<-mydata_sub_buy
mpid<-mydata$mpid; mpid<-replace(mpid,mpid!=0,1)

T<-nrow(mydata)
l<-floor(4*(T/100)^(2/9));
test1<-Box.test(mpid,lag=l,type=c("Box-Pierce"))
test2<-Box.test(mpid,lag=l,type=c("Ljung-Box"))
test3<-acf(mpid,type=c("correlation"),lag.max=l,demean=F)
tests_buy[[i]]<-cbind(test1,test2)
acfs_buy[[i]]<-test3

mydata<-mydata_sub_sell
mpid<-mydata$mpid; mpid<-replace(mpid,mpid!=0,1)

T<-nrow(mydata)
l<-floor(4*(T/100)^(2/9));
test1<-Box.test(mpid,lag=l,type=c("Box-Pierce"))
test2<-Box.test(mpid,lag=l,type=c("Ljung-Box"))
test3<-acf(mpid,type=c("correlation"),lag.max=l,demean=F)
tests_sell[[i]]<-cbind(test1,test2)
acfs_sell[[i]]<-test3


}


acf_all<-matrix(NA,length(ticks),10)

for (i in 1:length(ticks)){
  
acf<-acfs[[i]]
acf_all[i,1:10]<-acf$acf[1:10]
  
}

plotname<-paste(mypath,"Graphs/AutoCorr_MPID_All.pdf",sep="")
pdf(plotname,width=7,height=7,paper='special')
p<-matplot(t(acf_all),type="l",lty=1,col=1:8,ylab="Autocorrelation",xlab="Lag",main="Autocorrelation of MPID Submissions")
legend("topright", ticks,lty=1,col=1:8,lwd=1)
dev.off()

acf_all<-matrix(NA,length(ticks),10)

for (i in 1:length(ticks)){
  
  acf<-acfs_buy[[i]]
  acf_all[i,1:10]<-acf$acf[1:10]
  
}

plotname<-paste(mypath,"Graphs/AutoCorr_MPID_Buy.pdf",sep="")
pdf(plotname,width=7,height=7,paper='special')
p<-matplot(t(acf_all),type="l",lty=1,col=1:8,ylab="Autocorrelation",xlab="Lag",main="Autocorrelation of MPID Buy Submissions")
legend("topright", ticks,lty=1,col=1:8,lwd=1)
dev.off()

acf_all<-matrix(NA,length(ticks),10)

for (i in 1:length(ticks)){
  
  acf<-acfs_sell[[i]]
  acf_all[i,1:10]<-acf$acf[1:10]
  
}

plotname<-paste(mypath,"Graphs/AutoCorr_MPID_Sell.pdf",sep="")
pdf(plotname,width=7,height=7,paper='special')
p<-matplot(t(acf_all),type="l",lty=1,col=1:8,ylab="Autocorrelation",xlab="Lag",main="Autocorrelation of MPID Sell Submissions")
legend("topright", ticks,lty=1,col=1:8,lwd=1)
dev.off()

test<-matrix(NA,length(ticks),2)

for (i in 1:length(ticks)){
  
tt<-unname(tests_sell[[i]])
test[i,1]<-unlist(tt[3,1])
test[i,2]<-unlist(tt[3,2])
  
}





