rm(list=ls(all=TRUE))
#library(car)
#library(plm)
#library(tseries)
library(xlsx)
#library(lmtest)
#library(sandwich)
#library(moments)

#panel <- function(jj){

windows<-c(60,30,10)
ticks<-c("AAPL","CSCO","EBAY","FB","GOOG","INTC","MSFT","YHOO")
#mypath<-"//tsclient/C/Users/Julia/Dropbox/Projects/Trader Anonymity/Data/"
mypath<-"C:/Users/Julia/Dropbox/Projects/Trader Anonymity/Data/"
k<-1
w<-windows[k]
nint<-((16*60*60-9.5*60*60)/w)*14

MPIDSUB<-matrix(NA,nint,length(ticks))

for (i in 1:length(ticks)){

mydata<-read.table(file=paste(mypath,"Data/",ticks[i],"_",w,"sec.rda",sep=""))
mydata$MPID.SUB.RATIO<-mydata$MPID.SUB/mydata$SUB.ALL.NUM
MPIDSUB[,i]<-mydata$MPID.SUB.RATIO
rm(mydata)

}

nlags<-20
ACF<-matrix(NA,nlags,length(ticks))

for (i in 1:length(ticks)){
  ACF[,i]<-acf(MPIDSUB[,i],lag.max=nlags+1,na.action=na.pass,plot=F)$acf[2:(nlags+1),,]
}

plotname<-paste(mypath,"SummaryStats/Autocorrelation/Autocorrelation_AllStocks_",w,"sec.pdf",sep="")

pdf(file=plotname,width=6,height=6,paper="special")
matplot(ACF,type="l",lty=1,col=1:8,lwd=2,ylim=c(-0.1,0.8),xlim=c(1,nlags),xlab="Lag",ylab="Sample Autocorrelation")
abline(h=0)
#add conf intervals
abline(h=c(2/sqrt(nint),-2/sqrt(nint)),lty=c(2,2))
#add legend
legend("topright",ticks,lty=1,col=1:8,lwd=1,cex=0.73)
dev.off()




















