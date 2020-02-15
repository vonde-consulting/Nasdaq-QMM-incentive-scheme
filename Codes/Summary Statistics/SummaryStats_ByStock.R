rm(list=ls(all=TRUE))
library(xlsx)

ticks<-c("AAPL","CSCO","EBAY","FB","GOOG","INTC","MSFT","YHOO")
dates<-c("04","05","06","07","08","12","13","14","15","18","19","20","21","22")
direc<-"C:/Users/Julia/Dropbox/Projects/Trader Anonymity/"

excelfilename<-paste(direc,"Data/SummaryStats/SummaryStats_byStock.xlsx",sep="")

write.xlsx(0,file=excelfilename,row.names=FALSE,col.names=FALSE)
wb<-loadWorkbook(excelfilename)
sheets<-getSheets(wb)
sheet<-sheets[[1]] 
colnames<-c("","\\textbf{Mean}","\\textbf{Median}","\\textbf{Std. Dev.}","\\textbf{Min.}","\\textbf{Max.}")  

for (i in 1:length(ticks)){
 
rownames<-c("Average Stock Price (USD)","Daily Return ($\\%$)","Intradaily Return Volatility ($\\%$)","Number of Daily Orders",
            "Number of Daily Trades","$\\%$ MPID")
output<-matrix(NA,length(dates),length(rownames))
stats<-matrix(NA,length(rownames),(length(colnames)-1))
rownames(stats)<-rownames

for (j in 1:length(dates)){
  
  cat(paste("Stock ",i," Day ",j,"\n",sep=""))

  mypath=paste(direc,"Data/CleanedData/",sep="")
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

  ##AVERAGE STOCK PRICE
  output[j,1]<-mean(price[(event==4|event==5)],na.rm=T)
  
  ##ENDING PRICE
  q<-which(event==4|event==5)
  output[j,2]<-price[max(q)]
  rm(q) 
  
  ##AVERAGE INTRADAILY RETURN VOLATILTIY
  intervals<-seq(9.5*60*60,16*60*60,5*60)  
  n<-length(intervals)-1
  RV<-matrix(NA,1,n)
  p2<-price[(event==4|event==5)]
  t2<-time[(event==4|event==5)]
  
  for (ii in 1:n){
    
    iia<-min(which(t2>intervals[ii]))
    iib<-max(which(t2<=intervals[ii+1]))
    
    if (is.infinite(iia)|is.infinite(iib)){next}
    
    RV[ii]<-(log(p2[iib])-log(p2[iia]))^2
    
  }
  
  output[j,3]<-sqrt(sum(RV,na.rm=T))

  ##NUMBER OF DAILY ORDERS
  output[j,4]<-sum(event==1)
  
  ##NUMBER OF DAILY TRADES
  output[j,5]<-sum(event==4|event==5)
  
  ##PERCENTAGE MPID
  output[j,6]<-sum(event==1&mpid!=0)/sum(event==1)
  
}

output2<-output

temp<-output2[,2]
temp<-diff(temp)/temp[1:(length(temp)-1)]
temp<-c(NA,temp)
output2[,2]<-temp*100

stats[,1]<-colMeans(output2,na.rm=T)
stats[,2]<-apply(output2, 2, median,na.rm=T)
stats[,3]<-apply(output2, 2, sd,na.rm=T)
stats[,4]<-apply(output2, 2, min,na.rm=T)
stats[,5]<-apply(output2, 2, max,na.rm=T)
stats[3,]<-stats[3,]*100

stats[1:3,]<-round(stats[1:3,],digits=2)
stats[4:5,]<-round(stats[4:5,],digits=0)
stats[6,]<-paste("$",round(100*stats[6,],digits=2),"\\%$",sep="")

title<-paste("\\multicolumn{6}{l}{\\textbf{(",i,")"," ",ticks[i],"}}",sep="")

addDataFrame(title,sheet,col.names=FALSE,row.names=FALSE,startRow=2+7*(i-1),startColumn=1)
addDataFrame(stats,sheet,col.names=FALSE,row.names=TRUE,startRow=3+7*(i-1),startColumn=1)

rm(output2,stats,title)

}

addDataFrame(t(as.matrix(colnames)),sheet,col.names=FALSE,row.names=FALSE,startRow=1,startColumn=1)
saveWorkbook(wb,file=excelfilename)
  