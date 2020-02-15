rm(list=ls())
library(reshape2)

#Time: Seconds after midnight with decimal precision of at least milliseconds and up to nanoseconds 
#depending on the period requested

#Price: Dollar price times 10000 (i.e. a stock price of $91.14 is given by 911400)

#ORDER TYPES
#1: Submission of a new limit order
#2: Cancellation (partial deletion of a limit order)
#3: Deletion (total deletion of a limit order)
#4: Execution of a visible limit order
#5: Execution of a hidden limit order
#6: Indicates a cross trade, e.g. auction trade
#7: Trading halt indicator (detailed information below)

#DIRECTION
#-1: Sell limit order
#1: Buy limit order
#Note: Execution of a sell (buy) limit order corresponds to a buyer (seller) initiated trade, i.e. buy (sell) trade.

##READ IN RAW DATA
file<-"C:/DatiLocali/Dropbox/Trader Anonymity/Data/RawData/AAPL_2013-11-04_34200000_59400000_message_10.csv"
cols<-c("time","eventtype","orderID","size","price","direction","mpid")
mydata<-read.csv(file=file,header=F,col.names=cols,stringsAsFactors=F)
rm(file,cols)

##REMOVE EXECUTION OF HIDDEN ORDERS
mydata<-mydata[mydata$eventtype!=5,]

##ADJUST PRICE
mydata$price<-mydata$price/10000

##RECODE MPID
#mydata$mpid<-replace(mydata$mpid,mydata$mpid=="null",0)
#mydata$mpid<-replace(mydata$mpid,mydata$mpid=="UBSS",1)
#mydata$mpid<-replace(mydata$mpid,mydata$mpid=="TMBR",2)
#mydata$mpid<-replace(mydata$mpid,mydata$mpid=="SBSH",3)
#mydata$mpid<-as.integer(mydata$mpid)

##GENERATE CLOCK TIME
mydata$clocktime<-format(as.POSIXct((mydata$time),origin="1970-01-01",tz="UTC"),"%H:%M:%OS6")

##SEPARATE SUBMISSIONS AND EXECUTIONS/CANCELLATIONS
sub<-mydata[mydata$eventtype==1,]
execanc<-mydata[mydata$eventtype==2|mydata$eventtype==3|mydata$eventtype==4,]
rm(mydata)

##TEST TIME: 10:30am

testtime<-10.5*60*60

#find submissions at or before this timestamp
subt<-sub[which(sub$time<=testtime),]
#find orders that have been executed or cancelled prior to timestamp
execanct<-execanc[which(execanc$orderID%in%subt$orderID&execanc$time<=testtime),]
#sum across executions and cancellations (may be several partial or combo)
totalect<-aggregate(execanct$size,by=list(Category=execanct$orderID),FUN=sum); colnames(totalect)<-c("orderID","size")
#discard orders that have been completely executed/cancelled by timestamp
i<-match(subt$orderID,totalect$orderID)
subt$execanc<-totalect$size[i]; rm(i) #this is the amount of the submitted order that has been executed or cancelled as of timestamp
subt$execanc<-replace(subt$execanc,is.na(subt$execanc)==1,0) #replace missing with 0
subt$remainder<-subt$size-subt$execanc #this is the amount of the order remaining as of the timestamp
  #q<-which(subt$remainder<0) #sanity check
subt<-subt[subt$remainder!=0,] #these should represent the orders that have not yet been execulated
rm(execanct,totalect)

#aggregate across MPID type, price and direction
subt$temp<-paste(subt$mpid,subt$price,subt$direction,sep=";")
temp<-aggregate(subt$size,by=list(Category=subt$temp),FUN=sum)

s<-t(matrix(unlist(strsplit(temp$Category,split=";")),3,nrow(temp)))
temp$Category<-NULL
temp$mpid<-s[,1]
temp$price<-as.numeric(s[,2])
temp$direction<-as.integer(s[,3])
rm(s)


##subt[which(subt$orderID=="1059989"),]
#totalect[which(totalect$orderID=="1059989"),]



