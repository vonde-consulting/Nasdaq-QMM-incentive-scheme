rm(list=ls())
library(data.table)
library(bizdays)
library(moments)

directory1<-"W:/LOBSTER/quotes"
directory2<-"W:/LOBSTER/allMessages"
directory3<-"W:/LOBSTER/merged"
# Trading hours (start & end)
startTrad=9.5*60*60;       # 9:30:00.000 in ms after midnight
endTrad=16*60*60;        # 16:00:00.000 in ms after midnight

#get list of all files in /quotes/ and /allMessages/ directories
folders1<-list.files(directory1)
folders2<-list.files(directory2)
#get list of tickets
firms<-vapply(strsplit(folders1,"_"),`[`,1,FUN.VALUE=character(1))
#get list of dates
dates<-as.POSIXlt(seq(as.Date("2020-02-01"), as.Date("2020-02-28"),"days"),format="%Y-%m-%d")
dates<-dates[!dates$wday%in%c(6,0)] #get rid of weekdays
dates<-dates[dates!=as.POSIXlt(as.Date("2020-02-17"),format="%Y-%m-%d")] #get rid of President's Day
#define summary stats
stats<-c("mpid.ratio","num.rmm","mean.subs","median.subs","sd.subs","skew.subs","hhi")

######################################################################
################CYCLE THROUGH STOCKS AND DAYS
######################################################################
fullstats<-array(dim=c(length(dates),length(firms),length(stats)),dimnames=list(dates=as.character(dates),firms=firms,stats=stats))

for (i in 1:length(firms)){
for (j in 1:length(dates)){
  
cat(paste0("Firm ",i," out of ",length(firms),": date ",j," out of ",length(dates),"\n"))

###LOAD ORDERBOOK DATA
filename<-paste0(directory1,"/",folders1[i],"/",firms[i],"_",dates[j],"_","34200000_57600000_orderbook_1.csv")
data_o<-read.table(file=filename,sep=",",stringsAsFactors=F,col.names=c("askprice","asksize","bidprice","bidsize"))
rm(filename)
###GET TIMESTAMPS FROM MESSAGE_1 FILE
filename<-paste0(directory1,"/",folders1[i],"/",firms[i],"_",dates[j],"_","34200000_57600000_message_1.csv")
data_m<-read.table(file=filename,sep=",",stringsAsFactors=F,col.names=c("time","eventtype","orderID","size","price","direction","mpid"))
rm(filename)
###MERGE THEM
data_o$time<-data_m$time
data_o$clocktime<-format(as.POSIXct((data_o$time),origin="1970-01-01",tz="UTC"),"%H:%M:%OS6")
data_o<-unique(data_o)
rm(data_m)
data_o<-as.data.table(data_o)
setkey(data_o,clocktime)
###LOAD MESSAGE_0 FILE
filename<-paste0(directory2,"/",folders2[i],"/",firms[i],"_",dates[j],"_","14400000_72000000_message_0.csv")
data_m<-read.table(file=filename,sep=",",stringsAsFactors=F,col.names=c("time","eventtype","orderID","size","price","direction","mpid"))
rm(filename)
data_m$clocktime<-format(as.POSIXct((data_m$time),origin="1970-01-01",tz="UTC"),"%H:%M:%OS6")
data_m<-as.data.table(data_m)
setkey(data_m,clocktime)
###MERGE DATASETS
data<-data_o[data_m,roll=T,mult="last"]
data$i.time<-NULL
rm(data_o,data_m)

###STANDARD CLEANING ALGORITHM
#only keep messages that are part of the continuous trading period
#data<-data[data$time>=startTrad&data$time<=endTrad,]
#check for trading halts
tradehaltIdx<-which(data$eventtype==7&data$direction==-1);
tradequoteIdx<-which(data$eventtype==7 & data$direction==0);
traderesumeIdx<-which(data$eventtype==7 & data$direction==1);
  if(length(tradehaltIdx)==0 & length(tradequoteIdx)==0  & length(traderesumeIdx)==0 ){print("No trading halts detected.")}
  if(length(tradehaltIdx) !=0){cat("Data contains trading halt! at time stamp(s)", data$clocktime[tradehaltIdx],"\n" )}
  if(length(tradequoteIdx) !=0){cat(" Data contains quoting message! at time stamp(s)", data$clocktime[tradequoteIdx], "\n")}
  if(length(traderesumeIdx) !=0){cat(" Data resumes trading! at time stamp(s) ", data$clocktime[traderesumeIdx],"\n")}
rm(tradehaltIdx,tradequoteIdx,traderesumeIdx)

mpidratio<-sum(data$mpid!="null")/nrow(data)
mpidnum<-length(unique(data$mpid[data$mpid!="null"]))
mpidmean<-mean(table(data$mpid[data$mpid!="null"]),na.rm=T)
mpidmedian<-median(table(data$mpid[data$mpid!="null"]),na.rm=T)
mpidsd<-sd(table(data$mpid[data$mpid!="null"]),na.rm=T)
mpidskew<-skewness(table(data$mpid[data$mpid!="null"]),na.rm=T)
mpidhhi<-sum(((table(data$mpid[data$mpid!="null"])/sum(table(data$mpid[data$mpid!="null"])))*100)^2,na.rm=T)

fullstats[j,i,1]<-mpidratio
fullstats[j,i,2]<-mpidnum
fullstats[j,i,3]<-mpidmean
fullstats[j,i,4]<-mpidmedian
fullstats[j,i,5]<-mpidsd
fullstats[j,i,6]<-mpidskew
fullstats[j,i,7]<-mpidhhi

f<-paste0(directory3,"/",firms[i])
if (file.exists(f)==F){dir.create(f)}
filename<-paste0(f,"/",firms[i],"_",dates[j],".Rds")
save(data,file=filename)


}
}

filename<-paste0("W:/LOBSTER/MPIDStats_v1_20200323.Rds")
save(fullstats,file=filename)

filename<-paste0("W:/LOBSTER/MPIDStats_v1_20200323.Rds")
load(file=filename)

b<-apply(fullstats, c(2,3), mean)
max(b[,1])
min(b[,1])
mean(b[,2])
median(b[,2])
mean(b[,7])
median(b[,7])



