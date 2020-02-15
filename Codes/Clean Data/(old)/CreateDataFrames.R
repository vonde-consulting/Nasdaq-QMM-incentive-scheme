rm(list=ls(all=TRUE))
library(foreign)

if (TRUE){
head<-"C:/DatiLocali/"
windows<-c(120,60,30,10)
ticks<-c("AAPL","CSCO","EBAY","FB","GOOG","INTC","MSFT","YHOO")

k<-3
w<-windows[k]

#unstandardized data
i<-1

mypath<-paste(head,"Dropbox/Projects/Trader Anonymity/Data/",sep="")

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

###remove AAPL

q<-which(mydata$TICK==1)
mydata<-mydata[-q,]
ticks<-ticks[-1]
mydata$TICK<-(mydata$TICK-1)

###calculate instruments

qq<-which(colnames(mydata)%in%c('MPID.SUB.RATIO','MPID.SUB.BUY.RATIO','MPID.SUB.SELL.RATIO',
                                'MPID.CANC.RATIO','MPID.CANC.BUY.RATIO','MPID.CANC.SELL.RATIO',
                                'MPID.EXE.RATIO','MPID.EXE.BUY.RATIO','MPID.EXE.SELL.RATIO'))
qq2<-which(colnames(mydata)%in%c('SUB.ALL.NUM','SUB.BUY.NUM','SUB.SELL.NUM',
                                 'CANC.ALL.NUM','CANC.BUY.NUM','CANC.SELL.NUM',
                                 'EXE.ALL.NUM','EXE.BUY.NUM','EXE.SELL.NUM'))

qq<-c(qq,qq2)

for (jjj in 1:length(qq)){
  
  j<-qq[jjj]  
  
  i<-1
  
  tick<-ticks[i]
  mydatatemp<-mydata[mydata$TICK!=i,]
  MPID<-mydatatemp[,j]
  temp<-rowMeans(matrix(MPID,nrow=length(unique(mydatatemp$TIMEPOINT))),na.rm=T)
  INST<-temp
  rm(temp)
  
  for (i in 2:length(ticks)){
    
    #i<-2
    
    tick<-ticks[i]
    mydatatemp<-mydata[mydata$TICK!=i,]
    MPID<-mydatatemp[,j]
    temp<-rowMeans(matrix(MPID,nrow=length(unique(mydatatemp$TIMEPOINT))),na.rm=T)
    INST<-c(INST,temp)
    rm(temp,mydatatemp)
    
  }
  
  if (jjj==1){mydata$MPID.SUB.INST<-INST}
  if (jjj==2){mydata$MPID.SUB.BUY.INST<-INST}
  if (jjj==3){mydata$MPID.SUB.SELL.INST<-INST}
  if (jjj==4){mydata$MPID.CANC.INST<-INST}
  if (jjj==5){mydata$MPID.CANC.BUY.INST<-INST}
  if (jjj==6){mydata$MPID.CANC.SELL.INST<-INST}
  if (jjj==7){mydata$MPID.EXE.INST<-INST}
  if (jjj==8){mydata$MPID.EXE.BUY.INST<-INST}
  if (jjj==9){mydata$MPID.EXE.SELL.INST<-INST}
  if (jjj==10){mydata$SUB.ALL.NUM.INST<-INST}
  if (jjj==11){mydata$SUB.BUY.NUM.INST<-INST}
  if (jjj==12){mydata$SUB.SELL.NUM.INST<-INST}
  if (jjj==13){mydata$CANC.ALL.NUM.INST<-INST}
  if (jjj==14){mydata$CANC.BUY.NUM.INST<-INST}
  if (jjj==15){mydata$CANC.SELL.NUM.INST<-INST}
  if (jjj==16){mydata$EXE.ALL.NUM.INST<-INST}
  if (jjj==17){mydata$EXE.BUY.NUM.INST<-INST}
  if (jjj==18){mydata$EXE.SELL.NUM.INST<-INST}
  
  rm(INST)
  
}

##instrument validity

qq<-c('MPID.SUB.RATIO','MPID.SUB.BUY.RATIO','MPID.SUB.SELL.RATIO',
      'MPID.CANC.RATIO','MPID.CANC.BUY.RATIO','MPID.CANC.SELL.RATIO',
      'MPID.EXE.RATIO','MPID.EXE.BUY.RATIO','MPID.EXE.SELL.RATIO')
qq2<-c('SUB.ALL.NUM','SUB.BUY.NUM','SUB.SELL.NUM','CANC.ALL.NUM','CANC.BUY.NUM','CANC.SELL.NUM',
       'EXE.ALL.NUM','EXE.BUY.NUM','EXE.SELL.NUM')
qq<-c(qq,qq2)

qq2<-c('MPID.SUB.INST','MPID.SUB.BUY.INST','MPID.SUB.SELL.INST',
       'MPID.CANC.INST','MPID.CANC.BUY.INST','MPID.CANC.SELL.INST',
       'MPID.EXE.INST','MPID.EXE.BUY.INST','MPID.EXE.SELL.INST',
       'SUB.ALL.NUM.INST','SUB.BUY.NUM.INST','SUB.SELL.NUM.INST','CANC.ALL.NUM.INST','CANC.BUY.NUM.INST','CANC.SELL.NUM.INST',
       'EXE.ALL.NUM.INST','EXE.BUY.NUM.INST','EXE.SELL.NUM.INST')
validity<-matrix(NA,length(qq),length(ticks))
confintervals<-matrix(NA,length(qq),length(ticks)*2)
for (jjj in 1:length(qq)){
  v1<-which(colnames(mydata)==qq[jjj])  
  v2<-which(colnames(mydata)==qq2[jjj])  
  for (vv in 1:length(ticks)){
    vv1<-which(mydata$TICK==vv)
    validity[jjj,vv]<-cor(mydata[vv1,v1],mydata[vv1,v2])
    test<-cor.test(mydata[vv1,v1],mydata[vv1,v2])
    confintervals[jjj,(2*(vv-1)+1):(2*(vv-1)+2)]<-test$conf.int
  }  
}

colnames(validity)<-ticks
rownames(validity)<-qq

#filename<-paste(mypath,"Results/", folder, "/INSTRUMENT_VALIDITY_BYFIRM_",w,"sec.xlsx",sep="")
#write.xlsx(validity,file=filename,append=FALSE) 
#write.xlsx(confintervals,sheetName="Sheet2",file=filename,append=TRUE) 

vars<-c('TICK','TIMEPOINT','DAY','MPID.SUB.RATIO','MPID.SUB.BUY.RATIO','MPID.SUB.SELL.RATIO','MPID.SUB.INST',
        'MPID.SUB.BUY.INST','MPID.SUB.SELL.INST','SUB.ALL.NUM','SUB.BUY.NUM','SUB.SELL.NUM',
        'SUB.ALL.NUM.INST','SUB.BUY.NUM.INST','SUB.SELL.NUM.INST',"RELSPR","RELEFFSPR.EXE.REP","RELRLZSPR.EXE.REP.PRE",
        "RELRLZSPR.EXE.REP.POST",
        "VOL.GRID.SHORT.PRE","VOL.SD","VOL.GRID.LONG.PRE","VOL.GRID.SHORT.POST","VOL.GRID.LONG.POST",
        "AGGR.REL.TOTAL","ORSZ.DVOL.TOTAL","AGGR.REL.MPID",
        "ORSZ.DVOL.MPID","SUB.ALL.DVOL","EXE.ALL.DVOL","SUB.BUY.DVOL","EXE.BUY.DVOL","SUB.SELL.DVOL","EXE.SELL.DVOL",
        "DEPTH.TOTAL.DVOL","DEPTH.BUY.DVOL","DEPTH.SELL.DVOL","RELDPR.MID","NEG.DUMMY",'OPEN','HASBROUCK.MAX')
mydata<-mydata[,vars]

filename<-paste(mypath,"DATA_INST_NOAAPL_",w,"sec.Rds",sep="")
save(mydata_unst,mydata,file=filename)

filename<-paste(mypath,"DATA_INST_NOAAPL_",w,"sec.dta",sep="")
write.dta(mydata,file=filename) 

}

##############################################################################################

if (TRUE){

rm(list=ls(all=TRUE))
library(foreign)


head<-"C:/DatiLocali/"
windows<-c(120,60,30,10)
ticks<-c("AAPL","CSCO","EBAY","FB","GOOG","INTC","MSFT","YHOO")

k<-3
w<-windows[k]

#unstandardized data
i<-1

mypath<-paste(head,"Dropbox/Projects/Trader Anonymity/Data/",sep="")

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

###calculate instruments

qq<-which(colnames(mydata)%in%c('MPID.SUB.RATIO','MPID.SUB.BUY.RATIO','MPID.SUB.SELL.RATIO',
                                'MPID.CANC.RATIO','MPID.CANC.BUY.RATIO','MPID.CANC.SELL.RATIO',
                                'MPID.EXE.RATIO','MPID.EXE.BUY.RATIO','MPID.EXE.SELL.RATIO'))
qq2<-which(colnames(mydata)%in%c('SUB.ALL.NUM','SUB.BUY.NUM','SUB.SELL.NUM',
                                 'CANC.ALL.NUM','CANC.BUY.NUM','CANC.SELL.NUM',
                                 'EXE.ALL.NUM','EXE.BUY.NUM','EXE.SELL.NUM'))

qq<-c(qq,qq2)

for (jjj in 1:length(qq)){
  
  j<-qq[jjj]  
  
  i<-1
  
  tick<-ticks[i]
  mydatatemp<-mydata[mydata$TICK!=i,]
  MPID<-mydatatemp[,j]
  temp<-rowMeans(matrix(MPID,nrow=length(unique(mydatatemp$TIMEPOINT))),na.rm=T)
  INST<-temp
  rm(temp)
  
  for (i in 2:length(ticks)){
    
    #i<-2
    
    tick<-ticks[i]
    mydatatemp<-mydata[mydata$TICK!=i,]
    MPID<-mydatatemp[,j]
    temp<-rowMeans(matrix(MPID,nrow=length(unique(mydatatemp$TIMEPOINT))),na.rm=T)
    INST<-c(INST,temp)
    rm(temp,mydatatemp)
    
  }
  
  if (jjj==1){mydata$MPID.SUB.INST<-INST}
  if (jjj==2){mydata$MPID.SUB.BUY.INST<-INST}
  if (jjj==3){mydata$MPID.SUB.SELL.INST<-INST}
  if (jjj==4){mydata$MPID.CANC.INST<-INST}
  if (jjj==5){mydata$MPID.CANC.BUY.INST<-INST}
  if (jjj==6){mydata$MPID.CANC.SELL.INST<-INST}
  if (jjj==7){mydata$MPID.EXE.INST<-INST}
  if (jjj==8){mydata$MPID.EXE.BUY.INST<-INST}
  if (jjj==9){mydata$MPID.EXE.SELL.INST<-INST}
  if (jjj==10){mydata$SUB.ALL.NUM.INST<-INST}
  if (jjj==11){mydata$SUB.BUY.NUM.INST<-INST}
  if (jjj==12){mydata$SUB.SELL.NUM.INST<-INST}
  if (jjj==13){mydata$CANC.ALL.NUM.INST<-INST}
  if (jjj==14){mydata$CANC.BUY.NUM.INST<-INST}
  if (jjj==15){mydata$CANC.SELL.NUM.INST<-INST}
  if (jjj==16){mydata$EXE.ALL.NUM.INST<-INST}
  if (jjj==17){mydata$EXE.BUY.NUM.INST<-INST}
  if (jjj==18){mydata$EXE.SELL.NUM.INST<-INST}
  
  rm(INST)
  
}

##instrument validity

qq<-c('MPID.SUB.RATIO','MPID.SUB.BUY.RATIO','MPID.SUB.SELL.RATIO',
      'MPID.CANC.RATIO','MPID.CANC.BUY.RATIO','MPID.CANC.SELL.RATIO',
      'MPID.EXE.RATIO','MPID.EXE.BUY.RATIO','MPID.EXE.SELL.RATIO')
qq2<-c('SUB.ALL.NUM','SUB.BUY.NUM','SUB.SELL.NUM','CANC.ALL.NUM','CANC.BUY.NUM','CANC.SELL.NUM',
       'EXE.ALL.NUM','EXE.BUY.NUM','EXE.SELL.NUM')
qq<-c(qq,qq2)

qq2<-c('MPID.SUB.INST','MPID.SUB.BUY.INST','MPID.SUB.SELL.INST',
       'MPID.CANC.INST','MPID.CANC.BUY.INST','MPID.CANC.SELL.INST',
       'MPID.EXE.INST','MPID.EXE.BUY.INST','MPID.EXE.SELL.INST',
       'SUB.ALL.NUM.INST','SUB.BUY.NUM.INST','SUB.SELL.NUM.INST','CANC.ALL.NUM.INST','CANC.BUY.NUM.INST','CANC.SELL.NUM.INST',
       'EXE.ALL.NUM.INST','EXE.BUY.NUM.INST','EXE.SELL.NUM.INST')
validity<-matrix(NA,length(qq),length(ticks))
confintervals<-matrix(NA,length(qq),length(ticks)*2)
for (jjj in 1:length(qq)){
  v1<-which(colnames(mydata)==qq[jjj])  
  v2<-which(colnames(mydata)==qq2[jjj])  
  for (vv in 1:length(ticks)){
    vv1<-which(mydata$TICK==vv)
    validity[jjj,vv]<-cor(mydata[vv1,v1],mydata[vv1,v2])
    test<-cor.test(mydata[vv1,v1],mydata[vv1,v2])
    confintervals[jjj,(2*(vv-1)+1):(2*(vv-1)+2)]<-test$conf.int
  }  
}

colnames(validity)<-ticks
rownames(validity)<-qq

#filename<-paste(mypath,"Results/", folder, "/INSTRUMENT_VALIDITY_BYFIRM_",w,"sec.xlsx",sep="")
#write.xlsx(validity,file=filename,append=FALSE) 
#write.xlsx(confintervals,sheetName="Sheet2",file=filename,append=TRUE) 

vars<-c('TICK','TIMEPOINT','DAY','MPID.SUB.RATIO','MPID.SUB.BUY.RATIO','MPID.SUB.SELL.RATIO','MPID.SUB.INST',
        'MPID.SUB.BUY.INST','MPID.SUB.SELL.INST','SUB.ALL.NUM','SUB.BUY.NUM','SUB.SELL.NUM',
        'SUB.ALL.NUM.INST','SUB.BUY.NUM.INST','SUB.SELL.NUM.INST',"RELSPR","RELEFFSPR.EXE.REP","RELRLZSPR.EXE.REP.PRE",
        "RELRLZSPR.EXE.REP.POST",
        "VOL.GRID.SHORT.PRE","VOL.SD","VOL.GRID.LONG.PRE","VOL.GRID.SHORT.POST","VOL.GRID.LONG.POST",
        "AGGR.REL.TOTAL","ORSZ.DVOL.TOTAL","AGGR.REL.MPID",
        "ORSZ.DVOL.MPID","SUB.ALL.DVOL","EXE.ALL.DVOL","SUB.BUY.DVOL","EXE.BUY.DVOL","SUB.SELL.DVOL","EXE.SELL.DVOL",
        "DEPTH.TOTAL.DVOL","DEPTH.BUY.DVOL","DEPTH.SELL.DVOL","RELDPR.MID","NEG.DUMMY",'OPEN','HASBROUCK.MAX')
mydata<-mydata[,vars]

filename<-paste(mypath,"DATA_INST_",w,"sec.Rds",sep="")
save(mydata_unst,mydata,file=filename)

filename<-paste(mypath,"DATA_INST_",w,"sec.dta",sep="")
write.dta(mydata,file=filename) 

}

#########################################################################

if (TRUE){
  
rm(list=ls(all=TRUE))
library(foreign)
library(plyr)
  
head<-"C:/DatiLocali/"
windows<-c(120,60,30,10)
ticks<-c("AAPL","CSCO","EBAY","FB","GOOG","INTC","MSFT","YHOO")

k<-3
w<-windows[k]

#unstandardized data
i<-1

mypath<-paste(head,"Dropbox/Projects/Trader Anonymity/Data/",sep="")

mydata<-read.table(file=paste(mypath,ticks[i],"_",w,"sec.rda",sep=""))
hasbrouck<-read.table(file=paste(mypath,"/Hasbrouck/Hasbrouck_",ticks[i],"_",w,"sec.rda",sep=""))
mydata$HASBROUCK.MAX<-hasbrouck$HASBROUCK.MAX
mydata$HASBROUCK.MEAN<-hasbrouck$HASBROUCK.MEAN
mydata$TICK<-rep(ticks[i],nrow(mydata))
mydata$TIMEPOINT<-seq(1,nrow(mydata),1)
rm(hasbrouck)

mydata$MPID.SUB.RATIO<-mydata$TMBR.SUB/mydata$SUB.ALL.NUM
mydata$MPID.SUB.BUY.RATIO<-mydata$TMBR.SUB.BUY/mydata$SUB.BUY.NUM
mydata$MPID.SUB.SELL.RATIO<-mydata$TMBR.SUB.SELL/mydata$SUB.SELL.NUM  

mydata$MPID.CANC.RATIO<-mydata$TMBR.CANC/mydata$CANC.ALL.NUM
mydata$MPID.CANC.BUY.RATIO<-mydata$TMBR.CANC.BUY/mydata$CANC.BUY.NUM
mydata$MPID.CANC.SELL.RATIO<-mydata$TMBR.CANC.SELL/mydata$CANC.SELL.NUM  

mydata$MPID.EXE.RATIO<-mydata$TMBR.EXE/mydata$EXE.ALL.NUM
mydata$MPID.EXE.BUY.RATIO<-mydata$TMBR.EXE.BUY/mydata$EXE.BUY.NUM
mydata$MPID.EXE.SELL.RATIO<-mydata$TMBR.EXE.SELL/mydata$EXE.SELL.NUM    

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
  
  mydata2$MPID.SUB.RATIO<-mydata2$TMBR.SUB/mydata2$SUB.ALL.NUM
  mydata2$MPID.SUB.BUY.RATIO<-mydata2$TMBR.SUB.BUY/mydata2$SUB.BUY.NUM
  mydata2$MPID.SUB.SELL.RATIO<-mydata2$TMBR.SUB.SELL/mydata2$SUB.SELL.NUM  
  
  mydata2$MPID.CANC.RATIO<-mydata2$TMBR.CANC/mydata2$CANC.ALL.NUM
  mydata2$MPID.CANC.BUY.RATIO<-mydata2$TMBR.CANC.BUY/mydata2$CANC.BUY.NUM
  mydata2$MPID.CANC.SELL.RATIO<-mydata2$TMBR.CANC.SELL/mydata2$CANC.SELL.NUM  
  
  mydata2$MPID.EXE.RATIO<-mydata2$TMBR.EXE/mydata2$EXE.ALL.NUM
  mydata2$MPID.EXE.BUY.RATIO<-mydata2$TMBR.EXE.BUY/mydata2$EXE.BUY.NUM
  mydata2$MPID.EXE.SELL.RATIO<-mydata2$TMBR.EXE.SELL/mydata2$EXE.SELL.NUM     
  
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

mydata<-read.table(file=paste(mypath,ticks[i],"_",w,"sec.rda",sep=""))
hasbrouck<-read.table(file=paste(mypath,"/Hasbrouck/Hasbrouck_",ticks[i],"_",w,"sec.rda",sep=""))
mydata$HASBROUCK.MAX<-hasbrouck$HASBROUCK.MAX
mydata$HASBROUCK.MEAN<-hasbrouck$HASBROUCK.MEAN
mydata$TICK<-rep(ticks[i],nrow(mydata))
mydata$TIMEPOINT<-seq(1,nrow(mydata),1)
rm(hasbrouck)

mydata$MPID.SUB.RATIO<-mydata$TMBR.SUB/mydata$SUB.ALL.NUM
mydata$MPID.SUB.BUY.RATIO<-mydata$TMBR.SUB.BUY/mydata$SUB.BUY.NUM
mydata$MPID.SUB.SELL.RATIO<-mydata$TMBR.SUB.SELL/mydata$SUB.SELL.NUM  

mydata$MPID.CANC.RATIO<-mydata$TMBR.CANC/mydata$CANC.ALL.NUM
mydata$MPID.CANC.BUY.RATIO<-mydata$TMBR.CANC.BUY/mydata$CANC.BUY.NUM
mydata$MPID.CANC.SELL.RATIO<-mydata$TMBR.CANC.SELL/mydata$CANC.SELL.NUM  

mydata$MPID.EXE.RATIO<-mydata$TMBR.EXE/mydata$EXE.ALL.NUM
mydata$MPID.EXE.BUY.RATIO<-mydata$TMBR.EXE.BUY/mydata$EXE.BUY.NUM
mydata$MPID.EXE.SELL.RATIO<-mydata$TMBR.EXE.SELL/mydata$EXE.SELL.NUM     

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
  
  mydata2$MPID.SUB.RATIO<-mydata2$TMBR.SUB/mydata2$SUB.ALL.NUM
  mydata2$MPID.SUB.BUY.RATIO<-mydata2$TMBR.SUB.BUY/mydata2$SUB.BUY.NUM
  mydata2$MPID.SUB.SELL.RATIO<-mydata2$TMBR.SUB.SELL/mydata2$SUB.SELL.NUM  
  
  mydata2$MPID.CANC.RATIO<-mydata2$TMBR.CANC/mydata2$CANC.ALL.NUM
  mydata2$MPID.CANC.BUY.RATIO<-mydata2$TMBR.CANC.BUY/mydata2$CANC.BUY.NUM
  mydata2$MPID.CANC.SELL.RATIO<-mydata2$TMBR.CANC.SELL/mydata2$CANC.SELL.NUM  
  
  mydata2$MPID.EXE.RATIO<-mydata2$TMBR.EXE/mydata2$EXE.ALL.NUM
  mydata2$MPID.EXE.BUY.RATIO<-mydata2$TMBR.EXE.BUY/mydata2$EXE.BUY.NUM
  mydata2$MPID.EXE.SELL.RATIO<-mydata2$TMBR.EXE.SELL/mydata2$EXE.SELL.NUM     
  
  
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

###remove AAPL

q<-which(mydata$TICK==1)
mydata<-mydata[-q,]
ticks<-ticks[-1]
mydata$TICK<-(mydata$TICK-1)

###calculate instruments

qq<-which(colnames(mydata)%in%c('MPID.SUB.RATIO','MPID.SUB.BUY.RATIO','MPID.SUB.SELL.RATIO',
                                'MPID.CANC.RATIO','MPID.CANC.BUY.RATIO','MPID.CANC.SELL.RATIO',
                                'MPID.EXE.RATIO','MPID.EXE.BUY.RATIO','MPID.EXE.SELL.RATIO'))
qq2<-which(colnames(mydata)%in%c('SUB.ALL.NUM','SUB.BUY.NUM','SUB.SELL.NUM',
                                 'CANC.ALL.NUM','CANC.BUY.NUM','CANC.SELL.NUM',
                                 'EXE.ALL.NUM','EXE.BUY.NUM','EXE.SELL.NUM'))

qq<-c(qq,qq2)

for (jjj in 1:length(qq)){
  
  j<-qq[jjj]  
  
  i<-1
  
  tick<-ticks[i]
  mydatatemp<-mydata[mydata$TICK!=i,]
  MPID<-mydatatemp[,j]
  temp<-rowMeans(matrix(MPID,nrow=length(unique(mydatatemp$TIMEPOINT))),na.rm=T)
  INST<-temp
  rm(temp)
  
  for (i in 2:length(ticks)){
    
    #i<-2
    
    tick<-ticks[i]
    mydatatemp<-mydata[mydata$TICK!=i,]
    MPID<-mydatatemp[,j]
    temp<-rowMeans(matrix(MPID,nrow=length(unique(mydatatemp$TIMEPOINT))),na.rm=T)
    INST<-c(INST,temp)
    rm(temp,mydatatemp)
    
  }
  
  if (jjj==1){mydata$MPID.SUB.INST<-INST}
  if (jjj==2){mydata$MPID.SUB.BUY.INST<-INST}
  if (jjj==3){mydata$MPID.SUB.SELL.INST<-INST}
  if (jjj==4){mydata$MPID.CANC.INST<-INST}
  if (jjj==5){mydata$MPID.CANC.BUY.INST<-INST}
  if (jjj==6){mydata$MPID.CANC.SELL.INST<-INST}
  if (jjj==7){mydata$MPID.EXE.INST<-INST}
  if (jjj==8){mydata$MPID.EXE.BUY.INST<-INST}
  if (jjj==9){mydata$MPID.EXE.SELL.INST<-INST}
  if (jjj==10){mydata$SUB.ALL.NUM.INST<-INST}
  if (jjj==11){mydata$SUB.BUY.NUM.INST<-INST}
  if (jjj==12){mydata$SUB.SELL.NUM.INST<-INST}
  if (jjj==13){mydata$CANC.ALL.NUM.INST<-INST}
  if (jjj==14){mydata$CANC.BUY.NUM.INST<-INST}
  if (jjj==15){mydata$CANC.SELL.NUM.INST<-INST}
  if (jjj==16){mydata$EXE.ALL.NUM.INST<-INST}
  if (jjj==17){mydata$EXE.BUY.NUM.INST<-INST}
  if (jjj==18){mydata$EXE.SELL.NUM.INST<-INST}
  
  rm(INST)
  
}

##instrument validity

qq<-c('MPID.SUB.RATIO','MPID.SUB.BUY.RATIO','MPID.SUB.SELL.RATIO',
      'MPID.CANC.RATIO','MPID.CANC.BUY.RATIO','MPID.CANC.SELL.RATIO',
      'MPID.EXE.RATIO','MPID.EXE.BUY.RATIO','MPID.EXE.SELL.RATIO')
qq2<-c('SUB.ALL.NUM','SUB.BUY.NUM','SUB.SELL.NUM','CANC.ALL.NUM','CANC.BUY.NUM','CANC.SELL.NUM',
       'EXE.ALL.NUM','EXE.BUY.NUM','EXE.SELL.NUM')
qq<-c(qq,qq2)

qq2<-c('MPID.SUB.INST','MPID.SUB.BUY.INST','MPID.SUB.SELL.INST',
       'MPID.CANC.INST','MPID.CANC.BUY.INST','MPID.CANC.SELL.INST',
       'MPID.EXE.INST','MPID.EXE.BUY.INST','MPID.EXE.SELL.INST',
       'SUB.ALL.NUM.INST','SUB.BUY.NUM.INST','SUB.SELL.NUM.INST','CANC.ALL.NUM.INST','CANC.BUY.NUM.INST','CANC.SELL.NUM.INST',
       'EXE.ALL.NUM.INST','EXE.BUY.NUM.INST','EXE.SELL.NUM.INST')
validity<-matrix(NA,length(qq),length(ticks))
confintervals<-matrix(NA,length(qq),length(ticks)*2)
for (jjj in 1:length(qq)){
  v1<-which(colnames(mydata)==qq[jjj])  
  v2<-which(colnames(mydata)==qq2[jjj])  
  for (vv in 1:length(ticks)){
    vv1<-which(mydata$TICK==vv)
    validity[jjj,vv]<-cor(mydata[vv1,v1],mydata[vv1,v2])
    test<-cor.test(mydata[vv1,v1],mydata[vv1,v2])
    confintervals[jjj,(2*(vv-1)+1):(2*(vv-1)+2)]<-test$conf.int
  }  
}

colnames(validity)<-ticks
rownames(validity)<-qq

#filename<-paste(mypath,"Results/", folder, "/INSTRUMENT_VALIDITY_BYFIRM_",w,"sec.xlsx",sep="")
#write.xlsx(validity,file=filename,append=FALSE) 
#write.xlsx(confintervals,sheetName="Sheet2",file=filename,append=TRUE) 

vars<-c('TICK','TIMEPOINT','DAY','MPID.SUB.RATIO','MPID.SUB.BUY.RATIO','MPID.SUB.SELL.RATIO','MPID.SUB.INST',
        'MPID.SUB.BUY.INST','MPID.SUB.SELL.INST','SUB.ALL.NUM','SUB.BUY.NUM','SUB.SELL.NUM',
        'SUB.ALL.NUM.INST','SUB.BUY.NUM.INST','SUB.SELL.NUM.INST',"RELSPR","RELEFFSPR.EXE.REP","RELRLZSPR.EXE.REP.PRE",
        "RELRLZSPR.EXE.REP.POST",
        "VOL.GRID.SHORT.PRE","VOL.SD","VOL.GRID.LONG.PRE","VOL.GRID.SHORT.POST","VOL.GRID.LONG.POST",
        "AGGR.REL.TOTAL","ORSZ.DVOL.TOTAL","AGGR.REL.TMBR",
        "ORSZ.DVOL.TMBR","SUB.ALL.DVOL","EXE.ALL.DVOL","SUB.BUY.DVOL","EXE.BUY.DVOL","SUB.SELL.DVOL","EXE.SELL.DVOL",
        "DEPTH.TOTAL.DVOL","DEPTH.BUY.DVOL","DEPTH.SELL.DVOL","RELDPR.MID","NEG.DUMMY",'OPEN','HASBROUCK.MAX')
mydata<-mydata[,vars]

mydata<-rename(mydata, c("AGGR.REL.TMBR"="AGGR.REL.MPID", "ORSZ.DVOL.TMBR"="ORSZ.DVOL.MPID"))

filename<-paste(mypath,"DATA_TMBR_INST_NOAAPL_",w,"sec.Rds",sep="")
save(mydata_unst,mydata,file=filename)

filename<-paste(mypath,"DATA_TMBR_INST_NOAAPL_",w,"sec.dta",sep="")
write.dta(mydata,file=filename) 

}

###########################################################################
###TMBR + AAPL


if (TRUE){

  rm(list=ls(all=TRUE))
  library(foreign)
  library(plyr)
  
  head<-"C:/DatiLocali/"
  windows<-c(120,60,30,10)
  ticks<-c("AAPL","CSCO","EBAY","FB","GOOG","INTC","MSFT","YHOO")
  
  k<-3
  w<-windows[k]
  
  #unstandardized data
  i<-1
  
  mypath<-paste(head,"Dropbox/Projects/Trader Anonymity/Data/",sep="")
  
  mydata<-read.table(file=paste(mypath,ticks[i],"_",w,"sec.rda",sep=""))
  hasbrouck<-read.table(file=paste(mypath,"/Hasbrouck/Hasbrouck_",ticks[i],"_",w,"sec.rda",sep=""))
  mydata$HASBROUCK.MAX<-hasbrouck$HASBROUCK.MAX
  mydata$HASBROUCK.MEAN<-hasbrouck$HASBROUCK.MEAN
  mydata$TICK<-rep(ticks[i],nrow(mydata))
  mydata$TIMEPOINT<-seq(1,nrow(mydata),1)
  rm(hasbrouck)
  
  mydata$MPID.SUB.RATIO<-mydata$TMBR.SUB/mydata$SUB.ALL.NUM
  mydata$MPID.SUB.BUY.RATIO<-mydata$TMBR.SUB.BUY/mydata$SUB.BUY.NUM
  mydata$MPID.SUB.SELL.RATIO<-mydata$TMBR.SUB.SELL/mydata$SUB.SELL.NUM  
  
  mydata$MPID.CANC.RATIO<-mydata$TMBR.CANC/mydata$CANC.ALL.NUM
  mydata$MPID.CANC.BUY.RATIO<-mydata$TMBR.CANC.BUY/mydata$CANC.BUY.NUM
  mydata$MPID.CANC.SELL.RATIO<-mydata$TMBR.CANC.SELL/mydata$CANC.SELL.NUM  
  
  mydata$MPID.EXE.RATIO<-mydata$TMBR.EXE/mydata$EXE.ALL.NUM
  mydata$MPID.EXE.BUY.RATIO<-mydata$TMBR.EXE.BUY/mydata$EXE.BUY.NUM
  mydata$MPID.EXE.SELL.RATIO<-mydata$TMBR.EXE.SELL/mydata$EXE.SELL.NUM    
  
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
    
    mydata2$MPID.SUB.RATIO<-mydata2$TMBR.SUB/mydata2$SUB.ALL.NUM
    mydata2$MPID.SUB.BUY.RATIO<-mydata2$TMBR.SUB.BUY/mydata2$SUB.BUY.NUM
    mydata2$MPID.SUB.SELL.RATIO<-mydata2$TMBR.SUB.SELL/mydata2$SUB.SELL.NUM  
    
    mydata2$MPID.CANC.RATIO<-mydata2$TMBR.CANC/mydata2$CANC.ALL.NUM
    mydata2$MPID.CANC.BUY.RATIO<-mydata2$TMBR.CANC.BUY/mydata2$CANC.BUY.NUM
    mydata2$MPID.CANC.SELL.RATIO<-mydata2$TMBR.CANC.SELL/mydata2$CANC.SELL.NUM  
    
    mydata2$MPID.EXE.RATIO<-mydata2$TMBR.EXE/mydata2$EXE.ALL.NUM
    mydata2$MPID.EXE.BUY.RATIO<-mydata2$TMBR.EXE.BUY/mydata2$EXE.BUY.NUM
    mydata2$MPID.EXE.SELL.RATIO<-mydata2$TMBR.EXE.SELL/mydata2$EXE.SELL.NUM     
    
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
  
  mydata<-read.table(file=paste(mypath,ticks[i],"_",w,"sec.rda",sep=""))
  hasbrouck<-read.table(file=paste(mypath,"/Hasbrouck/Hasbrouck_",ticks[i],"_",w,"sec.rda",sep=""))
  mydata$HASBROUCK.MAX<-hasbrouck$HASBROUCK.MAX
  mydata$HASBROUCK.MEAN<-hasbrouck$HASBROUCK.MEAN
  mydata$TICK<-rep(ticks[i],nrow(mydata))
  mydata$TIMEPOINT<-seq(1,nrow(mydata),1)
  rm(hasbrouck)
  
  mydata$MPID.SUB.RATIO<-mydata$TMBR.SUB/mydata$SUB.ALL.NUM
  mydata$MPID.SUB.BUY.RATIO<-mydata$TMBR.SUB.BUY/mydata$SUB.BUY.NUM
  mydata$MPID.SUB.SELL.RATIO<-mydata$TMBR.SUB.SELL/mydata$SUB.SELL.NUM  
  
  mydata$MPID.CANC.RATIO<-mydata$TMBR.CANC/mydata$CANC.ALL.NUM
  mydata$MPID.CANC.BUY.RATIO<-mydata$TMBR.CANC.BUY/mydata$CANC.BUY.NUM
  mydata$MPID.CANC.SELL.RATIO<-mydata$TMBR.CANC.SELL/mydata$CANC.SELL.NUM  
  
  mydata$MPID.EXE.RATIO<-mydata$TMBR.EXE/mydata$EXE.ALL.NUM
  mydata$MPID.EXE.BUY.RATIO<-mydata$TMBR.EXE.BUY/mydata$EXE.BUY.NUM
  mydata$MPID.EXE.SELL.RATIO<-mydata$TMBR.EXE.SELL/mydata$EXE.SELL.NUM     
  
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
    
    mydata2$MPID.SUB.RATIO<-mydata2$TMBR.SUB/mydata2$SUB.ALL.NUM
    mydata2$MPID.SUB.BUY.RATIO<-mydata2$TMBR.SUB.BUY/mydata2$SUB.BUY.NUM
    mydata2$MPID.SUB.SELL.RATIO<-mydata2$TMBR.SUB.SELL/mydata2$SUB.SELL.NUM  
    
    mydata2$MPID.CANC.RATIO<-mydata2$TMBR.CANC/mydata2$CANC.ALL.NUM
    mydata2$MPID.CANC.BUY.RATIO<-mydata2$TMBR.CANC.BUY/mydata2$CANC.BUY.NUM
    mydata2$MPID.CANC.SELL.RATIO<-mydata2$TMBR.CANC.SELL/mydata2$CANC.SELL.NUM  
    
    mydata2$MPID.EXE.RATIO<-mydata2$TMBR.EXE/mydata2$EXE.ALL.NUM
    mydata2$MPID.EXE.BUY.RATIO<-mydata2$TMBR.EXE.BUY/mydata2$EXE.BUY.NUM
    mydata2$MPID.EXE.SELL.RATIO<-mydata2$TMBR.EXE.SELL/mydata2$EXE.SELL.NUM     
    
    
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
  
  ###remove AAPL
  
  #q<-which(mydata$TICK==1)
  #mydata<-mydata[-q,]
  #ticks<-ticks[-1]
  #mydata$TICK<-(mydata$TICK-1)
  
  ###calculate instruments
  
  qq<-which(colnames(mydata)%in%c('MPID.SUB.RATIO','MPID.SUB.BUY.RATIO','MPID.SUB.SELL.RATIO',
                                  'MPID.CANC.RATIO','MPID.CANC.BUY.RATIO','MPID.CANC.SELL.RATIO',
                                  'MPID.EXE.RATIO','MPID.EXE.BUY.RATIO','MPID.EXE.SELL.RATIO'))
  qq2<-which(colnames(mydata)%in%c('SUB.ALL.NUM','SUB.BUY.NUM','SUB.SELL.NUM',
                                   'CANC.ALL.NUM','CANC.BUY.NUM','CANC.SELL.NUM',
                                   'EXE.ALL.NUM','EXE.BUY.NUM','EXE.SELL.NUM'))
  
  qq<-c(qq,qq2)
  
  for (jjj in 1:length(qq)){
    
    j<-qq[jjj]  
    
    i<-1
    
    tick<-ticks[i]
    mydatatemp<-mydata[mydata$TICK!=i,]
    MPID<-mydatatemp[,j]
    temp<-rowMeans(matrix(MPID,nrow=length(unique(mydatatemp$TIMEPOINT))),na.rm=T)
    INST<-temp
    rm(temp)
    
    for (i in 2:length(ticks)){
      
      #i<-2
      
      tick<-ticks[i]
      mydatatemp<-mydata[mydata$TICK!=i,]
      MPID<-mydatatemp[,j]
      temp<-rowMeans(matrix(MPID,nrow=length(unique(mydatatemp$TIMEPOINT))),na.rm=T)
      INST<-c(INST,temp)
      rm(temp,mydatatemp)
      
    }
    
    if (jjj==1){mydata$MPID.SUB.INST<-INST}
    if (jjj==2){mydata$MPID.SUB.BUY.INST<-INST}
    if (jjj==3){mydata$MPID.SUB.SELL.INST<-INST}
    if (jjj==4){mydata$MPID.CANC.INST<-INST}
    if (jjj==5){mydata$MPID.CANC.BUY.INST<-INST}
    if (jjj==6){mydata$MPID.CANC.SELL.INST<-INST}
    if (jjj==7){mydata$MPID.EXE.INST<-INST}
    if (jjj==8){mydata$MPID.EXE.BUY.INST<-INST}
    if (jjj==9){mydata$MPID.EXE.SELL.INST<-INST}
    if (jjj==10){mydata$SUB.ALL.NUM.INST<-INST}
    if (jjj==11){mydata$SUB.BUY.NUM.INST<-INST}
    if (jjj==12){mydata$SUB.SELL.NUM.INST<-INST}
    if (jjj==13){mydata$CANC.ALL.NUM.INST<-INST}
    if (jjj==14){mydata$CANC.BUY.NUM.INST<-INST}
    if (jjj==15){mydata$CANC.SELL.NUM.INST<-INST}
    if (jjj==16){mydata$EXE.ALL.NUM.INST<-INST}
    if (jjj==17){mydata$EXE.BUY.NUM.INST<-INST}
    if (jjj==18){mydata$EXE.SELL.NUM.INST<-INST}
    
    rm(INST)
    
  }
  
  ##instrument validity
  
  qq<-c('MPID.SUB.RATIO','MPID.SUB.BUY.RATIO','MPID.SUB.SELL.RATIO',
        'MPID.CANC.RATIO','MPID.CANC.BUY.RATIO','MPID.CANC.SELL.RATIO',
        'MPID.EXE.RATIO','MPID.EXE.BUY.RATIO','MPID.EXE.SELL.RATIO')
  qq2<-c('SUB.ALL.NUM','SUB.BUY.NUM','SUB.SELL.NUM','CANC.ALL.NUM','CANC.BUY.NUM','CANC.SELL.NUM',
         'EXE.ALL.NUM','EXE.BUY.NUM','EXE.SELL.NUM')
  qq<-c(qq,qq2)
  
  qq2<-c('MPID.SUB.INST','MPID.SUB.BUY.INST','MPID.SUB.SELL.INST',
         'MPID.CANC.INST','MPID.CANC.BUY.INST','MPID.CANC.SELL.INST',
         'MPID.EXE.INST','MPID.EXE.BUY.INST','MPID.EXE.SELL.INST',
         'SUB.ALL.NUM.INST','SUB.BUY.NUM.INST','SUB.SELL.NUM.INST','CANC.ALL.NUM.INST','CANC.BUY.NUM.INST','CANC.SELL.NUM.INST',
         'EXE.ALL.NUM.INST','EXE.BUY.NUM.INST','EXE.SELL.NUM.INST')
  validity<-matrix(NA,length(qq),length(ticks))
  confintervals<-matrix(NA,length(qq),length(ticks)*2)
  for (jjj in 1:length(qq)){
    v1<-which(colnames(mydata)==qq[jjj])  
    v2<-which(colnames(mydata)==qq2[jjj])  
    for (vv in 1:length(ticks)){
      vv1<-which(mydata$TICK==vv)
      validity[jjj,vv]<-cor(mydata[vv1,v1],mydata[vv1,v2])
      test<-cor.test(mydata[vv1,v1],mydata[vv1,v2])
      confintervals[jjj,(2*(vv-1)+1):(2*(vv-1)+2)]<-test$conf.int
    }  
  }
  
  colnames(validity)<-ticks
  rownames(validity)<-qq
  
  #filename<-paste(mypath,"Results/", folder, "/INSTRUMENT_VALIDITY_BYFIRM_",w,"sec.xlsx",sep="")
  #write.xlsx(validity,file=filename,append=FALSE) 
  #write.xlsx(confintervals,sheetName="Sheet2",file=filename,append=TRUE) 
  
  vars<-c('TICK','TIMEPOINT','DAY','MPID.SUB.RATIO','MPID.SUB.BUY.RATIO','MPID.SUB.SELL.RATIO','MPID.SUB.INST',
          'MPID.SUB.BUY.INST','MPID.SUB.SELL.INST','SUB.ALL.NUM','SUB.BUY.NUM','SUB.SELL.NUM',
          'SUB.ALL.NUM.INST','SUB.BUY.NUM.INST','SUB.SELL.NUM.INST',"RELSPR","RELEFFSPR.EXE.REP","RELRLZSPR.EXE.REP.PRE",
          "RELRLZSPR.EXE.REP.POST",
          "VOL.GRID.SHORT.PRE","VOL.SD","VOL.GRID.LONG.PRE","VOL.GRID.SHORT.POST","VOL.GRID.LONG.POST",
          "AGGR.REL.TOTAL","ORSZ.DVOL.TOTAL","AGGR.REL.TMBR",
          "ORSZ.DVOL.TMBR","SUB.ALL.DVOL","EXE.ALL.DVOL","SUB.BUY.DVOL","EXE.BUY.DVOL","SUB.SELL.DVOL","EXE.SELL.DVOL",
          "DEPTH.TOTAL.DVOL","DEPTH.BUY.DVOL","DEPTH.SELL.DVOL","RELDPR.MID","NEG.DUMMY",'OPEN','HASBROUCK.MAX')
mydata<-mydata[,vars]

mydata<-rename(mydata, c("AGGR.REL.TMBR"="AGGR.REL.MPID", "ORSZ.DVOL.TMBR"="ORSZ.DVOL.MPID"))
  
filename<-paste(mypath,"DATA_TMBR_INST_",w,"sec.Rds",sep="")
save(mydata_unst,mydata,file=filename)

filename<-paste(mypath,"DATA_TMBR_INST_",w,"sec.dta",sep="")
write.dta(mydata,file=filename) 

}