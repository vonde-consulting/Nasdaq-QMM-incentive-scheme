rm(list=ls(all=TRUE))
library(foreign)
library(plyr)

consolidateVariables<-function(i){
  
  ticks<-c("AAPL","CSCO","EBAY","FB","GOOG","INTC","MSFT","YHOO")
  dates<-c("04","05","06","07","08","12","13","14","15","18","19","20","21","22")
  #windows<-c(120,60,30,10)
  windows<-c(30)

    for (k in 1:length(windows)){
      
      #i<-1
      #k<-1
      w<-windows[k]
      j<-1
      
      #mypath<-paste("D:/Data/Cleaned_Data/Data/Variables/")
      #mypath<-"C:/Projects/Trader Anonymity/Data/"
      mypath<-paste("Data/",w,"/",sep="")
      mydata<-read.table(file=paste(mypath,ticks[i],"_2013-11-",dates[j],"_",w,"sec.rda",sep=""))
      
      mydata$DAY<-rep(as.numeric(dates[j]),nrow(mydata))

      for (j in 2:length(dates)){
        
        mydata2<-read.table(file=paste(mypath,ticks[i],"_2013-11-",dates[j],"_",w,"sec.rda",sep=""))
        mydata2$DAY<-rep(as.numeric(dates[j]),nrow(mydata2))
        
        mydata<-rbind(mydata,mydata2)
        
      }
      
      write.table(mydata,file=paste(mypath,ticks[i],"_",w,"sec.rda",sep=""))
      
    }
  
  return(i)
  
}

# CLUSTER ----------------------------------------------------------------

ID <- as.integer(Sys.getenv("SGE_TASK_ID"))
s <- consolidateVariables(ID)