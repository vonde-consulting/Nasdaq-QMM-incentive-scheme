rm(list=ls(all=TRUE))
library(stargazer)
library(car)
library(plm)
library(tseries)
library(xlsx)
library(lmtest)
library(sandwich)
library(quantmod)
library(AER)

windows<-c(120,60,30,10)
mypath<-"C:/Users/user/Dropbox/Projects/Trader Anonymity/Data/Results/"

#########PANEL POST RATIO

k<-3
w<-windows[k]

for (jj in 1:25){

folder<-"Panel_Post_Ratio_NoInst"
filename<-paste(mypath,folder,"/1MPIDLags_5DepVarLags/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit11<-fit3
rm(fit1,fit2,fit3)

folder<-"Panel_Post_Ratio_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit12<-fit1
rm(fit1,fit3)

folder<-"Panel_Post_Ratio_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit13<-fit3
rm(fit1,fit3)

#folder<-"Panel_Post_Ratio"
#filename<-paste(mypath,folder,"/1MPIDLags_5DepVarLags_ExclAAPL2/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
#load(file=filename)
#fit14<-fit3
#rm(fit1,fit2,fit3)

folder<-"Panel_Post_Level_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit14<-fit3
rm(fit1,fit3)

stderr1<-coeftest(fit11,vcovHC(fit11,type="HC0",cluster="group")) 
stderr2<-coeftest(fit12,vcovHC(fit12,type="HC0",cluster="group")) 
stderr3<-coeftest(fit13,vcovHC(fit13,type="HC0",cluster="group")) 
stderr4<-coeftest(fit14,vcovHC(fit13,type="HC0",cluster="group")) 

ses<-list(stderr1[,2],stderr2[,2],stderr3[,2],stderr4[,2])
tstats<-list(stderr1[,3],stderr2[,3],stderr3[,3],stderr4[,3])
pvals<-list(stderr1[,4],stderr2[,4],stderr3[,4],stderr4[,4])

sg<-stargazer(fit11,fit12,fit13,fit14, title="Post Panel Regression Results: Relative Bid-Ask Spreads",
              align=FALSE, no.space=TRUE,p=pvals, se=ses,t=tstats)

filename<-paste(mypath,"/SummaryTable_",w,"_sec_",jj,".txt",sep="")
cat(paste(sg, collapse = "\n"), "\n", file=filename, append=FALSE)

}





