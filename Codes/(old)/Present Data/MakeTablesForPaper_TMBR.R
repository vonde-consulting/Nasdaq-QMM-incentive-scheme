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
head<-"C:/DatiLocali/"
mypath<-paste(head,"Dropbox/Projects/Trader Anonymity/Data/Results/",sep="")

#########PANEL POST RATIO

k<-3
w<-windows[k]

########RELATIVE BID-ASK SPREADS

jj<-1

folder<-"Panel_Post_Ratio_TMBR_NoInst"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit11<-fit3
rm(fit1,fit2,fit3)

folder<-"Panel_Post_Ratio_TMBR_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit12<-fit1
rm(fit1,fit3)

folder<-"Panel_Post_Ratio_TMBR_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit13<-fit3
rm(fit1,fit3)

jj<-2

folder<-"Panel_Post_Ratio_TMBR_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit14<-fit3
rm(fit1,fit3)

jj<-3

folder<-"Panel_Post_Ratio_TMBR_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit15<-fit3
rm(fit1,fit3)

jj<-1

folder<-"Panel_Post_Level_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit16<-fit3
rm(fit1,fit3)

stderr1<-coeftest(fit11,vcovHC(fit11,type="HC0",cluster="group")) 
stderr2<-coeftest(fit12,vcovHC(fit12,type="HC0",cluster="group")) 
stderr3<-coeftest(fit13,vcovHC(fit13,type="HC0",cluster="group")) 
stderr4<-coeftest(fit14,vcovHC(fit14,type="HC0",cluster="group")) 
stderr5<-coeftest(fit15,vcovHC(fit15,type="HC0",cluster="group")) 
stderr6<-coeftest(fit16,vcovHC(fit16,type="HC0",cluster="group")) 

ses<-list(stderr1[,2],stderr2[,2],stderr3[,2],stderr4[,2],stderr5[,2],stderr6[,2])
tstats<-list(stderr1[,3],stderr2[,3],stderr3[,3],stderr4[,3],stderr5[,3],stderr6[,3])
pvals<-list(stderr1[,4],stderr2[,4],stderr3[,4],stderr4[,4],stderr5[,4],stderr6[,4])

sg<-stargazer(fit11,fit12,fit13,fit14,fit15,fit16, title="Post Panel Regression Results: Relative Bid-Ask Spreads",
              align=FALSE, no.space=TRUE,p=pvals, se=ses,t=tstats)

filename<-paste(mypath,"SummaryTables/SummaryTable_TMBR_RelSpr",w,"_sec_",jj,".txt",sep="")
cat(paste(sg, collapse = "\n"), "\n", file=filename, append=FALSE)

########VOLATILTIY

jj<-6

folder<-"Panel_Post_Ratio_TMBR_NoInst"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit11<-fit3
rm(fit1,fit2,fit3)

folder<-"Panel_Post_Ratio_TMBR_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit12<-fit1
rm(fit1,fit3)

folder<-"Panel_Post_Ratio_TMBR_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit13<-fit3
rm(fit1,fit3)

jj<-7

folder<-"Panel_Post_Ratio_TMBR_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit14<-fit3
rm(fit1,fit3)

jj<-8

folder<-"Panel_Post_Ratio_TMBR_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit15<-fit3
rm(fit1,fit3)

jj<-6

folder<-"Panel_Post_Level_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit16<-fit3
rm(fit1,fit3)

stderr1<-coeftest(fit11,vcovHC(fit11,type="HC0",cluster="group")) 
stderr2<-coeftest(fit12,vcovHC(fit12,type="HC0",cluster="group")) 
stderr3<-coeftest(fit13,vcovHC(fit13,type="HC0",cluster="group")) 
stderr4<-coeftest(fit14,vcovHC(fit14,type="HC0",cluster="group")) 
stderr5<-coeftest(fit15,vcovHC(fit15,type="HC0",cluster="group")) 
stderr6<-coeftest(fit16,vcovHC(fit16,type="HC0",cluster="group")) 

ses<-list(stderr1[,2],stderr2[,2],stderr3[,2],stderr4[,2],stderr5[,2],stderr6[,2])
tstats<-list(stderr1[,3],stderr2[,3],stderr3[,3],stderr4[,3],stderr5[,3],stderr6[,3])
pvals<-list(stderr1[,4],stderr2[,4],stderr3[,4],stderr4[,4],stderr5[,4],stderr6[,4])

sg<-stargazer(fit11,fit12,fit13,fit14,fit15,fit16, title="Post Panel Regression Results: Volatility",
              align=FALSE, no.space=TRUE,p=pvals, se=ses,t=tstats)

filename<-paste(mypath,"SummaryTables/SummaryTable_TMBR_Vol",w,"_sec_",jj,".txt",sep="")
cat(paste(sg, collapse = "\n"), "\n", file=filename, append=FALSE)


########SUBMISSION VOLUME

jj<-11

folder<-"Panel_Post_Ratio_TMBR_NoInst"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit11<-fit3
rm(fit1,fit2,fit3)

folder<-"Panel_Post_Ratio_TMBR_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit12<-fit1
rm(fit1,fit3)

folder<-"Panel_Post_Ratio_TMBR_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit13<-fit3
rm(fit1,fit3)

jj<-12

folder<-"Panel_Post_Ratio_TMBR_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit14<-fit3
rm(fit1,fit3)

jj<-13

folder<-"Panel_Post_Ratio_TMBR_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit15<-fit3
rm(fit1,fit3)

jj<-11

folder<-"Panel_Post_Level_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit16<-fit3
rm(fit1,fit3)

stderr1<-coeftest(fit11,vcovHC(fit11,type="HC0",cluster="group")) 
stderr2<-coeftest(fit12,vcovHC(fit12,type="HC0",cluster="group")) 
stderr3<-coeftest(fit13,vcovHC(fit13,type="HC0",cluster="group")) 
stderr4<-coeftest(fit14,vcovHC(fit14,type="HC0",cluster="group")) 
stderr5<-coeftest(fit15,vcovHC(fit15,type="HC0",cluster="group")) 
stderr6<-coeftest(fit16,vcovHC(fit16,type="HC0",cluster="group")) 

ses<-list(stderr1[,2],stderr2[,2],stderr3[,2],stderr4[,2],stderr5[,2],stderr6[,2])
tstats<-list(stderr1[,3],stderr2[,3],stderr3[,3],stderr4[,3],stderr5[,3],stderr6[,3])
pvals<-list(stderr1[,4],stderr2[,4],stderr3[,4],stderr4[,4],stderr5[,4],stderr6[,4])

sg<-stargazer(fit11,fit12,fit13,fit14,fit15,fit16, title="Post Panel Regression Results: Submissions",
              align=FALSE, no.space=TRUE,p=pvals, se=ses,t=tstats)

filename<-paste(mypath,"SummaryTables/SummaryTable_TMBR_Sub",w,"_sec_",jj,".txt",sep="")
cat(paste(sg, collapse = "\n"), "\n", file=filename, append=FALSE)

########EXECUTION VOLUME

jj<-16

folder<-"Panel_Post_Ratio_TMBR_NoInst"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit11<-fit3
rm(fit1,fit2,fit3)

folder<-"Panel_Post_Ratio_TMBR_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit12<-fit1
rm(fit1,fit3)

folder<-"Panel_Post_Ratio_TMBR_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit13<-fit3
rm(fit1,fit3)

jj<-17

folder<-"Panel_Post_Ratio_TMBR_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit14<-fit3
rm(fit1,fit3)

jj<-18

folder<-"Panel_Post_Ratio_TMBR_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit15<-fit3
rm(fit1,fit3)

jj<-16

folder<-"Panel_Post_Level_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit16<-fit3
rm(fit1,fit3)

stderr1<-coeftest(fit11,vcovHC(fit11,type="HC0",cluster="group")) 
stderr2<-coeftest(fit12,vcovHC(fit12,type="HC0",cluster="group")) 
stderr3<-coeftest(fit13,vcovHC(fit13,type="HC0",cluster="group")) 
stderr4<-coeftest(fit14,vcovHC(fit14,type="HC0",cluster="group")) 
stderr5<-coeftest(fit15,vcovHC(fit15,type="HC0",cluster="group")) 
stderr6<-coeftest(fit16,vcovHC(fit16,type="HC0",cluster="group")) 

ses<-list(stderr1[,2],stderr2[,2],stderr3[,2],stderr4[,2],stderr5[,2],stderr6[,2])
tstats<-list(stderr1[,3],stderr2[,3],stderr3[,3],stderr4[,3],stderr5[,3],stderr6[,3])
pvals<-list(stderr1[,4],stderr2[,4],stderr3[,4],stderr4[,4],stderr5[,4],stderr6[,4])

sg<-stargazer(fit11,fit12,fit13,fit14,fit15,fit16, title="Post Panel Regression Results: Executions",
              align=FALSE, no.space=TRUE,p=pvals, se=ses,t=tstats)

filename<-paste(mypath,"SummaryTables/SummaryTable_TMBR_Exe",w,"_sec_",jj,".txt",sep="")
cat(paste(sg, collapse = "\n"), "\n", file=filename, append=FALSE)

########DEPTH

jj<-21

folder<-"Panel_Post_Ratio_TMBR_NoInst"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit11<-fit3
rm(fit1,fit2,fit3)

folder<-"Panel_Post_Ratio_TMBR_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit12<-fit1
rm(fit1,fit3)

folder<-"Panel_Post_Ratio_TMBR_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit13<-fit3
rm(fit1,fit3)

jj<-22

folder<-"Panel_Post_Ratio_TMBR_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit14<-fit3
rm(fit1,fit3)

jj<-23

folder<-"Panel_Post_Ratio_TMBR_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit15<-fit3
rm(fit1,fit3)

jj<-21

folder<-"Panel_Post_Level_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit16<-fit3
rm(fit1,fit3)

stderr1<-coeftest(fit11,vcovHC(fit11,type="HC0",cluster="group")) 
stderr2<-coeftest(fit12,vcovHC(fit12,type="HC0",cluster="group")) 
stderr3<-coeftest(fit13,vcovHC(fit13,type="HC0",cluster="group")) 
stderr4<-coeftest(fit14,vcovHC(fit14,type="HC0",cluster="group")) 
stderr5<-coeftest(fit15,vcovHC(fit15,type="HC0",cluster="group")) 
stderr6<-coeftest(fit16,vcovHC(fit16,type="HC0",cluster="group")) 

ses<-list(stderr1[,2],stderr2[,2],stderr3[,2],stderr4[,2],stderr5[,2],stderr6[,2])
tstats<-list(stderr1[,3],stderr2[,3],stderr3[,3],stderr4[,3],stderr5[,3],stderr6[,3])
pvals<-list(stderr1[,4],stderr2[,4],stderr3[,4],stderr4[,4],stderr5[,4],stderr6[,4])

sg<-stargazer(fit11,fit12,fit13,fit14,fit15,fit16, title="Post Panel Regression Results: Depth",
              align=FALSE, no.space=TRUE,p=pvals, se=ses,t=tstats)

filename<-paste(mypath,"SummaryTables/SummaryTable_TMBR_Depth",w,"_sec_",jj,".txt",sep="")
cat(paste(sg, collapse = "\n"), "\n", file=filename, append=FALSE)

########HASBROUCK

jj<-26

folder<-"Panel_Post_Ratio_TMBR_NoInst"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit11<-fit3
rm(fit1,fit2,fit3)

folder<-"Panel_Post_Ratio_TMBR_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit12<-fit1
rm(fit1,fit3)

folder<-"Panel_Post_Ratio_TMBR_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit13<-fit3
rm(fit1,fit3)

jj<-27

folder<-"Panel_Post_Ratio_TMBR_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit14<-fit3
rm(fit1,fit3)

jj<-28

folder<-"Panel_Post_Ratio_TMBR_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit15<-fit3
rm(fit1,fit3)

jj<-26

folder<-"Panel_Post_Level_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit16<-fit3
rm(fit1,fit3)

stderr1<-coeftest(fit11,vcovHC(fit11,type="HC0",cluster="group")) 
stderr2<-coeftest(fit12,vcovHC(fit12,type="HC0",cluster="group")) 
stderr3<-coeftest(fit13,vcovHC(fit13,type="HC0",cluster="group")) 
stderr4<-coeftest(fit14,vcovHC(fit14,type="HC0",cluster="group")) 
stderr5<-coeftest(fit15,vcovHC(fit15,type="HC0",cluster="group")) 
stderr6<-coeftest(fit16,vcovHC(fit16,type="HC0",cluster="group")) 

ses<-list(stderr1[,2],stderr2[,2],stderr3[,2],stderr4[,2],stderr5[,2],stderr6[,2])
tstats<-list(stderr1[,3],stderr2[,3],stderr3[,3],stderr4[,3],stderr5[,3],stderr6[,3])
pvals<-list(stderr1[,4],stderr2[,4],stderr3[,4],stderr4[,4],stderr5[,4],stderr6[,4])

sg<-stargazer(fit11,fit12,fit13,fit14,fit15,fit16, title="Post Panel Regression Results: Hasbrouck Measure",
              align=FALSE, no.space=TRUE,p=pvals, se=ses,t=tstats)

filename<-paste(mypath,"SummaryTables/SummaryTable_TMBR_HasbroucK_",w,"_sec_",jj,".txt",sep="")
cat(paste(sg, collapse = "\n"), "\n", file=filename, append=FALSE)

########PRICE CHANGES

jj<-29

folder<-"Panel_Post_Ratio_TMBR_NoInst"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit11<-fit3
rm(fit1,fit2,fit3)

folder<-"Panel_Post_Ratio_TMBR_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit12<-fit1
rm(fit1,fit3)

folder<-"Panel_Post_Ratio_TMBR_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit13<-fit3
rm(fit1,fit3)

jj<-30

folder<-"Panel_Post_Ratio_TMBR_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit14<-fit3
rm(fit1,fit3)

jj<-31

folder<-"Panel_Post_Ratio_TMBR_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit15<-fit3
rm(fit1,fit3)

jj<-29

folder<-"Panel_Post_Level_2SLS"
filename<-paste(mypath,folder,"/Panel_Regression_Post_",w,"sec_",jj,".rda",sep="")
load(file=filename)
fit16<-fit3
rm(fit1,fit3)

stderr1<-coeftest(fit11,vcovHC(fit11,type="HC0",cluster="group")) 
stderr2<-coeftest(fit12,vcovHC(fit12,type="HC0",cluster="group")) 
stderr3<-coeftest(fit13,vcovHC(fit13,type="HC0",cluster="group")) 
stderr4<-coeftest(fit14,vcovHC(fit14,type="HC0",cluster="group")) 
stderr5<-coeftest(fit15,vcovHC(fit15,type="HC0",cluster="group")) 
stderr6<-coeftest(fit16,vcovHC(fit16,type="HC0",cluster="group")) 

ses<-list(stderr1[,2],stderr2[,2],stderr3[,2],stderr4[,2],stderr5[,2],stderr6[,2])
tstats<-list(stderr1[,3],stderr2[,3],stderr3[,3],stderr4[,3],stderr5[,3],stderr6[,3])
pvals<-list(stderr1[,4],stderr2[,4],stderr3[,4],stderr4[,4],stderr5[,4],stderr6[,4])

sg<-stargazer(fit11,fit12,fit13,fit14,fit15,fit16, title="Post Panel Regression Results: Relative Price Changes",
              align=FALSE, no.space=TRUE,p=pvals, se=ses,t=tstats)

filename<-paste(mypath,"SummaryTables/SummaryTable_TMBR_RelDPr_",w,"_sec_",jj,".txt",sep="")
cat(paste(sg, collapse = "\n"), "\n", file=filename, append=FALSE)


