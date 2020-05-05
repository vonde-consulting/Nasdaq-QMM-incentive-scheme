rm(list = ls(all = TRUE))
library(foreign)

#######################################
#CHOICE VARIABLES
rootDirectory <- "W:/LOBSTER"
varDirectory <- paste0(rootDirectory, "/Variables")
saveDirectory <- paste0(rootDirectory, "/Stata")
w <- 60 #window length (in # of seconds)
#Trading hours (start & end)
startTrad <- 9.5 * 60 * 60
# 9:30:00.000 in ms after midnight
endTrad <- 16 * 60 * 60
# 16:00:00.000 in ms after midnight
int <-
  seq(startTrad, endTrad, w)
int <- int[1:(length(int) - 1)] #starting timestamp of either interval
#Variables to Include in Final Dataset
vars <- c(
  "TICK",
  "DATE",
  "TIME",
  "SUB.ALL.BUY.NUM",
  "SUB.ALL.SELL.NUM",
  "SUB.ALL.TOTAL.NUM",
  "EXE.ALL.BUY.NUM",
  "EXE.ALL.SELL.NUM",
  "EXE.ALL.TOTAL.NUM",
  "CANC.ALL.BUY.NUM",
  "CANC.ALL.SELL.NUM",
  "CANC.ALL.TOTAL.NUM",
  "SUB.MPID.BUY.NUM",
  "SUB.MPID.SELL.NUM",
  "SUB.MPID.TOTAL.NUM",
  "EXE.MPID.BUY.NUM",
  "EXE.MPID.SELL.NUM",
  "EXE.MPID.TOTAL.NUM",
  "CANC.MPID.BUY.NUM",
  "CANC.MPID.SELL.NUM",
  "CANC.MPID.TOTAL.NUM",
  "SUB.ALL.BUY.DVOL",
  "SUB.ALL.SELL.DVOL",
  "SUB.ALL.TOTAL.DVOL",
  "EXE.ALL.BUY.DVOL",
  "EXE.ALL.SELL.DVOL",
  "EXE.ALL.TOTAL.DVOL",
  "CANC.ALL.BUY.DVOL",
  "CANC.ALL.SELL.DVOL",
  "CANC.ALL.TOTAL.DVOL",
  "SUB.MPID.BUY.DVOL",
  "SUB.MPID.SELL.DVOL",
  "SUB.MPID.TOTAL.DVOL",
  "EXE.MPID.BUY.DVOL",
  "EXE.MPID.SELL.DVOL",
  "EXE.MPID.TOTAL.DVOL",
  "CANC.MPID.BUY.DVOL",
  "CANC.MPID.SELL.DVOL",
  "CANC.MPID.TOTAL.DVOL",
  "AGGR.REL.ALL",
  "AGGR.REL.MPID",
  "AGGR.REL.ANON",
  "ORSZ.DVOL.ALL",
  "ORSZ.DVOL.MPID",
  "ORSZ.DVOL.MPID",
  "DEPTH.BUY.DVOL",
  "DEPTH.SELL.DVOL",
  "DEPTH.TOTAL.DVOL",
  "D.MID.REL",
  "REL.SPR.TW",
  "VOL.RV.PRE",
  "VOL.RV.POST",
  "PRERR.MEAN",
  "PRERR.SD",
  "PRERR.MAX"
)
#######################################


###FULL LIST OF VARIABLES
#"SUB.ALL.BUY.NUM",
#"SUB.ALL.SELL.NUM",
#"SUB.ALL.TOTAL.NUM",
#"SUB.ALL.BUY.SVOL",
#"SUB.ALL.SELL.SVOL",
#"SUB.ALL.TOTAL.SVOL",
#"SUB.ALL.BUY.DVOL",
#"SUB.ALL.SELL.DVOL",
#"SUB.ALL.TOTAL.DVOL",
#"SUB.MPID.BUY.NUM",
#"SUB.MPID.SELL.NUM",
#"SUB.MPID.TOTAL.NUM",
#"SUB.MPID.BUY.SVOL",
#"SUB.MPID.SELL.SVOL",
#"SUB.MPID.TOTAL.SVOL",
#"SUB.MPID.BUY.DVOL",
#"SUB.MPID.SELL.DVOL",
#"SUB.MPID.TOTAL.DVOL",
#"SUB.ANON.BUY.NUM",
#"SUB.ANON.SELL.NUM",
#"SUB.ANON.TOTAL.NUM",
#"SUB.ANON.BUY.SVOL",
#"SUB.ANON.SELL.SVOL",
#"SUB.ANON.TOTAL.SVOL",
#"SUB.ANON.BUY.DVOL",
#"SUB.ANON.SELL.DVOL",
#"SUB.ANON.TOTAL.DVOL",
#"EXE.ALL.BUY.NUM",
#"EXE.ALL.SELL.NUM",
#"EXE.ALL.TOTAL.NUM",
#"EXE.ALL.BUY.SVOL",
#"EXE.ALL.SELL.SVOL",
#"EXE.ALL.TOTAL.SVOL",
#"EXE.ALL.BUY.DVOL",
#"EXE.ALL.SELL.DVOL",
#"EXE.ALL.TOTAL.DVOL",
#"EXE.MPID.BUY.NUM",
#"EXE.MPID.SELL.NUM",
#"EXE.MPID.TOTAL.NUM",
#"EXE.MPID.BUY.SVOL",
#"EXE.MPID.SELL.SVOL",
#"EXE.MPID.TOTAL.SVOL",
#"EXE.MPID.BUY.DVOL",
#"EXE.MPID.SELL.DVOL",
#"EXE.MPID.TOTAL.DVOL",
#"EXE.ANON.BUY.NUM",
#"EXE.ANON.SELL.NUM",
#"EXE.ANON.TOTAL.NUM",
#"EXE.ANON.BUY.SVOL",
#"EXE.ANON.SELL.SVOL",
#"EXE.ANON.TOTAL.SVOL",
#"EXE.ANON.BUY.DVOL",
#"EXE.ANON.SELL.DVOL",
#"EXE.ANON.TOTAL.DVOL",
#"CANC.ALL.BUY.NUM",
#"CANC.ALL.SELL.NUM",
#"CANC.ALL.TOTAL.NUM",
#"CANC.ALL.BUY.SVOL",
#"CANC.ALL.SELL.SVOL",
#"CANC.ALL.TOTAL.SVOL",
#"CANC.ALL.BUY.DVOL",
#"CANC.ALL.SELL.DVOL",
#"CANC.ALL.TOTAL.DVOL",
#"CANC.MPID.BUY.NUM",
#"CANC.MPID.SELL.NUM",
#"CANC.MPID.TOTAL.NUM",
#"CANC.MPID.BUY.SVOL",
#"CANC.MPID.SELL.SVOL",
#"CANC.MPID.TOTAL.SVOL",
#"CANC.MPID.BUY.DVOL",
#"CANC.MPID.SELL.DVOL",
#"CANC.MPID.TOTAL.DVOL",
#"CANC.ANON.BUY.NUM",
#"CANC.ANON.SELL.NUM",
#"CANC.ANON.TOTAL.NUM",
#"CANC.ANON.BUY.SVOL",
#"CANC.ANON.SELL.SVOL",
#"CANC.ANON.TOTAL.SVOL",
#"CANC.ANON.BUY.DVOL",
#"CANC.ANON.SELL.DVOL",
#"CANC.ANON.TOTAL.DVOL",
#"AGGR.ABS.ALL",
#"AGGR.REL.ALL",
#"AGGR.ABS.MPID",
#"AGGR.REL.MPID",
#"AGGR.ABS.ANON",
#"AGGR.REL.ANON",
#"ORSZ.SVOL.ALL",
#"ORSZ.DVOL.ALL",
#"ORSZ.SVOL.MPID",
#"ORSZ.DVOL.MPID",
#"ORSZ.SVOL.ANON",
#"ORSZ.DVOL.ANON"
#"DEPTH.BUY.SVOL",
#"DEPTH.SELL.SVOL",
#"DEPTH.TOTAL.SVOL",
#"DEPTH.BUY.DVOL",
#"DEPTH.SELL.DVOL",
#"DEPTH.TOTAL.DVOL",
#"D.MID.ABS",
#"D.MID.REL",
#"ABS.SPR.TW",
#"REL.SPR.TW",
#"VOL.SD",
#"VOL.RV.PRE",
#"VOL.RV.POST",
#"PRERR.MEAN",
#"PRERR.SD",
#"PRERR.MAX"

###Get List of Stock Tickers
ticks <- list.files(varDirectory)
###Set up initial dataframe to which to add data
data <- as.data.frame(matrix(NA, 0, length(vars)))
colnames(data) <- vars

##cycle thorugh tickers and dates

for (i in 1:length(ticks)) {
  cat(paste0("Ticker ", i, " out of ", length(ticks), "\n"))
  files <-
    list.files(paste0(varDirectory, "/", ticks[i]), pattern = paste0(w, "sec"))
  
  for (j in 1:length(files)) {
    filename <- paste0(varDirectory, "/", ticks[i], "/", files[j])
    load(filename)
    rm(filename)
    dataf$DATE <- unlist(strsplit(files[j], "_"))[2]
    dataf$TICK <- ticks[i]
    if (nrow(dataf) != length(int)) {
      stop("wrong number of intervals")
    }
    dataf$TIME <- int
    
    dataf <- dataf[, vars]
    
    data <- rbind(data, dataf)
    rm(dataf)
    
  }
}

filename <- paste0(saveDirectory, "/FullData_", w, "sec.dta")
write.dta(data, file = filename)
rm(filename)
