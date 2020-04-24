rm(list = ls(all = TRUE))
library(MTS)
library(zoo)
library(docstring)

rootDirectory <- "W:/LOBSTER"
mergedDirectory <- "W:/LOBSTER/merged"
saveDirectory <- "W:/LOBSTER/Variables"
codeDirectory <-
  "C:/DatiLocali/Dropbox/Trader Anonymity/Nasdaq-QMM-incentive-scheme/Codes/Clean Data/"

#Load Functions
getMessageType <- dget(paste(codeDirectory, "getMessageType.R", sep = ""))
getAggressiveness <-
  dget(paste(codeDirectory, "getAggressiveness.R", sep = ""))
getOrderSize <- dget(paste(codeDirectory, "getOrderSize.R", sep = ""))
getTWSpreads <- dget(paste(codeDirectory, "getTWSpreads.R", sep = ""))
getRealizedVolatility <-
  dget(paste(codeDirectory, "getRealizedVolatility.R", sep = ""))
getHasbrouck <- dget(paste(codeDirectory, "getHasbrouck.R", sep = ""))

##############################
#####CHOICE VARIABLES
##############################
tick <- "AAL"
date <- "2020-02-28"
w <- 60 #in number of seconds
##############################
#Varaibles for Calculation of Realized Volatility
n <- 5 #number of grids
delta <- w / n #length of grids (in seconds)
K <- 10 #number of subsamples
ss <- 1 #length of subsamples (in seconds)
##############################
#Number of lags for Hasbrouck Measures
lags <- 5
##############################
##############################

#Trading hours (start & end)
startTrad <- 9.5 * 60 * 60
# 9:30:00.000 in ms after midnight
endTrad <- 16 * 60 * 60
# 16:00:00.000 in ms after midnight

filename <- paste0(mergedDirectory, "/", tick, "/", tick, "_", date, ".Rds")
load(file = filename)

#Adjust Prices
data$price <- data$price / 10000
data$askprice <- data$askprice / 10000
data$bidprice <- data$bidprice / 10000
#Adjust MPID
data$mpid <- replace(data$mpid, data$mpid == "null", 0)
data$mpid <- replace(data$mpid, data$mpid != 0, 1)
data$mpid <- as.integer(data$mpid)

#Discard if trade is outside of trading hours
data <- data[data$time >= startTrad & data$time <= endTrad, ]

#Create Dollar Volume and Depth
data$dvol <- data$size * data$price
data$depth_buy <- data$bidprice * data$bidsize
data$depth_sell <- data$askprice * data$asksize
data$depth_all <-
  rowSums(cbind(data$depth_buy, data$depth_sell), na.rm = T)
#Same-Side Quote
data$ssquote <- NA
data$ssquote[data$direction == 1] <- data$bidprice[data$direction == 1]
data$ssquote[data$direction == -1] <-
  data$askprice[data$direction == -1]
#Midquote and Spread
data$mid <- 0.5 * (data$askprice + data$bidprice)
data$spread <- data$askprice - data$bidprice

#Create Data Table with prices at the beginning of each ss-second interval
sintervals<-seq(startTrad,endTrad,ss) #timestamp of intervals
snumInt<-(endTrad-startTrad)/ss #number of intervals
indexSInt<-findInterval(data$time,sintervals) #assign messages to intervals
findexSInt<-match(1:snumInt,indexSInt) #find first price within each interval
ssMid<-as.data.frame(cbind(data$mid[findexSInt],sintervals[1:snumInt])); colnames(ssMid)<-c("mid","time")
#if mid=NA, that means there were no updates during interval; replace with last observation
ssMid$mid<-na.locf(ssMid$mid,na.rm=F)
ssMid$mid<-log(ssMid$mid)

intervals <- seq(startTrad, endTrad, w)
noint <- (endTrad - startTrad) / w

## Cycle Through Intervals

sub <-
  as.data.frame(matrix(NA, noint, 27))
colnames(sub) <-
  c(
    "SUB.ALL.BUY.NUM",
    "SUB.ALL.SELL.NUM",
    "SUB.ALL.TOTAL.NUM",
    "SUB.ALL.BUY.SVOL",
    "SUB.ALL.SELL.SVOL",
    "SUB.ALL.TOTAL.SVOL",
    "SUB.ALL.BUY.DVOL",
    "SUB.ALL.SELL.DVOL",
    "SUB.ALL.TOTAL.DVOL",
    "SUB.MPID.BUY.NUM",
    "SUB.MPID.SELL.NUM",
    "SUB.MPID.TOTAL.NUM",
    "SUB.MPID.BUY.SVOL",
    "SUB.MPID.SELL.SVOL",
    "SUB.MPID.TOTAL.SVOL",
    "SUB.MPID.BUY.DVOL",
    "SUB.MPID.SELL.DVOL",
    "SUB.MPID.TOTAL.DVOL",
    "SUB.ANON.BUY.NUM",
    "SUB.ANON.SELL.NUM",
    "SUB.ANON.TOTAL.NUM",
    "SUB.ANON.BUY.SVOL",
    "SUB.ANON.SELL.SVOL",
    "SUB.ANON.TOTAL.SVOL",
    "SUB.ANON.BUY.DVOL",
    "SUB.ANON.SELL.DVOL",
    "SUB.ANON.TOTAL.DVOL"
  )
exe <-
  as.data.frame(matrix(NA, noint, 27))
colnames(exe) <-
  c(
    "EXE.ALL.BUY.NUM",
    "EXE.ALL.SELL.NUM",
    "EXE.ALL.TOTAL.NUM",
    "EXE.ALL.BUY.SVOL",
    "EXE.ALL.SELL.SVOL",
    "EXE.ALL.TOTAL.SVOL",
    "EXE.ALL.BUY.DVOL",
    "EXE.ALL.SELL.DVOL",
    "EXE.ALL.TOTAL.DVOL",
    "EXE.MPID.BUY.NUM",
    "EXE.MPID.SELL.NUM",
    "EXE.MPID.TOTAL.NUM",
    "EXE.MPID.BUY.SVOL",
    "EXE.MPID.SELL.SVOL",
    "EXE.MPID.TOTAL.SVOL",
    "EXE.MPID.BUY.DVOL",
    "EXE.MPID.SELL.DVOL",
    "EXE.MPID.TOTAL.DVOL",
    "EXE.ANON.BUY.NUM",
    "EXE.ANON.SELL.NUM",
    "EXE.ANON.TOTAL.NUM",
    "EXE.ANON.BUY.SVOL",
    "EXE.ANON.SELL.SVOL",
    "EXE.ANON.TOTAL.SVOL",
    "EXE.ANON.BUY.DVOL",
    "EXE.ANON.SELL.DVOL",
    "EXE.ANON.TOTAL.DVOL"
  )
canc <-
  as.data.frame(matrix(NA, noint, 27))
colnames(canc) <-
  c(
    "CANC.ALL.BUY.NUM",
    "CANC.ALL.SELL.NUM",
    "CANC.ALL.TOTAL.NUM",
    "CANC.ALL.BUY.SVOL",
    "CANC.ALL.SELL.SVOL",
    "CANC.ALL.TOTAL.SVOL",
    "CANC.ALL.BUY.DVOL",
    "CANC.ALL.SELL.DVOL",
    "CANC.ALL.TOTAL.DVOL",
    "CANC.MPID.BUY.NUM",
    "CANC.MPID.SELL.NUM",
    "CANC.MPID.TOTAL.NUM",
    "CANC.MPID.BUY.SVOL",
    "CANC.MPID.SELL.SVOL",
    "CANC.MPID.TOTAL.SVOL",
    "CANC.MPID.BUY.DVOL",
    "CANC.MPID.SELL.DVOL",
    "CANC.MPID.TOTAL.DVOL",
    "CANC.ANON.BUY.NUM",
    "CANC.ANON.SELL.NUM",
    "CANC.ANON.TOTAL.NUM",
    "CANC.ANON.BUY.SVOL",
    "CANC.ANON.SELL.SVOL",
    "CANC.ANON.TOTAL.SVOL",
    "CANC.ANON.BUY.DVOL",
    "CANC.ANON.SELL.DVOL",
    "CANC.ANON.TOTAL.DVOL"
  )
aggr <-
  as.data.frame(matrix(NA, noint, 6))
colnames(aggr) <-
  c(
    "AGGR.ABS.ALL",
    "AGGR.REL.ALL",
    "AGGR.ABS.MPID",
    "AGGR.REL.MPID",
    "AGGR.ABS.ANON",
    "AGGR.REL.ANON"
  )
orsz <-
  as.data.frame(matrix(NA, noint, 6))
colnames(orsz) <-
  c(
    "ORSZ.SVOL.ALL",
    "ORSZ.DVOL.ALL",
    "ORSZ.SVOL.MPID",
    "ORSZ.DVOL.MPID",
    "ORSZ.SVOL.ANON",
    "ORSZ.DVOL.ANON"
  )
depth <-
  as.data.frame(matrix(NA, noint, 6))
colnames(depth) <-
  c(
    "DEPTH.BUY.SVOL",
    "DEPTH.SELL.SVOL",
    "DEPTH.TOTAL.SVOL",
    "DEPTH.BUY.DVOL",
    "DEPTH.SELL.DVOL",
    "DEPTH.TOTAL.DVOL"
  )
pricemov <-
  as.data.frame(matrix(NA, noint, 2))
colnames(pricemov) <- c("D.MID.ABS", "D.MID.REL")
baspr <-
  as.data.frame(matrix(NA, noint, 2))
colnames(baspr) <- c("ABS.SPR.TW", "REL.SPR.TW")
vol <-
  as.data.frame(matrix(NA, noint, 3))
colnames(vol) <- c("VOL.SD", "VOL.RV.PRE", "VOL.RV.POST")
hasbrouck <-
  as.data.frame(matrix(NA, noint, 3))
colnames(hasbrouck) <- c("PRERR.MEAN", "PRERR.SD", "PRERR.MAX")

######
endSpread <-
  rep(NA, 3) #get ending spread from previous interval upon each loop (for calculation of time-weighted spreads)
######

for (q in 1:noint) {
  cat(paste(tick, date, w, "iteration =", q, "out of", noint, "\n"))
  
  #st <- min(which(data$time > intervals[q]))
  #en <- max(which(data$time <= intervals[q + 1]))
  
  index<-which(data$time>=intervals[q]&data$time<intervals[q+1])

  if (length(index)<10){next}
  
  data_part <- data[index, ]
  time <- data_part$time
  mpid <- data_part$mpid
  event <- data_part$eventtype
  direction <- data_part$direction
  price <- data_part$price
  sz <- data_part$size
  dvol <- data_part$dvol
  mid <- data_part$mid
  ssquote <- data_part$ssquote
  asksz <- data_part$asksize
  bidsz <- data_part$bidsize
  spread <- data_part$spread
  depth_buy <- data_part$depth_buy
  depth_sell <- data_part$depth_sell
  depth_all <- data_part$depth_all
  
  numObs <- nrow(data_part)
  
  ###########################################
  ####MESSAGE TYPES
  ###########################################
  
  #SUBMISSIONS
  #All Submissions
  condition <- (event == 1)
  sub_all <- getMessageType(condition, direction, sz, dvol, "SUB", "ALL")
  #MPID Submissions
  condition <- (event == 1 & mpid == 1)
  sub_mpid <- getMessageType(condition, direction, sz, dvol, "SUB", "MPID")
  #Anonymous Submissions
  condition <- (event == 1 & mpid == 0)
  sub_anon <- getMessageType(condition, direction, sz, dvol, "SUB", "ANON")
  
  sub[q, ] <- c(sub_all, sub_mpid, sub_anon)
  rm(sub_all, sub_mpid, sub_anon)
  
  #EXECUTIONS
  #All Executions
  condition <- (event == 4 | event == 5)
  exe_all <- getMessageType(condition, direction, sz, dvol, "EXE", "ALL")
  #MPID Executions
  condition <- ((event == 4 | event == 5) & mpid == 1)
  exe_mpid <- getMessageType(condition, direction, sz, dvol, "EXE", "MPID")
  #Anonymous Executions
  condition <- ((event == 4 | event == 5) & mpid == 0)
  exe_anon <- getMessageType(condition, direction, sz, dvol, "EXE", "ANON")
  
  exe[q, ] <- c(exe_all, exe_mpid, exe_anon)
  rm(exe_all, exe_mpid, exe_anon)
  
  #CANCELLATIONS
  #All Cancellations
  condition <- (event == 2 | event == 3)
  canc_all <- getMessageType(condition, direction, sz, dvol, "CANC", "ALL")
  #MPID Cancellations
  condition <- ((event == 2 | event == 3) & mpid == 1)
  canc_mpid <- getMessageType(condition, direction, sz, dvol, "CANC", "MPID")
  #Anonymous Cancellations
  condition <- ((event == 2 | event == 3) & mpid == 0)
  canc_anon <- getMessageType(condition, direction, sz, dvol, "CANC", "ANON")
  
  canc[q, ] <- c(canc_all, canc_mpid, canc_anon)
  rm(canc_all, canc_mpid, canc_anon)
  
  ###########################################
  ####ORDER CHARACTERISTICS
  ###########################################
  
  #AGGRESSIVENESS
  #All Submissions
  indexSub <- which(event == 1)
  indexSub <- indexSub[indexSub != 1]
  aggr_all <-
    getAggressiveness(direction, price, mid, ssquote, indexSub, "ALL")
  rm(indexSub)
  #MPID Submissions
  indexSub <- which(event == 1 &
                      mpid == 1)
  indexSub <- indexSub[indexSub != 1]
  aggr_mpid <-
    getAggressiveness(direction, price, mid, ssquote, indexSub, "MPID")
  rm(indexSub)
  #Anonymous Submissions
  indexSub <- which(event == 1 &
                      mpid == 0)
  indexSub <- indexSub[indexSub != 1]
  aggr_anon <-
    getAggressiveness(direction, price, mid, ssquote, indexSub, "ANON")
  rm(indexSub)
  
  aggr[q, ] <- c(aggr_all, aggr_mpid, aggr_anon)
  rm(aggr_all, aggr_mpid, aggr_anon)
  
  #ORDER SIZE
  #All Submissions
  indexSub <- which(event == 1)
  indexSub <- indexSub[indexSub != 1]
  orsz_all <- getOrderSize(price, sz, indexSub, "ALL")
  #MPID Submissions
  indexSub <- which(event == 1 &
                      mpid == 1)
  indexSub <- indexSub[indexSub != 1]
  orsz_mpid <- getOrderSize(price, sz, indexSub, "MPID")
  #Anonymous Submissions
  indexSub <- which(event == 1 &
                      mpid == 0)
  indexSub <- indexSub[indexSub != 1]
  orsz_anon <- getOrderSize(price, sz, indexSub, "ANON")
  
  orsz[q, ] <- c(orsz_all, orsz_mpid, orsz_anon)
  rm(orsz_all, orsz_mpid, orsz_anon)
  
  ###########################################
  ####DEPTH
  ###########################################
  depth[q, ] <-
    c(
      mean(bidsz),
      mean(asksz),
      mean(bidsz + asksz),
      mean(depth_buy),
      mean(depth_sell),
      mean(depth_all)
    )
  
  ###########################################
  ####PRICE MOVEMENTS
  ###########################################
  dPriceAbs <- mid[numObs] - mid[1] #change in midquote in absolute terms
  dPriceRel <-
    (mid[numObs] - mid[1]) / (mid[1]) #percentage change in midquote
  
  pricemov[q, ] <- c(dPriceAbs, dPriceRel)
  
  ###########################################
  ####SPREADS
  ###########################################
  
  #TIME-WEIGHTED BID ASK SPREADS
  baspr[q, ] <- getTWSpreads(endSpread, spread, mid, time, intervals, q)
  
  endSpread <-
    c(spread[numObs], mid[numObs], time[numObs]) #replace with ending spread for next interval loop
  
  ###########################################
  ####VOLATILITY
  ###########################################
  
  #Standard deviation of midquote returns (in bps)
  volSd <- sd(diff(mid) / mid[1:(numObs - 1)] ^ 2) * 10000
  
  #Realized volatility (following definition in Hautsch(2011))
  #calculation requires a rolling window calculation.
  #Therefore, realized volatility requires two different calculations: one for regressions using lagged variables,
  #and another for regressions using lead variables, such that volatility calcuations are not endogeneous to the
  #dependent variables in these regressions
  #(e.g., regressing ex-post volatility at time t+1 on MPID ratio at time t, volatility calculation must avoid using
  #data from time t. For regression of MPID ratio at time t on volatility at time t-1, volatility must avoid using
  #data from time t).
  
  #PRE
  volRVpre <- NA
  if (q >= (n + 1)) {
    volRVpre <-
      getRealizedVolatility(n, delta, K, ss, intervals, q, ssMid, "PRE")
    volRVpre <- volRVpre * 10000 #in bps
  }
  #POST
  volRVpost <- NA
  if (q <= (noint - n)) {
    volRVpost <-
      getRealizedVolatility(n, delta, K, ss, intervals, q, ssMid, "POST")
    volRVpost <- volRVpost * 10000
  }
  
  vol[q, ] <- c(volSd, volRVpre, volRVpost)
  rm(volSd, volRVpre, volRVpost)
  
  ###########################################
  ####HASBROUCK MEASURE
  ###########################################
  
  indexExe <- which(event == 4 | event == 5) #index of executions
  #require at least 50 execution prices
  if (length(indexExe) >= 50) {
    hasbrouck[q, ] <- getHasbrouck(indexExe, lags, price, direction, sz)
  }
  rm(lags, indexExe)
  
}

dataf <-
  cbind(sub, exe, canc, aggr, orsz, depth, pricemov, baspr, vol, hasbrouck)
f <- paste0(saveDirectory, "/", tick)
if (file.exists(f) == F) {
  dir.create(f)
}
filename <- paste0(f, "/", tick, "_", date, "_", w, "sec", ".Rds")
save(dataf, file = filename)
rm(filename)
