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
getTWSpreads <-
  dget(paste(codeDirectory, "getTWSpreads.R", sep = ""))

##############################
#####CHOICE VARIABLES
#####get list of all firms and dates
ticks <- list.files(mergedDirectory)
dates <-
  as.POSIXlt(seq(as.Date("2020-02-01"), as.Date("2020-02-28"), "days"), format =
               "%Y-%m-%d")
dates <- dates[!dates$wday %in% c(6, 0)] #get rid of weekdays
dates <-
  dates[dates != as.POSIXlt(as.Date("2020-02-17"), format = "%Y-%m-%d")] #get rid of President's Day
#define different interval lengths (in terms of number of seconds)tick <- "AAL"
date <- "2020-02-28"
##############################
#Trading hours (start & end)
startTrad <- 9.5 * 60 * 60
# 9:30:00.000 in ms after midnight
endTrad <- 16 * 60 * 60
# 16:00:00.000 in ms after midnight

baspr_full <- as.data.frame(matrix(NA, length(ticks) * length(dates), 6))
colnames(baspr_full) <-
  c("TICK",
    "DATE",
    "ABS.SPR.TW",
    "REL.SPR.TW",
    "ABS.SPR",
    "REL.SPR")
baspr_full$TICK <- rep(ticks, each = length(dates))
baspr_full$DATE <- as.character(rep(dates, length(ticks)))

for (i in 1:length(ticks)) {
  for (j in 1:length(dates)) {
    tick <- ticks[i]
    date <- dates[j]
    
    cat(paste(tick, date,  "\n"))
    
    filename <-
      paste0(mergedDirectory, "/", tick, "/", tick, "_", date, ".Rds")
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
    data <- data[data$time >= startTrad & data$time <= endTrad,]
    
    #Midquote and Spread
    mid <- 0.5 * (data$askprice + data$bidprice)
    spread <- data$askprice - data$bidprice
    time <- data$time
    intervals <- c(startTrad, endTrad)
    q <- 1
    
    endSpread <-
      rep(NA, 3) #get ending spread from previous interval upon each loop (for calculation of time-weighted spreads)
    
    baspr <-
      getTWSpreads(endSpread, spread, mid, time, intervals, q)
    
    colInd <- match(names(baspr), colnames(baspr_full))
    baspr_full[(i - 1) * length(dates) + j, colInd] <- baspr
    rm(baspr)
    
    baspr <- c(mean(spread, na.rm = T), mean((spread / mid), na.rm = T))
    names(baspr) <- c("ABS.SPR", "REL.SPR")
    
    colInd <- match(names(baspr), colnames(baspr_full))
    baspr_full[(i - 1) * length(dates) + j, colInd] <- baspr
    rm(baspr)
    
  }
}

filename <-
  paste0(saveDirectory, "TimeWeightedBidAskSpreads_byStockDay.Rds")
save(baspr_full, file = filename)
rm(filename)

baspr <-
  aggregate(baspr_full[, 3:ncol(baspr_full)],
            by = list(baspr_full$TICK),
            mean,
            na.rm = T)
