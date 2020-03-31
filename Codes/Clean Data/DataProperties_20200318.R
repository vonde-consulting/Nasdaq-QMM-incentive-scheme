rm(list = ls())
library(data.table)
library(bizdays)
library(moments)

rootDirectory <- "W:/LOBSTER"
mergedDirectory <- "W:/LOBSTER/merged"
#define summary stats
stats <-
  c("mpid.ratio",
    "num.rmm",
    "mean.subs",
    "median.subs",
    "sd.subs",
    "skew.subs",
    "hhi")

#get list of all files in /quotes/ and /allMessages/ directories
mergedFolder <- list.files(mergedDirectory)
dates <-
  as.POSIXlt(seq(as.Date("2020-02-01"), as.Date("2020-02-28"), "days"), format =
               "%Y-%m-%d")
dates <- dates[!dates$wday %in% c(6, 0)] #get rid of weekdays
dates <-
  dates[dates != as.POSIXlt(as.Date("2020-02-17"), format = "%Y-%m-%d")] #get rid of President's Day

######################################################################
################CYCLE THROUGH STOCKS AND DAYS
######################################################################

fullstats <-
  array(
    dim = c(length(dates), length(mergedFolder), length(stats)),
    dimnames = list(
      dates = as.character(dates),
      firms = mergedFolder,
      stats = stats
    )
  )

for (i in 1:length(mergedFolder)) {
  cat(paste0("Firm ", i, " out of ", length(mergedFolder)), "\n")
  mergedFiles <-
    list.files(paste0(mergedDirectory, "/", mergedFolder[i]))
  
  for (j in 1:length(mergedFiles)) {
    filename <-
      paste0(mergedDirectory, "/", mergedFolder[i], "/", mergedFiles[j])
    load(file = filename)
    
    mpidratio <-
      sum(data$mpid != "null") / nrow(data) #MPID-attributed submissions / # of submissions
    mpidnum <-
      length(unique(data$mpid[data$mpid != "null"])) #number of unique RMMs
    
    mpidDt <-
      table(data$mpid[data$mpid != "null"]) #number of submissions per unique RMM
    mpidmean <- mean(mpidDt, na.rm = T)
    mpidmedian <- median(mpidDt, na.rm = T)
    mpidsd <- sd(mpidDt, na.rm = T)
    mpidskew <- skewness(mpidDt, na.rm = T)
    mpidhhi <- sum(((mpidDt / sum(mpidDt)) * 100) ^ 2, na.rm = T) #hhi index
    rm(mpidDt)
    
    fullstats[j, i, 1] <- mpidratio
    fullstats[j, i, 2] <- mpidnum
    fullstats[j, i, 3] <- mpidmean
    fullstats[j, i, 4] <- mpidmedian
    fullstats[j, i, 5] <- mpidsd
    fullstats[j, i, 6] <- mpidskew
    fullstats[j, i, 7] <- mpidhhi
    
    
  }
  
}

filename <- paste0("W:/LOBSTER/MPIDStats_v1_20200331.Rds")
save(fullstats, file = filename)

b <- apply(fullstats, c(2, 3), mean)
max(b[, 1])
min(b[, 1])
mean(b[, 2])
median(b[, 2])
mean(b[, 7])
median(b[, 7])
