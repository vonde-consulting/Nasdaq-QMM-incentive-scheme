rm(list = ls())
library(data.table)
library(bizdays)
library(moments)

rootDirectory <- "W:/LOBSTER"
quoteDirectory <- "W:/LOBSTER/quotes"
messageDirectory <- "W:/LOBSTER/allMessages"
mergedDirectory <- "W:/LOBSTER/merged"

# Trading hours (start & end)
startTrad <- 9.5 * 60 * 60
# 9:30:00.000 in ms after midnight
endTrad <- 16 * 60 * 60
# 16:00:00.000 in ms after midnight

#get list of all files in /quotes/ and /allMessages/ directories
quoteFolder <- list.files(quoteDirectory)
messageFolder <- list.files(messageDirectory)
#get list of tickets
firms <-
  vapply(strsplit(quoteFolder, "_"), `[`, 1, FUN.VALUE = character(1))
#get list of dates
dates <-
  as.POSIXlt(seq(as.Date("2020-02-01"), as.Date("2020-02-28"), "days"), format =
               "%Y-%m-%d")

######################################################################
################CYCLE THROUGH STOCKS AND DAYS
######################################################################

for (i in 1:length(firms)) {
  for (j in 1:length(dates)) {
    cat(paste0(
      "Firm ",
      i,
      " out of ",
      length(firms),
      ": date ",
      j,
      " out of ",
      length(dates),
      "\n"
    ))
    
    ###LOAD ORDERBOOK DATA
    filename <-
      paste0(
        quoteDirectory,
        "/",
        quoteFolder[i],
        "/",
        firms[i],
        "_",
        dates[j],
        "_",
        "34200000_57600000_orderbook_1.csv"
      )
    if (file.exists(filename) == F) {
      next
    } #skip if file does not exist
    data_o <-
      read.table(
        file = filename,
        sep = ",",
        stringsAsFactors = F,
        col.names = c("askprice", "asksize", "bidprice", "bidsize")
      )
    rm(filename)
    if (nrow(data_o) < 10) {
      stolp("Skipped it")
    } #skip if fewer than 10 observations
    ###GET TIMESTAMPS FROM MESSAGE_1 FILE
    filename <-
      paste0(
        quoteDirectory,
        "/",
        quoteFolder[i],
        "/",
        firms[i],
        "_",
        dates[j],
        "_",
        "34200000_57600000_message_1.csv"
      )
    if (file.exists(filename) == F) {
      next
    } #skip if file does not exist
    data_m <-
      read.table(
        file = filename,
        sep = ",",
        stringsAsFactors = F,
        col.names = c(
          "time",
          "eventtype",
          "orderID",
          "size",
          "price",
          "direction",
          "mpid"
        )
      )
    rm(filename)
    if (nrow(data_m) < 10) {
      next
    } #skip if fewer than 10 observations
    ###MERGE THEM
    data_o$time <- data_m$time
    data_o$clocktime <-
      format(as.POSIXct((data_o$time), origin = "1970-01-01", tz = "UTC"),
             "%H:%M:%OS6")
    data_o <- unique(data_o)
    rm(data_m)
    data_o <- as.data.table(data_o)
    setkey(data_o, clocktime)
    ###LOAD MESSAGE_0 FILE
    filename <-
      paste0(
        messageDirectory,
        "/",
        messageFolder[i],
        "/",
        firms[i],
        "_",
        dates[j],
        "_",
        "14400000_72000000_message_0.csv"
      )
    data_m <-
      read.table(
        file = filename,
        sep = ",",
        stringsAsFactors = F,
        col.names = c(
          "time",
          "eventtype",
          "orderID",
          "size",
          "price",
          "direction",
          "mpid"
        )
      )
    rm(filename)
    data_m$clocktime <-
      format(as.POSIXct((data_m$time), origin = "1970-01-01", tz = "UTC"),
             "%H:%M:%OS6")
    data_m <- as.data.table(data_m)
    setkey(data_m, clocktime)
    ###MERGE DATASETS
    data <- data_o[data_m, roll = T, mult = "last"]
    data$i.time <- NULL
    rm(data_o, data_m)
    
    ###STANDARD CLEANING ALGORITHM
    #only keep messages that are part of the continuous trading period
    #data<-data[data$time>=startTrad&data$time<=endTrad,]
    #check for trading halts
    tradehaltIdx <- which(data$eventtype == 7 & data$direction == -1)
    
    tradequoteIdx <- which(data$eventtype == 7 & data$direction == 0)
    
    traderesumeIdx <- which(data$eventtype == 7 & data$direction == 1)
    
    if (length(tradehaltIdx) == 0 &
        length(tradequoteIdx) == 0  &
        length(traderesumeIdx) == 0) {
      print("No trading halts detected.")
    }
    if (length(tradehaltIdx) != 0) {
      cat("Data contains trading halt! at time stamp(s)",
          data$clocktime[tradehaltIdx],
          "\n")
    }
    if (length(tradequoteIdx) != 0) {
      cat(" Data contains quoting message! at time stamp(s)",
          data$clocktime[tradequoteIdx],
          "\n")
    }
    if (length(traderesumeIdx) != 0) {
      cat(" Data resumes trading! at time stamp(s) ",
          data$clocktime[traderesumeIdx],
          "\n")
    }
    rm(tradehaltIdx, tradequoteIdx, traderesumeIdx)
    
    #f<-paste0(mergedDirectory,"/",firms[i])
    #if (file.exists(f)==F){dir.create(f)}
    #filename<-paste0(f,"/",firms[i],"_",dates[j],".Rds")
    #save(data,file=filename)
    
  }
}
