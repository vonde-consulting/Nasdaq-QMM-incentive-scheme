rm(list = ls())
library(data.table)
library(ggplot2)
library(reshape2)

rootDirectory <- "W:/LOBSTER"
posDirectory <- "W:/LOBSTER/mpPositions"
quoteDirectory <- "W:/LOBSTER/quotes"

###LOAD DATA################
filename <- paste0(posDirectory, "/MpPositions.csv")
data <-
  fread(
    file = filename,
    sep = ",",
    stringsAsFactors = F,
    header = T
  )
#get rid of 2019-11-11
data <- data[data$date != "2019-11-11", ]
datafull <- data
rm(filename)

##FORMAT DATES##############
dates <-
  as.POSIXlt(seq(as.Date("2020-02-01"), as.Date("2020-02-28"), "days"), format =
               "%Y-%m-%d")
dates <- dates[!dates$wday %in% c(6, 0)] #get rid of weekdays
dates <-
  dates[dates != as.POSIXlt(as.Date("2020-02-17"), format = "%Y-%m-%d")] #get rid of President's Day
data <- data[data$date %in% dates]

###KEEP ONLY MPIDs THAT ARE IN OUR SAMPLE
#get list of all possible MPIDs
filename <- paste0(rootDirectory, "/UniqueMPIDs_20200506.Rds")
load(file = filename)
rm(filename)
MPID <- MPID[MPID != "null"]
data <- data[data$MPID %in% MPID, ]

###KEEP ONLY STOCKS THAT ARE IN OUR SAMPLE
quoteFolder <- list.files(quoteDirectory)
#get list of tickers
firms <-
  vapply(strsplit(quoteFolder, "_"), `[`, 1, FUN.VALUE = character(1))
data <- data[data$symbol %in% firms, ]

##CALCULATE MODE PER MPID
mode <- c("Normal", "Passive", "Syndicate", "PreSyndicate", "Penalty")
tableMode <-
  matrix(
    unlist(lapply(MPID, function(y)
      lapply(mode, function(x)
        sum(data$MPID == y &
              data$marketMakerMode == x) / sum(data$MPID == y)))),
    length(MPID),
    length(mode),
    byrow = T,
    dimnames = list(MPID, mode)
  )
#save to table
#explore circumstances when mode!="Normal"
ind <- which(data$marketMakerMode != "Normal")
data[ind, ]
#MPID "JPMS" (JPMorgan Securities) is listed as "PreSyndicate" for CDW (CDW COrporation) for all sample dates
#CDW Corp went public in 2013
#pre-syndicate bid: https://financial-dictionary.thefreedictionary.com/Rule+10b-7
#https://www.globenewswire.com/news-release/2020/04/16/2017743/0/en/CDW-Corporation-Announces-Upsizing-and-Pricing-of-Registered-Offering-of-600-Million-of-Senior-Notes-due-2025.html
#is JPMS always listed as pre-syndicate for CDW?
datafull[datafull$MPID == "JPMS" & datafull$symbol == "CDW", ]

##CALCULATE # PRIMARY MARKET MAKER PER MPID
tablePrim <-
  unlist(lapply(MPID, function(y)
    sum(data$MPID == y & data$primaryMarketMaker == T) / sum(data$MPID == y)))
names(tablePrim) <- MPID
data[which(data$primaryMarketMaker == F), ]
#MSCO (Morgan Stanley & Co.) is listed as not the primary market maker for SPLK (Splunk, Inc.)
#designated a primary market maker if they meet certain standards: https://www.sec.gov/pdf/nasd1/4000ser.pdf
temp <- datafull[which(datafull$primaryMarketMaker == F), ]
temp <- temp[order(temp$MPID, temp$symbol, temp$date), ]
data[data$MPID == "MSCO" & data$symbol == "SPLK", ]

##CALCULATE STATE PER MPID
state <-
  c("Active",
    "ExcusedWithdrawn",
    "Withdrawn",
    "Suspended",
    "Deleted")
tableState <-
  matrix(
    unlist(lapply(MPID, function(y)
      lapply(state, function(x)
        sum(data$MPID == y &
              data$marketParticipantSate == x) / sum(data$MPID == y)))),
    length(MPID),
    length(state),
    byrow = T,
    dimnames = list(MPID, state)
  )
data[data$marketParticipantSate != "Active", ]
#GSCO (Goldman Sachs) is listed as "excused/withdrawn" for TESLA in Feb. 13&14
data[data$MPID == "GSCO" & data$symbol == "TSLA", ]
#E or Excused Withdrawn - The market participant has requested to be removed temporarily from the issue for special circumstances, such as a religious holiday, compliance requirement, system failure, etc. Also, ECN's assume this state when they do not have size at the inside.
temp <- datafull[datafull$marketParticipantSate != "Active", ]
temp <- temp[order(temp$MPID, temp$symbol, temp$date), ]
#number of withdrawn per day over time
dates <- unique(datafull$date)
numW <-
  unlist(lapply(dates, function(x)
    sum(
      datafull$date == x & datafull$marketParticipantSate == "Withdrawn"
    )))
names(numW) <- dates
numE <-
  unlist(lapply(dates, function(x)
    sum(
      datafull$date == x &
        datafull$marketParticipantSate == "ExcusedWithdrawn"
    )))
names(numE) <- dates
numWE <- numW + numE

###MAKE PLOTS -- NUMBER OF WITHDRAWALS
plotData <-
  as.data.frame(cbind(names(numW), unname(numW), unname(numE)), stringsAsFactors =
                  F)
colnames(plotData) <-
  c("Dates", "Number of Withdrawals", "Number of Excused")
plotData$`Number of Withdrawals` <-
  as.integer(plotData$`Number of Withdrawals`)
plotData$`Number of Excused` <-
  as.integer(plotData$`Number of Excused`)
years <- as.integer(substring(plotData$Dates, 1, 4))
#plotData$Event<-"2019"
#plotData$Event[years==2020]<-"2020"
#plotData$Event <- factor(plotData$Event, levels=c("2019","2020"))

##WITHDRAWALS
plotData <- melt(plotData, id.vars = 'Dates')

plotname <-
  paste0(rootDirectory, "/MMWithdrawals")
pdf(
  paste(plotname, ".pdf", sep = ""),
  width = 7,
  height = 7,
  paper = 'special'
)
p <-
  ggplot(plotData, aes(x = Dates,
                       y = value,
                       fill = variable)) +
  xlab("") +
  ylab("") +
  geom_bar(position = "stack",
           stat = "identity",
           width = 1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "bottom")
p
dev.off()

###MAKE PLOTS -- NUMBER OF WITHDRAWALS PER MPID
datatemp <-
  datafull[, c("MPID", "symbol", "date", "marketParticipantSate")]
datatemp$W <-
  datatemp$marketParticipantSate == "Withdrawn" |
  datatemp$marketParticipantSate == "WithdrawnExcused"
numWMPID <-
  aggregate(datatemp$W,
            by = list(datatemp$MPID, datatemp$date),
            FUN = sum)
colnames(numWMPID) <- c("MPID", "date", "W")
numWMPID <- numWMPID[numWMPID$W > 0, ]
numWSym <-
  aggregate(datatemp$W,
            by = list(datatemp$symbol, datatemp$date),
            FUN = sum)
colnames(numWSym) <- c("symbol", "date", "W")
numWSym <- numWSym[numWSym$W > 0, ]

##WITHDRAWALS BY MPID
plotname <-
  paste0(rootDirectory, "/MMWithdrawalByMPID")
pdf(
  paste(plotname, ".pdf", sep = ""),
  width = 7,
  height = 10,
  paper = 'special'
)
p <-
  ggplot(numWMPID, aes(x = factor(date),
                       y = W,
                       fill = MPID)) +
  xlab("") +
  ylab("Number of Withdrawals") +
  geom_bar(position = "stack",
           stat = "identity",
           width = 1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "bottom")
p
dev.off()

##WITHDRAWALS BY STOCK
plotname <-
  paste0(rootDirectory, "/MMWithdrawalByStock")
pdf(
  paste(plotname, ".pdf", sep = ""),
  width = 7,
  height = 7,
  paper = 'special'
)
p <-
  ggplot(numWSym, aes(x = factor(date),
                      y = W,
                      fill = symbol)) +
  xlab("") +
  ylab("Number of Withdrawals") +
  geom_bar(position = "stack",
           stat = "identity",
           width = 1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "bottom") +
  theme(legend.position = "none")
p
dev.off()
