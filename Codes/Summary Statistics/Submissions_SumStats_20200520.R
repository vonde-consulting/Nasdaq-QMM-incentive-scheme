rm(list = ls(all = TRUE))
library(ggplot2)
library(reshape2)
Sys.setenv(TZ ="UTC")

rootDirectory <- "W:/LOBSTER"
mergedDirectory <- "W:/LOBSTER/merged"
saveDirectory <- "W:/LOBSTER/Variables"
graphDirectory <- "W:/LOBSTER/Graphs"
codeDirectory <-
  "C:/DatiLocali/Dropbox/Trader Anonymity/Nasdaq-QMM-incentive-scheme/Codes/Summary Statistics/"

#Load Functions
graphSubmissionStats <-
  dget(paste(codeDirectory, "graphSubmissionStats.R", sep = ""))

##CHOICE VARIABLES
w <- 60
#Trading hours (start & end)
startTrad <- 9.5 * 60 * 60
# 9:30:00.000 in ms after midnight
endTrad <- 16 * 60 * 60
#number of intervals
intervals <-
  seq(startTrad, endTrad, w)
intervals <- intervals[-length(intervals)]
noint <- (endTrad - startTrad) / w
clocktime <-
  format(as.POSIXct((intervals), origin = "1970-01-01"),
         "%H:%M:%S")

#get list of all firms and dates
ticks <- list.files(mergedDirectory)
dates <-
  as.POSIXlt(seq(as.Date("2020-02-01"), as.Date("2020-02-28"), "days"), format =
               "%Y-%m-%d")
dates <- dates[!dates$wday %in% c(6, 0)] #get rid of weekdays
dates <-
  dates[dates != as.POSIXlt(as.Date("2020-02-17"), format = "%Y-%m-%d")] #get rid of President's Day
datetime <-
  as.POSIXct(paste(rep(dates, each = length(clocktime)), rep(clocktime, length(dates)), tz = "Europe/Paris"), format =
               "%Y-%m-%d %H:%M:%S")

####################################
####CYCLE THROUGH AND COLLECT SUBMISSIONS VARIABLES
####################################

if (FALSE) {
  subfull <-
    as.data.frame(matrix(0, noint * length(dates) * length(ticks), ((27 * 4) +
                                                                      4)))
  temp <-
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
  colnames(subfull) <-
    c(
      "DATE",
      "TIME",
      "TICK",
      "MIDQUOTE",
      paste0(temp, ".-Inf.Inf"),
      paste0(temp, ".1.1"),
      paste0(temp, ".1.3"),
      paste0(temp, ".3.Inf")
    )
  
  
  subfull$TICK <- rep(ticks, each = noint * length(dates))
  subfull$TIME <- rep(intervals, times = length(dates) * length(ticks))
  subfull$DATE <- rep(dates, each = noint, times = length(ticks))
  
  subagg <- as.data.frame(matrix(0, noint, (27 * 4) + 2))
  colnames(subagg) <-
    c(
      "TIME",
      "MIDQUOTE",
      paste0(temp, ".-Inf.Inf"),
      paste0(temp, ".1.1"),
      paste0(temp, ".1.3"),
      paste0(temp, ".3.Inf")
    )
  subagg$TIME <- intervals
  
  for (i in 1:length(ticks)) {
    for (j in 1:length(dates)) {
      tick <- ticks[i]
      date <- dates[j]
      
      cat(paste(tick, date, w, "\n"))
      
      f <- paste0(saveDirectory, "/", tick)
      
      filename <- paste0(f, "/", tick, "_", date, "_", w, "sec", ".Rds")
      load(file = filename)
      rm(filename)
      
      colInd <- match(colnames(subfull)[4:ncol(subfull)], colnames(dataf))
      rowInd <- which(subfull$DATE == date & subfull$TICK == tick)
      
      dataf[, colInd] <- replace(dataf[, colInd], is.na(dataf[, colInd]) == 1, 0)
      subfull[rowInd, 4:ncol(subfull)] <- dataf[, colInd]
      
      rm(rowInd)
      
      subagg[, 2:ncol(subagg)] <- subagg[, 2:ncol(subagg)] + dataf[, colInd]
      
      
    }
  }
  
  subagg[, 2:ncol(subagg)] <-
    subagg[, 2:ncol(subagg)] / (length(dates) * length(ticks))
  
  #save
  filename <- paste0(saveDirectory, "/Submissions_byMPID_", w, "sec.Rds")
  save(subfull, subagg, file = filename)
  rm(filename)
  
}

####################################
####MAKE PLOTS
####################################

filename <- paste0(saveDirectory, "/Submissions_byMPID_", w, "sec.Rds")
load(file = filename)
rm(filename)
subfull$MIDQUOTE <- replace(subfull$MIDQUOTE, subfull$MIDQUOTE == 0, NA)

#take average across times
sub <-
  as.data.frame(matrix(NA, noint, ncol(subfull)))
colnames(sub) <- colnames(subfull)
sub$TIME <- intervals
sub$DATE <- NULL
sub$TICK <- NULL

for (i in 1:length(intervals)) {
  cat(paste0("Time Interval ", i, "\n"))
  ind <- which(subfull$TIME == intervals[i])
  sub[i, 2:ncol(sub)] <-
    colMeans(subfull[ind, 4:ncol(subfull)], na.rm = T)
}

test <- sum(sub[, 2:ncol(sub)]) - sum(subagg[, 2:ncol(subagg)], na.rm =
                                        T)
#if (test>0){stop("Something wrong with aggregation")}

#take daily sums
subdate <-
  as.data.frame(matrix(NA, length(dates) * length(ticks), ncol(subfull)))
colnames(subdate) <- colnames(subfull)
subdate$TIME <- NULL

for (i in 1:length(ticks)) {
  for (j in 1:length(dates)) {
    cat(paste0("Ticks ", i, " Dates ", j, "\n"))
    rowInd <- which(subfull$TICK == ticks[i] & subfull$DATE == dates[j])
    subdate$DATE[((i - 1) * length(dates) + j)] <-
      as.character(dates[j])
    subdate$TICK[((i - 1) * length(dates) + j)] <- ticks[i]
    subdate[((i - 1) * length(dates) + j), 3] <-
      median(subfull[rowInd, 4], na.rm = T)
    subdate[((i - 1) * length(dates) + j), 4:ncol(subdate)] <-
      colSums(subfull[rowInd, 5:ncol(subfull)], na.rm = T)
  }
}

subdate2 <-
  aggregate(subdate[, 4:ncol(subdate)], by = list(subdate$DATE), sum, na.rm =
              T)
mid2 <- aggregate(subdate[, 3], by = list(subdate$DATE), mean, na.rm = T)
cols <- c("DATE", "MIDQUOTE", colnames(subdate[, 4:ncol(subdate)]))
subdate2$MIDQUOTE <- mid2$x
rm(mid2)
colnames(subdate2)[1] <- "DATE"
subdate2 <- subdate2[, cols]
rm(cols)

####################################################
####################################################
###BY TIME INTERVAL
####################################################
####################################################

####################################################
###BREAKDOWN BETWEEN MPID AND ANON SUBMISSION VOLUMES
####################################################
level <- "-Inf.Inf"
side <- "TOTAL."
scale <- "DVOL."
select <- c(paste0("SUB.", c("ANON.", "MPID."), side, scale, level))
TIME <- rep(1:noint, length(dates))
plotdata <- sub[, select]

plotdata <- aggregate(plotdata, by = list(sub$TIME), sum, na.rm = T)
colnames(plotdata) <- c("t", "ANON", "MPID")
plotdata <- melt(plotdata, id.vars = 't')

legend.labels <- c("ANON", "MPID")
tick.breaks <- plotdata$t[seq(1, noint, 30)]
tick.labels <- clocktime[seq(1, noint, 30)]
midquote <- sub$MIDQUOTE

#filled plot
bartype <- "fill"
name <-
  paste0("Plot_Submissions_MPID_Anon_byTime_",
         side,
         scale,
         level,
         "_",
         bartype,
         "_",
         w,
         "sec")
plotname <-
  paste0(graphDirectory, "/", name)
plotname <- gsub("\\.", "_", plotname)
pdf(
  paste(plotname, ".pdf", sep = ""),
  width = 7,
  height = 7,
  paper = 'special'
)
p <-
  graphSubmissionStats(plotdata,
                       midquote,
                       name,
                       legend.labels,
                       tick.breaks,
                       tick.labels,
                       bartype)
p
dev.off()

#stacked plot
bartype <- "stack"
name <-
  paste0("Plot_Submissions_MPID_Anon_byTime_",
         side,
         scale,
         level,
         "_",
         bartype,
         "_",
         w,
         "sec")
plotname <-
  paste0(graphDirectory, "/", name)
plotname <- gsub("\\.", "_", plotname)
pdf(
  paste(plotname, ".pdf", sep = ""),
  width = 7,
  height = 7,
  paper = 'special'
)
p <-
  graphSubmissionStats(plotdata,
                       midquote,
                       name,
                       legend.labels,
                       tick.breaks,
                       tick.labels,
                       bartype)
p
dev.off()

rm(
  side,
  scale,
  level,
  p,
  bartype,
  plotdata,
  legend.labels,
  tick.labels,
  tick.breaks,
  midquote
)

####################################################
###BREAKDOWN BETWEEN BUY AND SELL SUBMISSION VOLUMES
####################################################

level <- "-Inf.Inf"
type <- "ALL."
scale <- "DVOL."
select <- c(paste0("SUB.", type, c("BUY.", "SELL."), scale, level))
plotdata <- sub[, select]
plotdata <- aggregate(plotdata, by = list(sub$TIME), sum, na.rm = T)
colnames(plotdata) <- c("t", "BUY", "SELL")
plotdata <- melt(plotdata, id.vars = 't')

legend.labels <- c("Buy", "Sell")
tick.breaks <- plotdata$t[seq(1, noint, 30)]
tick.labels <- clocktime[seq(1, noint, 30)]
midquote <- sub$MIDQUOTE

#filled plot
bartype <- "fill"
name <-
  paste0("Plot_Submissions_Buy_Sell_byTime_",
         type,
         scale,
         level,
         "_",
         bartype,
         "_",
         w,
         "sec")
plotname <-
  paste0(graphDirectory, "/", name)
plotname <- gsub("\\.", "_", plotname)
pdf(
  paste(plotname, ".pdf", sep = ""),
  width = 7,
  height = 7,
  paper = 'special'
)
p <-
  graphSubmissionStats(plotdata,
                       midquote,
                       name,
                       legend.labels,
                       tick.breaks,
                       tick.labels,
                       bartype)
p
dev.off()

#stacked plot
bartype <- "stack"
name <-
  paste0("Plot_Submissions_Buy_Sell_byTime_",
         type,
         scale,
         level,
         "_",
         bartype,
         "_",
         w,
         "sec")
plotname <-
  paste0(graphDirectory, "/", name)
plotname <- gsub("\\.", "_", plotname)
pdf(
  paste(plotname, ".pdf", sep = ""),
  width = 7,
  height = 7,
  paper = 'special'
)
p <-
  graphSubmissionStats(plotdata,
                       midquote,
                       name,
                       legend.labels,
                       tick.breaks,
                       tick.labels,
                       bartype)
p
dev.off()

rm(
  type,
  scale,
  level,
  p,
  bartype,
  plotdata,
  legend.labels,
  tick.labels,
  tick.breaks,
  midquote
)

####################################################
###BREAKDOWN BETWEEN SUBMISSIONS BY BOOK LEVEL
####################################################

type <- "MPID."
scale <- "DVOL."
side <- "TOTAL."
select <-
  c(paste0("SUB.", type, side, scale, c("-Inf.Inf", "1.1", "1.3", "3.Inf")))
TIME <- rep(1:noint, length(dates))
plotdata <- sub[, select]
plotdata[, 1] <- plotdata[, 1] - plotdata[, 2] - plotdata[, 3] - plotdata[, 4]
if (sum(plotdata[, 1] < (-1e-10)) > 0) {
  stop("negative order totals")
}
plotdata[, 1] <- replace(plotdata[, 1], plotdata[, 1] < 0, 0)

plotdata <- aggregate(plotdata, by = list(sub$TIME), sum, na.rm = T)
colnames(plotdata) <- c("t", "-Inf.Inf", "1.1", "1.3", "3.Inf")
plotdata <- melt(plotdata, id.vars = 't')

legend.labels <-
  c("Price Improving", "Book Level 1", "Book Level 1-3", "Book Level 3+")
tick.breaks <- plotdata$t[seq(1, noint, 30)]
tick.labels <- clocktime[seq(1, noint, 30)]
midquote <- sub$MIDQUOTE

#filled plot
bartype <- "fill"
name <-
  paste0("Plot_Submissions_BookLevel_byTime_",
         type,
         side,
         scale,
         bartype,
         "_",
         w,
         "sec")
plotname <-
  paste0(graphDirectory, "/", name)
plotname <- gsub("\\.", "_", plotname)
pdf(
  paste(plotname, ".pdf", sep = ""),
  width = 7,
  height = 7,
  paper = 'special'
)
p <-
  graphSubmissionStats(plotdata,
                       midquote,
                       name,
                       legend.labels,
                       tick.breaks,
                       tick.labels,
                       bartype)
p
dev.off()

#stacked plot
bartype <- "stack"
name <-
  paste0("Plot_Submissions_BookLevel_byTime_",
         type,
         side,
         scale,
         bartype,
         "_",
         w,
         "sec")
plotname <-
  paste0(graphDirectory, "/", name)
plotname <- gsub("\\.", "_", plotname)
pdf(
  paste(plotname, ".pdf", sep = ""),
  width = 7,
  height = 7,
  paper = 'special'
)
p <-
  graphSubmissionStats(plotdata,
                       midquote,
                       name,
                       legend.labels,
                       tick.breaks,
                       tick.labels,
                       bartype)
p
dev.off()

rm(
  type,
  side,
  scale,
  p,
  bartype,
  plotdata,
  legend.labels,
  tick.labels,
  tick.breaks,
  midquote
)

####################################################
####################################################
###BY DATE
####################################################
####################################################

####################################################
###BREAKDOWN BETWEEN MPID AND ANON SUBMISSION VOLUMES
####################################################
level <- "-Inf.Inf"
side <- "TOTAL."
scale <- "DVOL."
select <- c(paste0("SUB.", c("ANON.", "MPID."), side, scale, level))
plotdata <- subdate2[, select]
plotdata <- aggregate(plotdata, by = list(subdate2$DATE), sum, na.rm = T)
colnames(plotdata) <- c("t", "ANON", "MPID")
plotdata <- melt(plotdata, id.vars = 't')

legend.labels <- c("ANON", "MPID")
tick.breaks <- plotdata$t
tick.labels <- plotdata$t
midquote <- subdate2$MIDQUOTE

#filled plot
bartype <- "fill"
name <-
  paste0("Plot_Submissions_MPID_Anon_byDate_",
         side,
         scale,
         level,
         "_",
         bartype,
         "_",
         w,
         "sec")
plotname <-
  paste0(graphDirectory, "/", name)
plotname <- gsub("\\.", "_", plotname)
pdf(
  paste(plotname, ".pdf", sep = ""),
  width = 7,
  height = 7,
  paper = 'special'
)
p <-
  graphSubmissionStats(plotdata,
                       midquote,
                       name,
                       legend.labels,
                       tick.breaks,
                       tick.labels,
                       bartype)
p
dev.off()

#stacked plot
bartype <- "stack"
name <-
  paste0("Plot_Submissions_MPID_Anon_byDate_",
         side,
         scale,
         level,
         "_",
         bartype,
         "_",
         w,
         "sec")
plotname <-
  paste0(graphDirectory, "/", name)
plotname <- gsub("\\.", "_", plotname)
pdf(
  paste(plotname, ".pdf", sep = ""),
  width = 7,
  height = 7,
  paper = 'special'
)
p <-
  graphSubmissionStats(plotdata,
                       midquote,
                       name,
                       legend.labels,
                       tick.breaks,
                       tick.labels,
                       bartype)
p
dev.off()

rm(
  level,
  side,
  scale,
  p,
  bartype,
  plotdata,
  legend.labels,
  tick.labels,
  tick.breaks,
  midquote
)

####################################################
###BREAKDOWN BETWEEN BUY AND SELL SUBMISSION VOLUMES
####################################################

level <- "-Inf.Inf"
type <- "MPID."
scale <- "DVOL."
select <- c(paste0("SUB.", type, c("BUY.", "SELL."), scale, level))
plotdata <- subdate2[, select]

plotdata <- aggregate(plotdata, by = list(subdate2$DATE), sum, na.rm = T)
colnames(plotdata) <- c("t", "BUY", "SELL")
plotdata <- melt(plotdata, id.vars = 't')

legend.labels <- c("Buy", "Sell")
tick.breaks <- plotdata$t
tick.labels <- plotdata$t
midquote <- subdate2$MIDQUOTE

#filled plot
bartype <- "fill"
name <-
  paste0("Plot_Submissions_Buy_Sell_byDate_",
         type,
         scale,
         level,
         "_",
         bartype,
         "_",
         w,
         "sec")
plotname <-
  paste0(graphDirectory, "/", name)
plotname <- gsub("\\.", "_", plotname)
pdf(
  paste(plotname, ".pdf", sep = ""),
  width = 7,
  height = 7,
  paper = 'special'
)
p <-
  graphSubmissionStats(plotdata,
                       midquote,
                       name,
                       legend.labels,
                       tick.breaks,
                       tick.labels,
                       bartype)
p
dev.off()

#stacked plot
bartype <- "stack"
name <-
  paste0("Plot_Submissions_Buy_Sell_byDate_",
         type,
         scale,
         level,
         "_",
         bartype,
         "_",
         w,
         "sec")
plotname <-
  paste0(graphDirectory, "/", name)
plotname <- gsub("\\.", "_", plotname)
pdf(
  paste(plotname, ".pdf", sep = ""),
  width = 7,
  height = 7,
  paper = 'special'
)
p <-
  graphSubmissionStats(plotdata,
                       midquote,
                       name,
                       legend.labels,
                       tick.breaks,
                       tick.labels,
                       bartype)
p
dev.off()

rm(level,
   type,
   scale,
   p,
   bartype,
   plotdata,
   legend.labels,
   tick.labels,
   tick.breaks)

####################################################
###BREAKDOWN BETWEEN SUBMISSIONS BY BOOK LEVEL
####################################################

type <- "ALL."
scale <- "DVOL."
side <- "TOTAL."
select <-
  c(paste0("SUB.", type, side, scale, c("-Inf.Inf", "1.1", "1.3", "3.Inf")))
plotdata <- subdate2[, select]
plotdata[, 1] <- plotdata[, 1] - plotdata[, 2] - plotdata[, 3] - plotdata[, 4]
if (sum(plotdata[, 1] < (-1e-10)) > 0) {
  stop("negative order totals")
}
plotdata[, 1] <- replace(plotdata[, 1], plotdata[, 1] < 0, 0)

plotdata <- aggregate(plotdata, by = list(subdate2$DATE), sum, na.rm = T)
colnames(plotdata) <- c("t", "-Inf.Inf", "1.1", "1.3", "3.Inf")
plotdata <- melt(plotdata, id.vars = 't')

legend.labels <-
  c("Price Improving", "Book Level 1", "Book Level 1-3", "Book Level 3+")
tick.breaks <- plotdata$t
tick.labels <- plotdata$t
midquote <- subdate2$MIDQUOTE

#filled plot
bartype <- "fill"
name <-
  paste0("Plot_Submissions_BookLevel_byDate_",
         type,
         side,
         scale,
         bartype,
         "_",
         w,
         "sec")
plotname <-
  paste0(graphDirectory, "/", name)
plotname <- gsub("\\.", "_", plotname)
pdf(
  paste(plotname, ".pdf", sep = ""),
  width = 7,
  height = 7,
  paper = 'special'
)
p <-
  graphSubmissionStats(plotdata,
                       midquote,
                       name,
                       legend.labels,
                       tick.breaks,
                       tick.labels,
                       bartype)
p
dev.off()

#stacked plot
bartype <- "stack"
name <-
  paste0("Plot_Submissions_BookLevel_byDate_",
         type,
         side,
         scale,
         bartype,
         "_",
         w,
         "sec")
plotname <-
  paste0(graphDirectory, "/", name)
plotname <- gsub("\\.", "_", plotname)
pdf(
  paste(plotname, ".pdf", sep = ""),
  width = 7,
  height = 7,
  paper = 'special'
)
p <-
  graphSubmissionStats(plotdata,
                       midquote,
                       name,
                       legend.labels,
                       tick.breaks,
                       tick.labels,
                       bartype)
p
dev.off()

rm(
  type,
  side,
  scale,
  p,
  bartype,
  plotdata,
  legend.labels,
  tick.labels,
  tick.breaks,
  midquote
)

####################################################
####################################################
###BY STOCK
####################################################
####################################################

ticker <- "AAPL"

####################################################
###BREAKDOWN BETWEEN MPID AND ANON SUBMISSION VOLUMES
####################################################

####################################################
###BY DATETIME
####################################################

level <- "-Inf.Inf"
side <- "TOTAL."
scale <- "DVOL."
select <- c(paste0("SUB.", c("ANON.", "MPID."), side, scale, level))
rowInd <- which(subfull$TICK == ticker)
plotdata <- subfull[rowInd, select]
plotdata$DATETIME <- datetime
cols <- c("DATETIME", select)
plotdata <- plotdata[, cols]
rm(cols)
colnames(plotdata) <- c("t", "ANON", "MPID")
plotdata <- melt(plotdata, id.vars = 't')

legend.labels <- c("ANON", "MPID")
tick.breaks <- plotdata$t[seq(1, length(clocktime), noint)]
tick.labels <- plotdata$t[seq(1, length(clocktime), noint)]
midquote <- subfull$MIDQUOTE[rowInd]

#filled plot
bartype <- "fill"
name <-
  paste0(
    "Plot_Submissions_MPID_Anon_",
    ticker,
    "_byDateTime_",
    side,
    scale,
    level,
    "_",
    bartype,
    "_",
    w,
    "sec"
  )
plotname <-
  paste0(graphDirectory, "/", name)
plotname <- gsub("\\.", "_", plotname)
pdf(
  paste(plotname, ".pdf", sep = ""),
  width = 7,
  height = 7,
  paper = 'special'
)
p <-
  graphSubmissionStats(plotdata,
                       midquote,
                       name,
                       legend.labels,
                       tick.breaks,
                       tick.labels,
                       bartype)
p
dev.off()

#stacked plot
bartype <- "stack"
name <-
  paste0(
    "Plot_Submissions_MPID_Anon_",
    ticker,
    "_byDateTime_",
    side,
    scale,
    level,
    "_",
    bartype,
    "_",
    w,
    "sec"
  )
plotname <-
  paste0(graphDirectory, "/", name)
plotname <- gsub("\\.", "_", plotname)
pdf(
  paste(plotname, ".pdf", sep = ""),
  width = 7,
  height = 7,
  paper = 'special'
)
p <-
  graphSubmissionStats(plotdata,
                       midquote,
                       name,
                       legend.labels,
                       tick.breaks,
                       tick.labels,
                       bartype)
p
dev.off()

####################################################
###BY DATE
####################################################

level <- "-Inf.Inf"
side <- "TOTAL."
scale <- "DVOL."
select <- c(paste0("SUB.", c("ANON.", "MPID."), side, scale, level))
rowInd <- which(subfull$TICK == ticker)
plotdata <- subfull[rowInd, select]
DATE <- as.character(rep(dates, each = noint))
plotdata <- aggregate(plotdata, by = list(DATE), sum, na.rm = T)
colnames(plotdata) <- c("t", "ANON", "MPID")
plotdata <- melt(plotdata, id.vars = 't')

legend.labels <- c("ANON", "MPID")
tick.breaks <- plotdata$t[seq(1, length(clocktime), noint)]
tick.labels <- plotdata$t[seq(1, length(clocktime), noint)]
midquote <-
  aggregate(subfull$MIDQUOTE[rowInd],
            by = list(DATE),
            mean,
            na.rm = T)$x

#filled plot
bartype <- "fill"
name <-
  paste0(
    "Plot_Submissions_MPID_Anon_",
    ticker,
    "_byDate_",
    side,
    scale,
    level,
    "_",
    bartype,
    "_",
    w,
    "sec"
  )
plotname <-
  paste0(graphDirectory, "/", name)
plotname <- gsub("\\.", "_", plotname)
pdf(
  paste(plotname, ".pdf", sep = ""),
  width = 7,
  height = 7,
  paper = 'special'
)
p <-
  graphSubmissionStats(plotdata,
                       midquote,
                       name,
                       legend.labels,
                       tick.breaks,
                       tick.labels,
                       bartype)
p
dev.off()

#stacked plot
bartype <- "stack"
name <-
  paste0(
    "Plot_Submissions_MPID_Anon_",
    ticker,
    "_byDate_",
    side,
    scale,
    level,
    "_",
    bartype,
    "_",
    w,
    "sec"
  )
plotname <-
  paste0(graphDirectory, "/", name)
plotname <- gsub("\\.", "_", plotname)
pdf(
  paste(plotname, ".pdf", sep = ""),
  width = 7,
  height = 7,
  paper = 'special'
)
p <-
  graphSubmissionStats(plotdata,
                       midquote,
                       name,
                       legend.labels,
                       tick.breaks,
                       tick.labels,
                       bartype)
p
dev.off()

rm(
  level,
  side,
  scale,
  p,
  bartype,
  plotdata,
  legend.labels,
  tick.labels,
  tick.breaks,
  midquote
)

####################################################
###BREAKDOWN BETWEEN BUY AND SELL SUBMISSION VOLUMES
####################################################

####################################################
###BY DATETIME
####################################################

level <- "-Inf.Inf"
type <- "ALL."
scale <- "DVOL."
select <- c(paste0("SUB.", type, c("BUY.", "SELL."), scale, level))
rowInd <- which(subfull$TICK == ticker)
plotdata <- subfull[rowInd, select]
plotdata$DATETIME <- datetime
cols <- c("DATETIME", select)
plotdata <- plotdata[, cols]
rm(cols)
colnames(plotdata) <- c("t", "BUY", "SELL")
plotdata <- melt(plotdata, id.vars = 't')

legend.labels <- c("Buy", "Sell")
tick.breaks <- plotdata$t[seq(1, length(clocktime), noint)]
tick.labels <- plotdata$t[seq(1, length(clocktime), noint)]
midquote <- subfull$MIDQUOTE[rowInd]

#filled plot
bartype <- "fill"
name <-
  paste0(
    "Plot_Submissions_Buy_Sell_",
    ticker,
    "_byDateTime_",
    type,
    scale,
    level,
    "_",
    bartype,
    "_",
    w,
    "sec"
  )
plotname <-
  paste0(graphDirectory, "/", name)
plotname <- gsub("\\.", "_", plotname)
pdf(
  paste(plotname, ".pdf", sep = ""),
  width = 7,
  height = 7,
  paper = 'special'
)
p <-
  graphSubmissionStats(plotdata,
                       midquote,
                       name,
                       legend.labels,
                       tick.breaks,
                       tick.labels,
                       bartype)
p
dev.off()

#stacked plot
bartype <- "stack"
name <-
  paste0(
    "Plot_Submissions_Buy_Sell_",
    ticker,
    "_byDateTime_",
    type,
    scale,
    level,
    "_",
    bartype,
    "_",
    w,
    "sec"
  )
plotname <-
  paste0(graphDirectory, "/", name)
plotname <- gsub("\\.", "_", plotname)
pdf(
  paste(plotname, ".pdf", sep = ""),
  width = 7,
  height = 7,
  paper = 'special'
)
p <-
  graphSubmissionStats(plotdata,
                       midquote,
                       name,
                       legend.labels,
                       tick.breaks,
                       tick.labels,
                       bartype)
p
dev.off()

rm(
  level,
  type,
  scale,
  p,
  bartype,
  plotdata,
  legend.labels,
  tick.labels,
  tick.breaks,
  midquote
)

####################################################
###BY DATE
####################################################

level <- "-Inf.Inf"
type <- "ALL."
scale <- "DVOL."
select <- c(paste0("SUB.", type, c("BUY.", "SELL."), scale, level))
rowInd <- which(subfull$TICK == ticker)
plotdata <- subfull[rowInd, select]
DATE <- as.character(rep(dates, each = noint))
plotdata <- aggregate(plotdata, by = list(DATE), sum, na.rm = T)
colnames(plotdata) <- c("t", "ANON", "MPID")
plotdata <- melt(plotdata, id.vars = 't')

legend.labels <- c("Buy", "Sell")
tick.breaks <- plotdata$t[seq(1, length(clocktime), noint)]
tick.labels <- plotdata$t[seq(1, length(clocktime), noint)]
midquote <-
  aggregate(subfull$MIDQUOTE[rowInd],
            by = list(DATE),
            mean,
            na.rm = T)$x

#filled plot
bartype <- "fill"
name <-
  paste0(
    "Plot_Submissions_Buy_Sell_",
    ticker,
    "_byDate_",
    type,
    scale,
    level,
    "_",
    bartype,
    "_",
    w,
    "sec"
  )
plotname <-
  paste0(graphDirectory, "/", name)
plotname <- gsub("\\.", "_", plotname)
pdf(
  paste(plotname, ".pdf", sep = ""),
  width = 7,
  height = 7,
  paper = 'special'
)
p <-
  graphSubmissionStats(plotdata,
                       midquote,
                       name,
                       legend.labels,
                       tick.breaks,
                       tick.labels,
                       bartype)
p
dev.off()

#stacked plot
bartype <- "stack"
name <-
  paste0(
    "Plot_Submissions_Buy_Sell_",
    ticker,
    "_byDate_",
    type,
    scale,
    level,
    "_",
    bartype,
    "_",
    w,
    "sec"
  )
plotname <-
  paste0(graphDirectory, "/", name)
plotname <- gsub("\\.", "_", plotname)
pdf(
  paste(plotname, ".pdf", sep = ""),
  width = 7,
  height = 7,
  paper = 'special'
)
p <-
  graphSubmissionStats(plotdata,
                       midquote,
                       name,
                       legend.labels,
                       tick.breaks,
                       tick.labels,
                       bartype)
p
dev.off()

rm(
  level,
  type,
  scale,
  p,
  bartype,
  plotdata,
  legend.labels,
  tick.labels,
  tick.breaks,
  midquote
)

####################################################
###BREAKDOWN BETWEEN SUBMISSIONS BY BOOK LEVEL
####################################################

####################################################
###BY DATETIME
####################################################

type <- "MPID."
scale <- "DVOL."
side <- "TOTAL."
select <-
  c(paste0("SUB.", type, side, scale, c("-Inf.Inf", "1.1", "1.3", "3.Inf")))
rowInd <- which(subfull$TICK == ticker)
plotdata <- subfull[rowInd, select]
plotdata$DATETIME <- datetime
cols <- c("DATETIME", select)
plotdata <- plotdata[, cols]
rm(cols)
colnames(plotdata) <- c("t", "-Inf.Inf", "1.1", "1.3", "3.Inf")
plotdata <- melt(plotdata, id.vars = 't')

legend.labels <-
  c("Price Improving", "Book Level 1", "Book Level 1-3", "Book Level 3+")
tick.breaks <- plotdata$t[seq(1, length(clocktime), noint)]
tick.labels <- plotdata$t[seq(1, length(clocktime), noint)]
midquote <- subfull$MIDQUOTE[rowInd]

#filled plot
bartype <- "fill"
name <-
  paste0(
    "Plot_Submissions_BookLevel_",
    ticker,
    "_byDateTime_",
    type,
    side,
    scale,
    bartype,
    "_",
    w,
    "sec"
  )
plotname <-
  paste0(graphDirectory, "/", name)
plotname <- gsub("\\.", "_", plotname)
pdf(
  paste(plotname, ".pdf", sep = ""),
  width = 7,
  height = 7,
  paper = 'special'
)
p <-
  graphSubmissionStats(plotdata,
                       midquote,
                       name,
                       legend.labels,
                       tick.breaks,
                       tick.labels,
                       bartype)
p
dev.off()

#stacked plot
bartype <- "stack"
name <-
  paste0(
    "Plot_Submissions_BookLevel_",
    ticker,
    "_byDateTime_",
    type,
    side,
    scale,
    bartype,
    "_",
    w,
    "sec"
  )
plotname <-
  paste0(graphDirectory, "/", name)
plotname <- gsub("\\.", "_", plotname)
pdf(
  paste(plotname, ".pdf", sep = ""),
  width = 7,
  height = 7,
  paper = 'special'
)
p <-
  graphSubmissionStats(plotdata,
                       midquote,
                       name,
                       legend.labels,
                       tick.breaks,
                       tick.labels,
                       bartype)
p
dev.off()

rm(
  type,
  side,
  scale,
  p,
  bartype,
  plotdata,
  legend.labels,
  tick.labels,
  tick.breaks,
  midquote
)

####################################################
###BY DATE
####################################################

type <- "MPID."
scale <- "DVOL."
side <- "TOTAL."
select <-
  c(paste0("SUB.", type, side, scale, c("-Inf.Inf", "1.1", "1.3", "3.Inf")))
rowInd <- which(subfull$TICK == ticker)
plotdata <- subfull[rowInd, select]
DATE <- as.character(rep(dates, each = noint))
plotdata <- aggregate(plotdata, by = list(DATE), sum, na.rm = T)
colnames(plotdata) <- c("t", "-Inf.Inf", "1.1", "1.3", "3.Inf")
plotdata <- melt(plotdata, id.vars = 't')

legend.labels <-
  c("Price Improving", "Book Level 1", "Book Level 1-3", "Book Level 3+")
tick.breaks <- plotdata$t[seq(1, length(clocktime), noint)]
tick.labels <- plotdata$t[seq(1, length(clocktime), noint)]
midquote <-
  aggregate(subfull$MIDQUOTE[rowInd],
            by = list(DATE),
            mean,
            na.rm = T)$x

#filled plot
bartype <- "fill"
name <-
  paste0(
    "Plot_Submissions_BookLevel_",
    ticker,
    "_byDate_",
    type,
    side,
    scale,
    bartype,
    "_",
    w,
    "sec"
  )
plotname <-
  paste0(graphDirectory, "/", name)
plotname <- gsub("\\.", "_", plotname)
pdf(
  paste(plotname, ".pdf", sep = ""),
  width = 7,
  height = 7,
  paper = 'special'
)
p <-
  graphSubmissionStats(plotdata,
                       midquote,
                       name,
                       legend.labels,
                       tick.breaks,
                       tick.labels,
                       bartype)
p
dev.off()

#stacked plot
bartype <- "stack"
name <-
  paste0(
    "Plot_Submissions_BookLevel_",
    ticker,
    "_byDate_",
    type,
    side,
    scale,
    bartype,
    "_",
    w,
    "sec"
  )
plotname <-
  paste0(graphDirectory, "/", name)
plotname <- gsub("\\.", "_", plotname)
pdf(
  paste(plotname, ".pdf", sep = ""),
  width = 7,
  height = 7,
  paper = 'special'
)
p <-
  graphSubmissionStats(plotdata,
                       midquote,
                       name,
                       legend.labels,
                       tick.breaks,
                       tick.labels,
                       bartype)
p
dev.off()

rm(
  type,
  side,
  scale,
  p,
  bartype,
  plotdata,
  legend.labels,
  tick.labels,
  tick.breaks,
  midquote
)
