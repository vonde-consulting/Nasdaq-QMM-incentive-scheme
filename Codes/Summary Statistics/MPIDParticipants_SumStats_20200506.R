rm(list = ls(all = TRUE))
library(MTS)
library(zoo)
library(docstring)
library(reshape2)
library(ggplot2)
library(scales)
library(openxlsx)
options(warn = 2)

rootDirectory <- "W:/LOBSTER"
mergedDirectory <- "W:/LOBSTER/merged"
codeDirectory <-
  "C:/DatiLocali/Dropbox/Trader Anonymity/Nasdaq-QMM-incentive-scheme/Codes/Summary Statistics/"

#Load Functions
getMPIDSubmissions <-
  dget(paste(codeDirectory, "getMPIDSubmissions.R", sep = ""),
       keep.source = T)

#get list of all firms and dates
ticks <- list.files(mergedDirectory)
dates <-
  as.POSIXlt(seq(as.Date("2020-02-01"), as.Date("2020-02-28"), "days"), format =
               "%Y-%m-%d")
dates <- dates[!dates$wday %in% c(6, 0)] #get rid of weekdays
dates <-
  dates[dates != as.POSIXlt(as.Date("2020-02-17"), format = "%Y-%m-%d")] #get rid of President's Day
#define different interval lengths (in terms of number of seconds)
windows <- c(60)
#Trading hours (start & end)
startTrad <- 9.5 * 60 * 60
# 9:30:00.000 in ms after midnight
endTrad <- 16 * 60 * 60
# 16:00:00.000 in ms after midnight

######STEP 1: GET LIST OF ALL POSSIBLE MPIDs

if (FALSE) {
  MPID <- matrix(NA, 0, 0)
  
  for (i in 1:length(ticks)) {
    for (j in 1:length(dates)) {
      cat(paste0(
        "Stock ",
        i,
        " out of ",
        length(ticks),
        " : ",
        "Date ",
        j,
        " out of ",
        length(dates),
        "\n"
      ))
      
      tick <- ticks[i]
      date <- dates[j]
      
      filename <-
        paste0(mergedDirectory, "/", tick, "/", tick, "_", date, ".Rds")
      load(file = filename)
      rm(filename)
      
      mpid <- unique(data$mpid)
      MPID <- unique(c(MPID, mpid))
      
    }
  }
  
  MPID <- MPID[order(MPID)]
  filename <- paste0(rootDirectory, "/UniqueMPIDs_20200506.Rds")
  save(MPID, file = filename)
  rm(filename)
  
}

######STEP 2: GET SUBMISSION CONTRIBUTION BY MPID

if (FALSE) {
  filename <- paste0(rootDirectory, "UniqueMPIDs_20200506.Rds")
  load(file = filename)
  rm(filename)
  
  n <- length(MPID)
  
  SUB_MPID <- rep(0, n)
  names(SUB_MPID) <- MPID
  SUB_MPID_BYSTOCK <-
    matrix(0, length(ticks), length(MPID))
  colnames(SUB_MPID_BYSTOCK) <- MPID
  rownames(SUB_MPID_BYSTOCK) <- ticks
  SUB_MPID_BYDATE <-
    matrix(0, length(dates), length(MPID))
  colnames(SUB_MPID_BYDATE) <-
    MPID
  rownames(SUB_MPID_BYDATE) <- as.character(dates)
  
  for (i in 1:length(ticks)) {
    for (j in 1:length(dates)) {
      cat(paste0(
        "Stock ",
        i,
        " out of ",
        length(ticks),
        " : ",
        "Date ",
        j,
        " out of ",
        length(dates),
        "\n"
      ))
      
      tick <- ticks[i]
      date <- dates[j]
      
      filename <-
        paste0(mergedDirectory, "/", tick, "/", tick, "_", date, ".Rds")
      load(file = filename)
      rm(filename)
      
      #create temporary vector with #row=n, equal to zero for MPIDs that are not present for that stock
      data <- data[data$eventtype == 1, ]
      mpid <- table(data$mpid)
      ind <- match(names(mpid), names(SUB_MPID))
      temp <- rep(0, n)
      temp[ind] <- unname(mpid)
      SUB_MPID <- SUB_MPID + temp
      SUB_MPID_BYSTOCK[i, ] <- SUB_MPID_BYSTOCK[i, ] + temp
      SUB_MPID_BYDATE[j, ] <- SUB_MPID_BYDATE[j, ] + temp
      
      rm(data, temp)
      
    }
  }
  
  #AS % OF TOTAL SUBMISSIONS
  SUB_MPID_PP <- round((SUB_MPID / sum(SUB_MPID)) * 100, digits = 3)
  SUB_MPID_BYSTOCK_P <-
    round((SUB_MPID_BYSTOCK / rowSums(SUB_MPID_BYSTOCK)) * 100, digits = 3)
  SUB_MPID_BYDATE_P <-
    round((SUB_MPID_BYDATE / rowSums(SUB_MPID_BYDATE)) * 100, digits = 3)
  
  #AS % OF MPID SUBMISSIONS
  SUB_MPIDN <- SUB_MPID[(names(SUB_MPID) != "null")]
  SUB_MPID_PP <- round((SUB_MPIDN / sum(SUB_MPIDN)) * 100, digits = 3)
  SUB_MPIDN_BYSTOCK <-
    SUB_MPID_BYSTOCK[, (colnames(SUB_MPID_BYSTOCK) != "null")]
  SUB_MPID_BYSTOCK_PP <-
    round((SUB_MPIDN_BYSTOCK / rowSums(SUB_MPIDN_BYSTOCK)) * 100, digits = 3)
  SUB_MPIDN_BYDATE <-
    SUB_MPID_BYDATE[, (colnames(SUB_MPID_BYDATE) != "null")]
  SUB_MPID_BYDATE_PP <-
    round((SUB_MPIDN_BYDATE / rowSums(SUB_MPIDN_BYDATE)) * 100, digits = 3)
  
  filename <- paste0(rootDirectory, "/Submissions_byMPID_20200507.Rds")
  save(SUB_MPID, SUB_MPID_BYSTOCK, SUB_MPID_BYDATE, file = filename)
  rm(filename)
  
}

######STEP 3: CREATE TABLES AND PLOTS

filename <- paste0(rootDirectory, "/Submissions_byMPID_20200507.Rds")
load(file = filename)
rm(filename)

MPID <- colnames(SUB_MPID_BYDATE)
MPIDN <- MPID[MPID != "null"]

SUB_MPIDN_BYDATE <-
  SUB_MPID_BYDATE[, (colnames(SUB_MPID_BYDATE) != "null")]
SUB_MPIDN_BYSTOCK <-
  SUB_MPID_BYSTOCK[, (colnames(SUB_MPID_BYSTOCK) != "null")]
SUB_MPIDN_BYDATE <-
  SUB_MPID_BYDATE[, (colnames(SUB_MPID_BYDATE) != "null")]

#SAVE AS EXCEL TABLES
x <- length(MPIDN)
sums <- colSums(SUB_MPIDN_BYSTOCK)
sumstemp <- sums[order(sums, decreasing = T)]
sumx <- sumstemp[x]
ind <- which(sums >= sumx)
SUB_temp <- SUB_MPIDN_BYSTOCK[, ind]
SUB_MPIDN_BYSTOCK_PP <-
  round((SUB_temp / rowSums(SUB_MPIDN_BYSTOCK)) * 100, digits = 1)

filename <-
  paste0(rootDirectory,
         "/Table_MPIDSubmissionsByStock_20200507.xlsx")
write.xlsx(SUB_MPIDN_BYSTOCK_PP, file = filename, row.names = T)
rm(filename)

#SAVE AS PLOTS

#keep only top x MPIDs
x <- 5
sums <- colSums(SUB_MPIDN_BYDATE)
sumstemp <- sums[order(sums, decreasing = T)]
sumx <- sumstemp[x]
ind <- which(sums >= sumx)
SUB_temp <- SUB_MPIDN_BYDATE[, ind]
MPID_temp <- MPIDN[ind]

temp <- as.vector(SUB_temp)
mpidtemp <-
  rep(MPID_temp, length(dates))
mpidtemp <- mpidtemp[order(mpidtemp)]
datetemp <-
  format(as.Date(as.character(dates)), '%d%b%y')
datetemp <- rep(datetemp, length(MPID_temp))

temp <-
  as.data.frame(cbind(datetemp, mpidtemp, temp), stringsAsFactors = F)
colnames(temp) <- c("Date", "MPID", "Submissions")
temp$Submissions <- as.numeric(temp$Submissions)
#dfm<-melt(temp, id.vars = "Date")
#brks<-seq(1,length(df$day),3)
dd <- format(as.Date(as.character(dates)), '%d%b%y')
brks <-
  order(dd)[!duplicated(sort(dd))]
brks <- brks[order(brks)]
brks <- brks[-1]
labs <- dd[brks]

plotname <-
  paste(rootDirectory, "/MPIDSubmissionVolumebyParticipant", sep = "")
pdf(
  paste(plotname, ".pdf", sep = ""),
  width = 7,
  height = 7,
  paper = 'special'
)
p <-
  ggplot(temp, aes(x = factor(Date), y = Submissions, fill = MPID)) +
  xlab("") +
  ylab("% MPID Submissions") +
  scale_fill_brewer(name = "Nasdaq Participant", palette = "OrRd") +
  geom_bar(position = "fill",
           stat = "identity",
           width = 1) +
  scale_y_continuous(labels = percent_format(), expand = c(0, 0))  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p
dev.off()

#stacked barplot without IMCC
x <- 6
sums <- colSums(SUB_MPIDN_BYDATE)
sumstemp <- sums[order(sums, decreasing = T)]
sumx <- sumstemp[x]
ind <- which(sums >= sumx)
#get rid of IMCC
ind <- ind[MPID[ind] != "IMCC"]
SUB_temp <- SUB_MPIDN_BYDATE[, ind]
MPID_temp <- MPIDN[ind]

temp <- as.vector(SUB_temp)
mpidtemp <-
  rep(MPID_temp, length(dates))
mpidtemp <- mpidtemp[order(mpidtemp)]
datetemp <-
  format(as.Date(as.character(dates)), '%d%b%y')
datetemp <- rep(datetemp, length(MPID_temp))

temp <-
  as.data.frame(cbind(datetemp, mpidtemp, temp), stringsAsFactors = F)
colnames(temp) <- c("Date", "MPID", "Submissions")
temp$Submissions <- as.numeric(temp$Submissions)
#dfm<-melt(temp, id.vars = "Date")
#brks<-seq(1,length(df$day),3)
dd <- format(as.Date(as.character(dates)), '%d%b%y')
brks <-
  order(dd)[!duplicated(sort(dd))]
brks <- brks[order(brks)]
brks <- brks[-1]
labs <- dd[brks]

plotname <-
  paste(rootDirectory,
        "/MPIDSubmissionVolumebyParticipant_ExclIMCC",
        sep = "")
pdf(
  paste(plotname, ".pdf", sep = ""),
  width = 7,
  height = 7,
  paper = 'special'
)
p <-
  ggplot(temp, aes(x = factor(Date), y = Submissions, fill = MPID)) +
  xlab("") +
  ylab("% MPID Submissions") +
  scale_fill_brewer(name = "Nasdaq Participant", palette = "OrRd") +
  geom_bar(position = "fill",
           stat = "identity",
           width = 1) +
  scale_y_continuous(labels = percent_format(), expand = c(0, 0))  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p
dev.off()

####CHECK BEHAVIOR OF INDIVIDUAL MARKET MAKERS

filename <- paste0(rootDirectory, "/UniqueMPIDs_20200506.Rds")
load(file = filename)
rm(filename)
M <- "IMCC"
SUB_MPID <-
  getMPIDSubmissions(M, as.character(dates), ticks, mergedDirectory)

#CREATE PLOT

temp <- as.vector(SUB_MPID)
ticktemp <- rep(ticks, each = length(dates))

datetemp <-
  format(as.Date(as.character(dates)), '%d%b%y')
datetemp <- rep(datetemp, times = length(ticks))

temp <-
  as.data.frame(cbind(datetemp, ticktemp, temp), stringsAsFactors = F)
colnames(temp) <- c("Date", "Stock", "Submissions")
temp$Submissions <- as.numeric(temp$Submissions)

plotname <-
  paste0(rootDirectory, "/MPIDSubmissionVolumebyStock_", M)
pdf(
  paste(plotname, ".pdf", sep = ""),
  width = 7,
  height = 14,
  paper = 'special'
)
p <-
  ggplot(temp, aes(x = factor(Date),
                   y = Submissions,
                   fill = Stock)) +
  xlab("") +
  ylab("% Daily Submissions") +
  ggtitle(paste("Submissions by", M)) +
  #  scale_fill_brewer(name = "Stock",palette="OrRd") +
  geom_bar(position = "fill",
           stat = "identity",
           width = 1) +
  scale_y_continuous(labels = percent_format(), expand = c(0, 0))  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "bottom")
p
dev.off()
