rm(list = ls())
library(ggplot2)

#Time: Seconds after midnight with decimal precision of at least milliseconds and up to nanoseconds
#depending on the period requested

#Price: Dollar price times 10000 (i.e. a stock price of $91.14 is given by 911400)

#ORDER TYPES
#1: Submission of a new limit order
#2: Cancellation (partial deletion of a limit order)
#3: Deletion (total deletion of a limit order)
#4: Execution of a visible limit order
#5: Execution of a hidden limit order
#6: Indicates a cross trade, e.g. auction trade
#7: Trading halt indicator (detailed information below)

#DIRECTION
#-1: Sell limit order
#1: Buy limit order
#Note: Execution of a sell (buy) limit order corresponds to a buyer (seller) initiated trade, i.e. buy (sell) trade.

direc <- "C:/DatiLocali/Dropbox/Trader Anonymity/Data/RawData/"
stock <- "AAPL"
day <- "04"

##READ IN RAW DATA
file <-
  paste0(direc,
         stock,
         "_2013-11-",
         day,
         "_34200000_59400000_message_10.csv")
cols <-
  c("time",
    "eventtype",
    "orderID",
    "size",
    "price",
    "direction",
    "mpid")
mydata <-
  read.csv(
    file = file,
    header = F,
    col.names = cols,
    stringsAsFactors = F
  )
rm(file, cols)
##READ IN RAW DATA
file <-
  paste0(direc,
         stock,
         "_2013-11-",
         day,
         "_34200000_59400000_orderbook_10.csv")
Cols <-
  c(
    "askprice1",
    "asksize1",
    "bidprice1",
    "bidsize1",
    "askprice2",
    "asksize2",
    "bidprice2",
    "bidsize2",
    "askprice3",
    "asksize3",
    "bidprice3",
    "bidsize3",
    "askprice4",
    "asksize4",
    "bidprice4",
    "bidsize4",
    "askprice5",
    "asksize5",
    "bidprice5",
    "bidsize5",
    "askprice6",
    "asksize6",
    "bidprice6",
    "bidsize6",
    "askprice7",
    "asksize7",
    "bidprice7",
    "bidsize7",
    "askprice8",
    "asksize8",
    "bidprice8",
    "bidsize8",
    "askprice9",
    "asksize9",
    "bidprice9",
    "bidsize9",
    "askprice10",
    "asksize10",
    "bidprice10",
    "bidsize10"
  )
mydata2 <-
  read.csv(
    file = file,
    header = F,
    col.names = Cols,
    stringsAsFactors = F
  )
rm(file)
mydata2[, seq(1, 40, 2)] <- mydata2[, seq(1, 40, 2)] / 10000
mydata <- cbind(mydata, mydata2)
rm(mydata2)

##REMOVE EXECUTION OF HIDDEN ORDERS
mydata <- mydata[mydata$eventtype != 5, ]

##ADJUST PRICE
mydata$price <- mydata$price / 10000

##RECODE MPID
#mydata$mpid<-replace(mydata$mpid,mydata$mpid=="null",0)
#mydata$mpid<-replace(mydata$mpid,mydata$mpid=="UBSS",1)
#mydata$mpid<-replace(mydata$mpid,mydata$mpid=="TMBR",2)
#mydata$mpid<-replace(mydata$mpid,mydata$mpid=="SBSH",3)
#mydata$mpid<-as.integer(mydata$mpid)

##GENERATE CLOCK TIME
mydata$clocktime <-
  format(as.POSIXct((mydata$time), origin = "1970-01-01", tz = "UTC"), "%H:%M:%OS6")

##SEPARATE SUBMISSIONS AND EXECUTIONS/CANCELLATIONS
sub <- mydata[mydata$eventtype == 1, ]
execanc <-
  mydata[mydata$eventtype == 2 |
           mydata$eventtype == 3 | mydata$eventtype == 4, ]
#rm(mydata)

##RUN THROUGH LOOP
min <- 1:60 #snapshot every minute for 1 hour

##SET UP EARLIEST INTERVAL TIME (for first loop, this is the beginning of the trading day)
oldtesttime <- min(mydata$time)
##FIRST INTERVAL ("TEST TIME"): 10:30am
testtime <- 10.5 * 60 * 60
##INPUT MATRICES
cols <- c("size", "mpid", "price", "direction", "interval")
subplot <-
  data.frame(matrix(NA, 0, length(cols)))
colnames(subplot) <- cols
mids <-
  data.frame(cbind(1:length(min),  rep(NA, length(min))))
colnames(mids) <- c("interval", "mid")
subold <-
  data.frame(matrix(NA, 0, length(Cols)))
colnames(subold) <- Cols
xlabels <- rep(NA, length(min))

for (y in 1:length(min)) {
  #find state of the order book at closest timestap before testtime
  orbk <-
    mydata[mydata$time <= testtime, ]
  orbk <-
    orbk[nrow(orbk), c(1, 8:11)]
  orbk$mid <- (orbk$askprice1 + orbk$bidprice1) / 2
  mids$mid[y] <- orbk$mid
  #find submissions before this interval but after previous interval
  subt <- sub[which(sub$time < testtime & sub$time >= oldtesttime), ]
  subt <- rbind(subold, subt) #add orders from previous interval
  #find orders that have been executed or cancelled prior to timestamp
  execanct <-
    execanc[which(
      execanc$orderID %in% subt$orderID &
        execanc$time < testtime & execanc$time >= oldtesttime
    ), ]
  #sum across executions and cancellations (may be several partial or combo)
  if (nrow(execanct) != 0) {
    totalect <-
      aggregate(execanct$size,
                by = list(Category = execanct$orderID),
                FUN = sum)
    colnames(totalect) <- c("orderID", "size")
    #discard orders that have been completely executed/cancelled by timestamp
    i <- match(subt$orderID, totalect$orderID)
    subt$execanc <-
      totalect$size[i]
    rm(i) #this is the amount of the submitted order that has been executed or cancelled as of timestamp
    subt$execanc <-
      replace(subt$execanc, is.na(subt$execanc) == 1, 0) #replace missing with 0
    subt$remainder <-
      subt$size - subt$execanc #this is the amount of the order remaining as of the timestamp
    if (length(q <-
               which(subt$remainder < 0)) != 0) {
      stop("Negative remainders")
    } #sanity check
    rm(totalect)
  } else {
    subt$remainder <- subt$size
  }
  rm(execanct)
  subt <-
    subt[subt$remainder != 0, ] #these should represent the orders that have not yet been executed
  #discard buy orders that are higher than the best bid and sell orders that are lower than the best ask
  subt <-
    subt[which((subt$direction == 1 &
                  subt$price <= orbk$bidprice1) |
                 (subt$direction == -1 & subt$price >= orbk$askprice1)
    ), ]
  
  #aggregate across MPID type, price and direction
  suba <- aggregate(size ~ mpid + direction + price, subt, sum)
  suba <- suba[order(suba$price), ]
  
  #only keep x best buy and sell orders
  #x<-50
  #buy<-suba[suba$direction==1,]; buy<-buy[order(buy$price,decreasing=T),]
  #buy<-buy[1:x,]; buy<-buy[order(buy$price),]
  #sell<-suba[suba$direction==-1,]; sell<-sell[order(sell$price),]
  #sell<-sell[1:x,]; sell<-sell[order(sell$price),]
  #suba<-rbind(buy,sell)
  
  #subt[which(subt$orderID=="1059989"),]
  #totalect[which(totalect$orderID=="1059989"),]
  
  suba$interval <- y
  suba <- suba[, match(colnames(suba), cols)]
  subplot <- rbind(subplot, suba)
  subold <- subt[, 1:48]
  rm(suba, subt)
  
  oldtesttime <- testtime
  xlabels[y] <- testtime
  testtime <- testtime + 60
  
}

#xlabels<-xlabels+60 #such that they represent the ending time of the interval

pdfname <-
  paste0(direc, stock, "_11-", day, "_", xlabels[1], "_", xlabels[length(xlabels)], ".pdf")
pdf(
  file = pdfname,
  width = 6,
  height = 4,
  paper = "special"
)
xlabels <-
  format(as.POSIXct((xlabels), origin = "1970-01-01", tz = "UTC"), "%H:%M")
p <-
  ggplot(subplot, aes(
    x = factor(interval),
    y = price,
    color = mpid
  )) + scale_colour_manual(values = c(
    "null" = "grey",
    "SBSH" = "green",
    "TMBR" = "blue",
    "UBSS" = "red",
    "WEMM" = "pink"
  )) +
  geom_point(alpha = 1 / 4, aes(size = size)) + scale_x_discrete(name =
                                                                   "",
                                                                 breaks = seq(1, 60, 10),
                                                                 labels = xlabels[seq(1, 60, 10)]) +
  ggtitle(paste(stock, "Nov", day))
for (i in 1:length(min)) {
  gg.data <- data.frame(i, mids$mid[i])
  colnames(gg.data) <- c("i", "mid")
  p <- p + geom_point(data = gg.data,
                      aes(x = i, y = mid),
                      shape = 3,
                      colour = "black")
  rm(gg.data)
}
p
dev.off()
