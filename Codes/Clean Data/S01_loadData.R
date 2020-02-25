library(sparklyr)
library(dplyr)
library(DBI)
#spark_install(version = "2.4")

HOME <- Sys.getenv("HOME")
dataRoot <- paste(HOME, "Dropbox", "LobsterOnSpark", sep="/")
messageRootDir <- paste(dataRoot, "message", sep="/")
orderBookRootDir <- paste(dataRoot, "orderBook", "level=10", sep="/")

conf <- spark_config()  
conf$`sparklyr.cores.local` <- 4
conf$`sparklyr.shell.driver-memory` <- "4G"  # in windows I only be able to use 1G
conf$spark.memory.fraction <- 0.9

sc <- spark_connect(master="local", config = conf) # http://localhost:4040 to monitor spark

### Data set is too big for the local server
#
# The following code could work in cluster but not local machine
#
# messages <- spark_read_parquet(sc, path=messageRootDir)
# orderbooks <- spark_read_parquet(sc, path=orderBookRootDir)

# messages_ATVI <- collect(filter(messages, symbol=="ATVI", date==as.Date("2020-02-03")))

myDate <- "2020-02-03"
myDateDir <- paste("date", myDate, sep="=")
oneSymbol <- "ATVI"

myMessages <- spark_read_parquet(sc, path=paste(messageRootDir, myDateDir, sep="/"))

# Use
# sdf_schema(myMessages)
# sdf_schema(myOrderbooks)
#
# to see the schema.

oneSymbolMPIDMessage <- collect(filter(myMessages, symbol==oneSymbol, attribution!=""))
head(oneSymbolMPIDMessage)

myOrderbooks <- spark_read_parquet(sc, path=paste(orderBookRootDir, myDateDir, sep="/"))
oneSymbolOrderBook <- collect(filter(myOrderbooks, symbol==oneSymbol))
head(oneSymbolOrderBook)

spark_disconnect(sc)
