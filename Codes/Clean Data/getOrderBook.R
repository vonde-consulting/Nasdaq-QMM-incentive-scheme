# Title     : Load Order book
# Objective : Load Order book from parquet file
# Created by: Ruihong Huang
# Created on: 24/05/2020

# Prerequired:
#
# > install.packages("data.table")
# > install.packages("reticulate")
# > install.packages("arrow")
# > arrow::install_pyarrow()
#
# restart R studio or R session

library(reticulate)
getOneDayOrderBook <- function(orderbook_directory, date){
  pd <- import("pandas")
  order_books <- pd$read_parquet(sprintf("%s/date=%s",  orderbook_directory, date))
  order_books$date = date
  order_books
}

getAllOrderBook <- function(orderbook_directory){
  pd <- import("pandas")
  order_books <- pd$read_parquet(orderbook_directory)
  order_books
}
