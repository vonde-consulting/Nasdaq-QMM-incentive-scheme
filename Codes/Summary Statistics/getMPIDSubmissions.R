function(M,
         dates,
         ticks,
         mergedDirectory) {
  #' Aggregate information about daily submissions by a particular Nasdaq participant by stock.
  #'
  #' @description This function produces a t x n matrix of order submissions by a given Nasdaq participant, where t is
  #' the number of days and n is the number of Nasdaq stocks in the sample.
  #'
  #' @param M Character scalar containing the Nasdaq participant's MPID.
  #' @param dates Vector of length t containing the dates
  #' @param ticks Vector of length n containing the stock tickers.
  #' @param mergedDirectory Character scalar containing the directory where the raw submissions data is saved.
  #'
  #' @usage getMPIDSubmissions(M,dates,ticks,mergedDirectory)
  #'
  #' @return t x n matrix of order submissions by a given Nasdaq participant, where t is
  #' the number of days and n is the number of Nasdaq stocks in the sample.
  
  
  
  SUB_MPID <-
    matrix(0, length(dates), length(ticks))
  colnames(SUB_MPID) <- ticks
  rownames(SUB_MPID) <- dates
  
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
      
      filename <- paste0(mergedDirectory, "/", tick, "/", tick, "_", date, ".Rds")
      load(file = filename)
      rm(filename)
      
      data <- which(data$eventtype == 1 & data$mpid == M)
      SUB_MPID[j, i] <- length(data)
      
      rm(data)
      
    }
  }
  
  return(SUB_MPID)
  
}