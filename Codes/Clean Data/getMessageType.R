function(condition,
         direction,
         sz,
         dvol,
         string1,
         string2) {
  #' Calculate the amount of order book update messages.
  #'
  #' @description This function calculates the amount of limit order book updates (in terms of (1) number of messages, 
  #' (2) share volume of messages, and (3) dollar volumes of messages) during a particular interval t. 
  #' Limit order book update types can include submissions, cancellations, and executions.
  #' 
  #' Define n as the number of observed order book update messages during interval t.
  #'
  #' @param condition Logical vector of length n specifying which order book messages should be included in the
  #' calculation (e.g., to calculate number of submissions, equal to T for order submissions, F otherwise)
  #' @param direction Vector of length n of trade directions associated with order book updates (1 for buy, -1 for sell).
  #' @param sz Vector of length n of the sizes (in terms of number of shares) of order book updates during interval t.
  #' @param dvol Vector of length n of dollar volumes (sz*price) of order book updates during interval t.
  #' @param string1 String specifying message type (e.g., "SUB","EXE","CANC")
  #' @param string2 String to further modify message type (e.g., "MPID","ANON",etc.)
  #' 
  #' @usage getMessageType(condition,  direction,  sz,  dvol,  string1,  string2) 
  #' 
  #' @return Vector of length 9 containing amount of messages in terms of: (1) number of buy-side ("BUY.NUM); (2)
  #' number of sell-side ("SELL.NUM"); (3) total number ("ALL.NUM"); (4) share volume of buy-side ("BUY.SVOL"); (5)
  #' share volume of sell-side ("SELL.SVOL"); (6) total share volume ("ALL.SVOL"); (7) dollar volume of buy-side
  #' ("BUY.DVOL"); (8) dollar volume of sell-side ("SELL.DVOL"); (9) total dollar volum ("ALL.DVOL").   
  
  #In Terms of Numbers of Messages
  totalNum <- sum(condition)
  buyNum <- sum(condition & direction == 1)
  sellNum <- sum(condition & direction == -1)
  if (buyNum + sellNum != totalNum) {
    stop("Buy and Sell numbers don't add up")
  }
  
  #In Terms of Share Volumes of Messages
  totalSVol <- sum(condition * sz)
  buySVol <- sum((condition & direction == 1) * sz)
  sellSVol <- sum((condition & direction == -1) * sz)
  if (buySVol + sellSVol != totalSVol) {
    stop("Buy and Sell share volumes don't add up")
  }
  
  #In Terms of Dollar Volumes of Messages
  totalDVol <- sum(condition * dvol)
  buyDVol <- sum((condition & direction == 1) * dvol)
  sellDVol <- sum((condition & direction == -1) * dvol)
  if (round(buyDVol + sellDVol, digits = 4) != round(totalDVol, digits =
                                                     4)) {
    stop("Buy and Sell dollar volumes don't add up")
  }
  
  messages <-
    c(
      buyNum,
      sellNum,
      totalNum,
      buySVol,
      sellSVol,
      totalSVol,
      buyDVol,
      sellDVol,
      totalDVol
    )
  names(messages) <-
    c(
      paste0(string1, ".", string2, ".", "BUY.NUM"),
      paste0(string1, ".", string2, ".", "SELL.NUM"),
      paste0(string1, ".", string2, ".", "TOTAL.NUM"),
      paste0(string1, ".", string2, ".", "BUY.SVOL"),
      paste0(string1, ".", string2, ".", "SELL.SVOL"),
      paste0(string1, ".", string2, ".", "TOTAL.SVOL"),
      paste0(string1, ".", string2, ".", "BUY.DVOL"),
      paste0(string1, ".", string2, ".", "SELL.DVOL"),
      paste0(string1, ".", string2, ".", "TOTAL.DVOL")
    )
  
  return(messages)
  
}  
