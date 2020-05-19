function(condition,
         direction,
         sz,
         price,
         bid,
         ask,
         minlevel,
         maxlevel,
         leftBracketClosed,
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
  #' @param price Vector of length n of prices of order book updates during interval t.
  #' @param bid Vector of length n of bid prices during interval t.
  #' @param ask Vector of length n of ask prices during interval t.
  #' @param minlevel Scalar specifying the lower-bound level (in terms of the spread distance from best bid or ask)
  #' to include. For example, minlevel=0 and maxlevel=0 means that only messages at the best bid and ask will be included.
  #' @param maxlevel Scalar specifying the upper-bound level (in terms of the spread distance from best bid or ask)
  #' to include. For example, minlevel=1 and maxlevel=10 means that only messages that are priced between 1 and 10
  #' times the spread away from the best bid or ask will be included.
  #' @param leftBracketClosed Logical scalar specifying whether left bracket of price interval should be closed or not.
  #' For example, minlevel=0, maxlevel=1 and leftBracketClosed=0 includes messages that are priced strictly worse than
  #' the best bid or ask but at least as good as 1 times the half-spread away.
  #' @param string1 String specifying message type (e.g., "SUB","EXE","CANC")
  #' @param string2 String to further modify message type (e.g., "MPID","ANON",etc.)
  #'
  #' @usage getMessageType(condition,  direction,  sz,  price,  bid,  ask,  setLevel,  minlevel,  maxlevel,  leftBracketClosed,  string1,string2)
  #'
  #' @return Vector of length 9 containing amount of messages in terms of: (1) number of buy-side ("BUY.NUM); (2)
  #' number of sell-side ("SELL.NUM"); (3) total number ("ALL.NUM"); (4) share volume of buy-side ("BUY.SVOL"); (5)
  #' share volume of sell-side ("SELL.SVOL"); (6) total share volume ("ALL.SVOL"); (7) dollar volume of buy-side
  #' ("BUY.DVOL"); (8) dollar volume of sell-side ("SELL.DVOL"); (9) total dollar volum ("ALL.DVOL").
  
  #dollar volumes
  dvol <- price * sz
  #spread and midquote
  spr <- (ask - bid)
  halfspr <- 0.5 * spr
  mq <- 0.5 * (ask + bid)
  
  distance <- direction * (mq - price)
  minprice <- (minlevel * halfspr)
  maxprice <- (maxlevel * halfspr)
  
  if (leftBracketClosed == 1) {
    level <- (distance >= minprice & distance <= maxprice)
  }
  if (leftBracketClosed == 0) {
    level <- (distance > minprice & distance <= maxprice)
  }
  
  #modify condition such that only messages at certain price levels are included
  condition <- condition * level
  
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
      paste0(string1, ".", string2, ".", "BUY.NUM.", minlevel, ".", maxlevel),
      paste0(
        string1,
        ".",
        string2,
        ".",
        "SELL.NUM.",
        minlevel,
        ".",
        maxlevel
      ),
      paste0(
        string1,
        ".",
        string2,
        ".",
        "TOTAL.NUM.",
        minlevel,
        ".",
        maxlevel
      ),
      paste0(
        string1,
        ".",
        string2,
        ".",
        "BUY.SVOL.",
        minlevel,
        ".",
        maxlevel
      ),
      paste0(
        string1,
        ".",
        string2,
        ".",
        "SELL.SVOL.",
        minlevel,
        ".",
        maxlevel
      ),
      paste0(
        string1,
        ".",
        string2,
        ".",
        "TOTAL.SVOL.",
        minlevel,
        ".",
        maxlevel
      ),
      paste0(
        string1,
        ".",
        string2,
        ".",
        "BUY.DVOL.",
        minlevel,
        ".",
        maxlevel
      ),
      paste0(
        string1,
        ".",
        string2,
        ".",
        "SELL.DVOL.",
        minlevel,
        ".",
        maxlevel
      ),
      paste0(
        string1,
        ".",
        string2,
        ".",
        "TOTAL.DVOL.",
        minlevel,
        ".",
        maxlevel
      )
    )
  
  return(messages)
  
}
