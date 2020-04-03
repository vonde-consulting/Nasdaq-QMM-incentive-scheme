function(condition,
         direction,
         sz,
         dvol,
         string1,
         string2) {
  
  
  
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