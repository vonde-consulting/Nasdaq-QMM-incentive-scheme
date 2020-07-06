function(condition,
         direction,
         sz,
         price,
         bid,
         ask,
         baspr_st,
         levelSelect,
         x,
         string1,
         string2) {
  #' Calculate the amount of order book update messages.
  #'
  #' @description This function calculates the amount of limit order book updates (in terms of (1) number of messages,
  #' (2) share volume of messages, and (3) dollar volumes of messages) during a particular interval t.
  #' Limit order book update types can include submissions, cancellations, and executions. Code can specify to only
  #' include messages at certain levels of the order book, defined according to a standardized distance 
  #' away from the best bid or ask (bbo).
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
  #' @param baspr_st Scalar specifying the variable used as the standard measure of distance away from the bbo.
  #' @param levelSelect Logical vector of length 4 specifying the book levels to include. Possibilities are: (1)
  #' Price improving; (2) At the BBO, (3) Worse than BBO but less than x times baspr_st away from the bbo; (4) 
  #' greater than x times baspr_st away from the bbo
  #' @param x Scalar specifying the cutoff between levels 2 and 2+ of the book
  #' @param string1 String specifying message type (e.g., "SUB","EXE","CANC")
  #' @param string2 String to further modify message type (e.g., "MPID","ANON",etc.)
  #'
  #' @usage getMessageType(condition, direction, sz,  price,  bid,  ask,  baspr_st,  levelSelect,  x,  string1,  string2)
  #'
  #' @return Vector of length 9 containing amount of messages in terms of: (1) number of buy-side ("BUY.NUM); (2)
  #' number of sell-side ("SELL.NUM"); (3) total number ("ALL.NUM"); (4) share volume of buy-side ("BUY.SVOL"); (5)
  #' share volume of sell-side ("SELL.SVOL"); (6) total share volume ("ALL.SVOL"); (7) dollar volume of buy-side
  #' ("BUY.DVOL"); (8) dollar volume of sell-side ("SELL.DVOL"); (9) total dollar volum ("ALL.DVOL").
  
  #dollar volumes
  dvol <- price * sz
  #best bid and ask
  bbo<-bid*(direction==1)+ask*(direction==-1)
  #distance of order away from bbo
  distance <- direction * (bbo-price)
  
  #orders that are better than the best bid or ask (i.e., distance<0)
  level0<-distance<0

  #orders that are exactly at the best bid or ask, (i.e., distance==0)
  level1<-distance==0
  
  #orders that are priced between 0 and 3 times the avg spread away from midquote
  level2<-(distance>0)&(distance<=(x*baspr_st))
  
  #orders that are priced greater than 3 times the avg spread away from midquote
  level3<-distance>(x*baspr_st)

  #sanity check -- sum of all level* variables should lead to vector of ones
  check<-level0+level1+level2+level3 
  if(sum(check!=1,na.rm=T)!=0){stop("Levels not properly assigned")}
  
  #select levels
  level<-level0*levelSelect[1]+level1*levelSelect[2]+level2*levelSelect[3]+level3*levelSelect[4]

  #modify condition such that only messages at certain price levels are included
  condition <- condition * level
  
  #In Terms of Numbers of Messages
  totalNum <- sum(condition,na.rm=T)
  buyNum <- sum(condition & direction == 1,na.rm=T)
  sellNum <- sum(condition & direction == -1,na.rm=T)
  check<-round(sum(totalNum-(buyNum+sellNum),na.rm=T),digits=5)
  if (check!=0) {
    stop("Buy and Sell numbers don't add up")
  }
  rm(check)
  
  #In Terms of Share Volumes of Messages
  totalSVol <- sum(condition * sz,na.rm=T)
  buySVol <- sum((condition & direction == 1) * sz,na.rm=T)
  sellSVol <- sum((condition & direction == -1) * sz,na.rm=T)
  check<-round(sum(totalSVol-(buySVol+sellSVol),na.rm=T),digits=5)
  if (check!=0) {
    stop("Buy and Sell share volumes don't add up")
  }
  rm(check)
  
  #In Terms of Dollar Volumes of Messages
  totalDVol <- sum(condition * dvol,na.rm=T)
  buyDVol <- sum((condition & direction == 1) * dvol,na.rm=T)
  sellDVol <- sum((condition & direction == -1) * dvol,na.rm=T)
  check<-round(sum(totalDVol-(buyDVol+sellDVol),na.rm=T),digits=5)
  if (check!=0) {
    stop("Buy and Sell dollar volumes don't add up")
  }
  rm(check)
  
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
      paste0(string1, ".", string2, ".", "BUY.NUM.", paste0(levelSelect, collapse="")),
      paste0(
        string1,
        ".",
        string2,
        ".",
        "SELL.NUM.",
        paste0(levelSelect, collapse="")
      ),
      paste0(
        string1,
        ".",
        string2,
        ".",
        "TOTAL.NUM.",
        paste0(levelSelect, collapse="")
      ),
      paste0(
        string1,
        ".",
        string2,
        ".",
        "BUY.SVOL.",
        paste0(levelSelect, collapse="")
      ),
      paste0(
        string1,
        ".",
        string2,
        ".",
        "SELL.SVOL.",
        paste0(levelSelect, collapse="")
      ),
      paste0(
        string1,
        ".",
        string2,
        ".",
        "TOTAL.SVOL.",
        paste0(levelSelect, collapse="")
      ),
      paste0(
        string1,
        ".",
        string2,
        ".",
        "BUY.DVOL.",
        paste0(levelSelect, collapse="")
      ),
      paste0(
        string1,
        ".",
        string2,
        ".",
        "SELL.DVOL.",
        paste0(levelSelect, collapse="")
      ),
      paste0(
        string1,
        ".",
        string2,
        ".",
        "TOTAL.DVOL.",
        paste0(levelSelect, collapse="")
      )
    )
  
  return(messages)
  
}
