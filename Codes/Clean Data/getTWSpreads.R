function(endSpread, spread, mid, time, intervals, q) {
  #' Calculate time-weighted bid-ask spreads.
  #'
  #' @description This function calculates time-weighted bid-ask spreads (both absolute spreads and relative to midquote) aggregated
  #' across a pre-defined interval length  (e.g., 60-second intervals). 
  #'
  #' Define n as the number of observed order book updates during interval t. Define T the total number of intervals.
  #'
  #' @param endSpread Vector of length 3 containing the information about the limit order book at the 
  #' end of interval t-1; information is in the following order: c(bid-ask spread (askprice-bidprice), 
  #' midquote (0.5*(askprice+bidprice)),timestamp (in # of seconds past midnight))
  #' (needed in order to determine the state of the limit order book at the beginning of interval t).
  #' @param spread Vector of length n of spreads (askprice-bidprice) during interval t.
  #' @param mid Vector of length n of midquotes (0.5*(askprice+bidprice)) during interval t.
  #' @param time Vector of length n of timestamps (in # of seconds past midnight) of order book updates during interval t.
  #' @param intervals Vector of length T of timestamps of cutoffs between intervals (i.e., first timestamp of each
  #' interval).
  #' @param q Scalar. Index of the timestamp corresponding to the beginning of interval t within the above vector 
  #' "intervals".
  #' 
  #' @usage getTWSpreads(endSpread, spread, mid, time, intervals, q) 
  #' 
  #' @return Vector of length 2 containing: (1) absoute time-weighted bid-ask spread ("ABS.SPR.TW"), and (2) 
  #' relative (to the midquote) time-weighted bid-ask spread ("REL.SPR.TW") during interval t
  
  quotesttemp <- as.data.frame(rbind(endSpread, cbind(spread, mid, time)))
  quotesttemp$time[1] <- intervals[q]
  #get total trading time and length of the interval
  mintime <- min(quotesttemp$time)
  maxtime <- max(quotesttemp$time)
  totaltime <- intervals[(q + 1)] - mintime
  alive <-
    diff(c(quotesttemp$time, intervals[(q + 1)])) #number of seconds that quote is in the books
  if (abs(sum(alive) - totaltime) > 1e-10) {
    stop("Mismatched times")
  }
  weights <- alive / totaltime
  if (abs(sum(weights) - 1) > 1e-10) {
    stop("Sum of weights not equal to one")
  }
  #quoted spreads
  basprAbs <- sum((quotesttemp$spread) * weights, na.rm = T)
  #relative quoted spreads
  basprRel <-
    sum(((quotesttemp$spread) / quotesttemp$mid) * weights, na.rm = T)
  
  baspr <-
    c(basprAbs, basprRel)
  names(baspr) <- c("ABS.SPR.TW", "REL.SPR.TW")
  
  return(baspr)
  
}
