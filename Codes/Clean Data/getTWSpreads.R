function(endSpread, spread, mid, time, intervals, q) {
  
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