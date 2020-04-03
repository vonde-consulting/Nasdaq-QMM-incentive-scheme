function(direction,
         price,
         mid,
         ssquote,
         indexSub,
         string) {
  
  midtemp <- mid[(indexSub - 1)] #midquote immediately before submission
  aggrAbs <-
    (price[indexSub] - ssquote[(indexSub - 1)]) * direction[indexSub] #aggressiveness in absolute terms
  aggrRel <- aggrAbs / midtemp #aggressiveness relative to midquote
  
  aggr <-
    c(mean(aggrAbs, na.rm = T), mean(aggrRel, na.rm = T))
  aggr <- replace(aggr, is.nan(aggr), NA)
  
  names(aggr) <-
    c(paste0("AGGR.ABS.", string), paste0("AGGR.REL.", string))
  
  return(aggr)
  
}