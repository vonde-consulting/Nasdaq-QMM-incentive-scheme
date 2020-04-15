function(direction,
         price,
         mid,
         ssquote,
         indexSub,
         string) {
         
  #' Calculate average aggressivenss of submissions.
  #'
  #' @description This function calculates the average aggressiveness (i.e., distance to prevailing midquote) of 
  #' submissions during a pre-defined interval length (e.g., 60-second intervals) using data on limit order book 
  #' message updates. 
  #'
  #' Define n as the number of order book updates during interval t. Define s as the number of submissions during
  #' interval t.
  #' 
  #' Note that the function can be used to ca;lculate the average aggressiveness of a subset of order submissions 
  #' by changing the values of "indexSub" (e.g., specifying the indexes of submissions from a particular market 
  #' participant or on a particular side of the book)
  #'
  #' @param direction Vector of length n of trade directions associated with order book updates (1 for buy, -1 for sell).
  #' @param price Vector of length n of the prices associated with order book updates during interval t. 
  #' @param mid Vector of length n of midquotes (0.5*(askprice+bidprice)) during interval t.
  #' @param ssquote Vector of length n of same-side quotes. For buy submissions, this is the bid price; for sell 
  #' submissioons, this is the ask price.
  #' @param indexSub Vector of length s indexing which order book updates correspond to order submissions 
  #'  (vs. cancellations or executions) during interval t.
  #' @param string Optional 1d string to modify variable names (if, e.g., being used for a subset of orders)
  #' 
  #' @usage getAggressiveness(direction, price, mid, ssquote, indexSub, string) 
  #' 
  #' @return Vector of length 2 containing: (1) average aggressiveness in relative terms
  #'("AGGR.ABS"), and (2) average aggressiveness relative to midquote ("AGGR.REL.")

  if (missing(string)) {
    string = ""
  }
    
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
