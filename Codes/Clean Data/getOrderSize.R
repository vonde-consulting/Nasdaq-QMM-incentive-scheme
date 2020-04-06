function(price, sz, indexSub, string) {
  #' Calculate average order sizes of submissions.
  #'
  #' @description This function calculates the average order size of submissions during a pre-defined interval length  
  #' (e.g., 60-second intervals) using data on limit order book message updates. 
  #'
  #' Define n as the number of order book updates during interval t. Define s as the number of submissions during
  #' interval t.
  #' 
  #' Note that the function can be used to ca;lculate the average size of a subset of order submissions by changing 
  #' the values of "indexSub" (e.g., specifying the indexes of submissions from a particular market participant or 
  #' on a particular side of the book)
  #'
  #' @param price Vector of length n of the prices associated with order book updates during interval t. 
  #' @param sz Vector of length n of the sizes (in terms of number of shares) of order book updates during interval t.
  #' @param indexSub Vector of length s indexing which order book updates correspond to order submissions 
  #'  (vs. cancellations or executions) during interval t.
  #' @param string Optional 1d string to modify variable names (if, e.g., being used for a subset of orders)
  #' 
  #' @usage getOrderSize(price, sz, indexSub, string) 
  #' 
  #' @return Vector of length 2 containing: (1) average order submission size in terms of number of shares
  #'("ORSZ.SVOL"), and (2) average order submission size in terms of dollar volume ("ORSZ.DVOL")
  #'
  if (missing(string)) {
    string = ""
  }
  
  orszSVol <- sz[indexSub] #order size in terms of share volume
  orszDVol <-
    sz[indexSub] * price[indexSub] #order size in terms of dollar volume
  
  orsz <-
    c(mean(orszSVol, na.rm = T), mean(orszDVol, na.rm = T))
  orsz <- replace(orsz, is.nan(orsz), NA)
  
  names(orsz) <-
    c(paste0("ORSZ.SVOL.", string), paste0("ORSZ.DVOL.", string))
  
  return(orsz)
  
}
