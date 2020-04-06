function(n,
         delta,
         K,
         ss,
         intervals,
         q,
         ssMid,
         string1) {
  #' Calculate realized volatility.
  #'
  #' @description This function calculates realized volatility as the sum of squared midquote returns over
  #' n equally-spaced sub-intervals within a given w-second interval, calculated using sub-sampling over K number of
  #' ss-second grids.
  #'
  #' For example, for w=60, n=5, delta=12, ss=1, K=10: divides a w=60-second interval into n=5 intervals each of
  #' length delta=12-seconds and sums squared midquote returns over these intervals to calculate the first RV subsample.
  #' Then, shift the grid (backward or forward) by ss=1 second. Do this a total of K=10 times.
  #' For example, the first subsample takes midquote returns at timestamps c(34752,34764,34776,34788,34800).
  #' The second subsample takes midquote returns at timestamps c(34751,34763,34775,34787,34799).
  #' Realized volatility is then calculated as the average RV across these K=10 subsamples.
  #'
  #' Based on Hautsch, N. (2011). Econometrics of financial high-frequency data. Springer.
  #'
  #' Define T the total number of w-second intervals.
  #'
  #' @param n Scalar: number of grids.
  #' @param delta Scalar: length of grids (in seconds). Should be equal to w/n.
  #' @param K Scalar: number of subsamples.
  #' @param ss Scalar: length of subsample (in seconds).
  #' @param intervals Vector of length T of timestamps of cutoffs between intervals (i.e., first timestamp of each
  #' interval).
  #' @param q Scalar. Index of the timestamp corresponding to the beginning of interval t within the above vector
  #' "intervals".
  #' @param ssMid dataframe containing two columns: (1) "time" containing timestamps of each ss-second interval;
  #' (2) "mid" containing the closest log midquote to the corresponding timestamp in "time".
  #' @param string1 1d string to specify whether rolling average across subsamples should be calculated moving
  #' backwards in time ("PRE) or forwards in time ("POST")
  #'
  #' @usage getRealizedVolatility(n,  delta,  K,  ss,  intervals,  q,  ssMid,  string1)
  #'
  #' @return Scalar containing realized volatility.
  
  if (!string1 %in% c("PRE", "POST")) {
    stop("String must be either PRE or POST")
  }
  
  if (string1 == "PRE") {
    RV_full <- matrix(NA, 1, K)
    
    for (xx in 1:K) {
      intervaltemp <-
        seq(intervals[q] - (xx - 1) * ss, intervals[(q + 1)] - (xx - 1) * ss, delta)
      begMid <-
        match(intervaltemp[1:n], ssMid$time) #midquotes at beginning of ss-second interval
      endMid <-
        match(intervaltemp[2:(n + 1)], ssMid$time) #midquotes at beginning of next ss-second interval
      
      RV_full[xx] <-
        sum((ssMid$mid[endMid] - ssMid$mid[begMid]) ^ 2, na.rm = T)
      
    }
    
    vol1 <-
      mean(RV_full, na.rm = T) #take average across different subsamples
    names(vol1) <- "VOL.RV.PRE"
    
    return(vol1)
    
  }
  
  if (string1 == "POST") {
    RV_full <- matrix(NA, 1, K)
    
    for (xx in 1:K) {
      intervaltemp <-
        seq(intervals[q] + (xx - 1) * ss, intervals[(q + 1)] + (xx - 1) * ss, delta)
      begMid <-
        match(intervaltemp[1:n], ssMid$time) #midquotes at beginning of ss-second interval
      endMid <-
        match(intervaltemp[2:(n + 1)], ssMid$time) #midquotes at beginning of next ss-second interval
      
      RV_full[xx] <-
        sum((ssMid$mid[endMid] - ssMid$mid[begMid]) ^ 2, na.rm = T)
      
    }
    
    vol1 <-
      mean(RV_full, na.rm = T) #take average across different subsamples
    names(vol1) <- "VOL.RV.POST"
    
    return(vol1)
    
  }
  
}
