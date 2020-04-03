function(n,
         delta,
         K,
         ss,
         intervals,
         q,
         time,
         mid,
         string1) {
  
  
  mid <- log(mid) #take log midquotes
  
  if (!string1 %in% c("PRE", "POST")) {
    stop("String must be either PRE or POST")
  }
  
  if (string1 == "PRE") {
    RV_full <- matrix(NA, 1, K)
    
    for (xx in 1:K) {
      intervaltemp <-
        seq(intervals[q - n] - (xx - 1) * ss, intervals[q + 1] - (xx - 1) * ss, delta) #split this interval into n intervals
      
      RV <- matrix(NA, 1, n)
      
      for (ii in 1:(length(intervaltemp) - 1)) {
        iia <- min(which(time > intervaltemp[ii]))
        iib <- max(which(time <= intervaltemp[ii + 1]))
        
        if (is.infinite(iia) | is.infinite(iib)) {
          next
        }
        
        RV[ii] <- (mid[iib] - mid[iia]) ^ 2
        
      }
      
      RV_full[xx] <- sum(RV, na.rm = T)
      
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
        seq(intervals[q] + (xx - 1) * ss, intervals[q + n] + (xx - 1) * ss, delta) #split this interval into n intervals
      
      RV <- matrix(NA, 1, n)
      
      for (ii in 1:(length(intervaltemp) - 1)) {
        iia <- min(which(time > intervaltemp[ii]))
        iib <- max(which(time <= intervaltemp[ii + 1]))
        
        if (is.infinite(iia) | is.infinite(iib)) {
          next
        }
        
        RV[ii] <- (mid[iib] - mid[iia]) ^ 2
        
      }
      
      RV_full[xx] <- sum(RV, na.rm = T)
      
    }
    
    vol1 <-
      mean(RV_full, na.rm = T) #take average across different subsamples
    names(vol1) <- "VOL.RV.POST"
    
    return(vol1)
    
  }
  
}