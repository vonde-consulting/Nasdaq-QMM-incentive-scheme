function(price, sz, indexSub, string) {
  
  
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