function(indexExe, lags, price, direction, sz) {
  #vector autoregressive (VAR) system of differences in log prices ($r_t=\Delta p_t$) and trade-related characteristics $x_t$:
  
  y <- diff(log(price[indexExe]))
  
  #The column vector of trade variables $\mathbf{x}_t$ includes:
  #(1) a sign indicator reflecting the direction of the trade,
  #(2) signed trading volume, and
  #(3) the signed square root of trading volume.
  
  x1 <-
    direction[indexExe] * (-1)
  x1 <-
    x1[-1] #note that direction needs to be multiplied by -1, as it refers to sign on submission (not execution)
  x2 <- sz[indexExe]
  x2 <- x2[-1] * x1
  x3 <- sz[indexExe]
  x3 <- sqrt(x3[-1]) * x1
  
  ##step one: get residuals from VAR
  model <- cbind(y, x1, x2, x3)
  dimx <- ncol(model)
  test <- try(VAR(model,
                  p = lags,
                  output = F,
                  include.mean = F), silent = T)
  if (class(test) == "try-error") {
    return(c(NA, NA, NA))
    break
  } #break if estimation is not possible (e.g., singular matrix)
  
  res <- residuals(test)
  rm(test)
  
  ##step two: estimate VMA parameters using residuals from VAR
  T <- nrow(res)
  ist <- lags + 1
  
  xmtx <- NULL
  for (j in 0:lags) {
    xmtx <- cbind(xmtx, res[(ist - j):(T - j), ])
  }
  y <- y[-(1:(2 * lags))]
  label <- c("V1.L", "V21.L", "V22.L", "V23.L")
  colnames(xmtx) <-
    c(
      paste0(label, 0),
      paste0(label, 1),
      paste0(label, 2),
      paste0(label, 3),
      paste0(label, 4),
      paste0(label, 5)
    )
  
  fit <- try(lm(y ~ xmtx - 1), silent = T)
  #"-1" denotes no constant
  if (class(fit) == "try-error") {
    return(c(NA, NA, NA)) #skip if model estimation is not possible
    break
  }
  
  #step 3: get reverse cumulative sums of coefficients
  
  coefs <- fit$coefficients
  coefs <-
    coefs[(dimx + 1):length(coefs)] #get rid of coefficients on lag 0
  
  a <- rev(coefs[seq(1, dimx * lags, dimx)]) #coefficients on v1
  astar <- (-rev(cumsum(a)))
  
  b1 <- rev(coefs[seq(2, dimx * lags, dimx)]) #coefficients on v21
  b1star <- (-rev(cumsum(b1)))
  
  b2 <- rev(coefs[seq(3, dimx * lags, dimx)]) #coefficients on v22
  b2star <- (-rev(cumsum(b2)))
  
  b3 <- rev(coefs[seq(4, dimx * lags, dimx)]) #coefficients on v23
  b3star <- (-rev(cumsum(b3)))
  
  #step 4: multiply by errors terms
  alpha <- c(astar, b1star, b2star, b3star)
  cols <-
    c(
      paste0("V1.L", 1:5),
      paste0("V21.L", 1:5),
      paste0("V22.L", 1:5),
      paste0("V21.L", 1:5)
    )
  xmtx <- xmtx[, match(cols, colnames(xmtx))]
  
  st <- matrix(0, nrow(xmtx), 1)
  
  for (ii in 1:length(alpha)) {
    st <- st + (alpha[ii] * xmtx[, ii])
  }
  
  stMean <- mean(st, na.rm = T)
  stSD <- sd(st, na.rm = T)
  stMax <- max(st, na.rm = T)
  
  hasbrouck <-
    c(stMean, stSD, stMax)
  names(hasbrouck) <- c("PRERR.MEAN", "PRERR.SD", "PRERR.MAX")
  
  return(hasbrouck)
  
  
}