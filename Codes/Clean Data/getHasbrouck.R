function(indexExe, lags, price, direction, sz) {
  #' Calculate Hasbrouck (1993) pricing errors
  #'
  #' @description Calculates the Hasbrouck pricing error during an interval t as either the mean, standard deviation,
  #' or maximum realization of the measure during the interval t.
  #' 
  #' For more details, see .
  #'
  #' Define n as the number of observed order book updates during interval t. Define by s the total number of 
  #' executions during interval t.
  #' 
  #'
  #' @param indexSub Vector of length s indexing which order book updates correspond to order executions
  #'  (vs. cancellations or submissions) during interval t.
  #' @param lags Scalar: Number of lags to include in VAR estimation.
  #' @param price Vector of length n of the prices associated with order book updates during interval t. 
  #' @param direction Vector of length n of trade directions associated with order book updates (1 for buy, -1 for sell).
  #' @param sz Vector of length n of the sizes (in terms of number of shares) of order book updates during interval t.
  #' 
  #' @usage getHasbrouck(indexExe, lags, price, direction, sz) 
  #' 
  #' @return Vector of length 3 containing: (1) mean Hasbrouck measure over interval t; (2) standard deviation
  #' across Hasbrouck measures during interval t; (3) maximum realization of Hasbrouck measure during interval t.
  
  
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
  }
  
  #step 3: get reverse cumulative sums of coefficients
  
  coefs <- fit$coefficients
  
  if (sum(is.na(coefs))>0){return(c(NA,NA,NA))} #break if some of the coefficients are NA (e.g., due to mullticollinearity)
  
  coefs <-
    coefs[(dimx + 1):length(coefs)] #get rid of coefficients on lag 0
  
  astar <- rev(coefs[seq(1, dimx * lags, dimx)]) #coefficients on v1
  alpha <- (-rev(cumsum(astar)))
  
  bstar1 <- rev(coefs[seq(2, dimx * lags, dimx)]) #coefficients on v21
  beta1 <- (-rev(cumsum(bstar1)))
  
  bstar2 <- rev(coefs[seq(3, dimx * lags, dimx)]) #coefficients on v22
  beta2 <- (-rev(cumsum(bstar2)))
  
  bstar3 <- rev(coefs[seq(4, dimx * lags, dimx)]) #coefficients on v23
  beta3 <- (-rev(cumsum(bstar3)))
  
  #step 4: multiply by errors terms
  mmult <- c(alpha, beta1, beta2, beta3)
  cols <-
    c(
      paste0("V1.L", 1:5),
      paste0("V21.L", 1:5),
      paste0("V22.L", 1:5),
      paste0("V21.L", 1:5)
    )
  xmtx <- xmtx[, match(cols, colnames(xmtx))]
  
  st <- matrix(0, nrow(xmtx), 1)
  
  for (ii in 1:length(mmult)) {
    st <- st + (mmult[ii] * xmtx[, ii])
  }
  
  stMean <- mean(st, na.rm = T)
  stSD <- sd(st, na.rm = T)
  stMax <- max(st, na.rm = T)
  
  hasbrouck <-
    c(stMean, stSD, stMax)
  names(hasbrouck) <- c("PRERR.MEAN", "PRERR.SD", "PRERR.MAX")
  
  return(hasbrouck)
  
  
}
