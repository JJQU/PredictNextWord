

## SimpleGT.R
## Simple Good-Turing method
## adapted from S codes: gtfuncs.S (nrzest, rstest) and gtanal.S
## by Willian A. Gale
## https://faculty.cs.byu.edu/~ringger/CS479/papers/Gale-SimpleGoodTuring.pdf


simpleGT <- function(freqhist) {
  
  ## nrzest: Averaging transformation
  ## Replace nr by zr = nr/(0.5*(t - q))
  ## where q, r, t are successive indices of non-zero values
  ## in gtfuncs.S by William A. Gale
  ##
  nrzest <- function(r, nr) {
    d <- c(1, diff(r))
    dr <- c(0.5 * (d[-1] + d[-length(d)]), d[length(d)])
    return(nr/dr)
  }
  
  ## rstest: Linear Good-Turing estimate
  ## log(nr) = a + b * log(r)
  ## b = coef[2]
  ## rstest r(star)est = r *(1 + 1/r)^(b + 1)
  ## b < -1
  ## in gtfuncs.S by William A. Gale
  ##
  rstest <- function(r, coef) {
    return(r * (1 + 1/r)^(1 + coef[2]))
  }
  
  ## The following code comes from gtanal.S by William A. Gale
  
  ## Get the input xr and xnr    
  xm <- freqhist
  xr <- xm[, 1]
  xnr <- xm[, 2]
  xN <- sum(xr * xnr)
  
  ## make averaging transform
  xnrz <- nrzest(xr, xnr)
  
  ## get Linear Good-Turing estimate
  xf <- lsfit(log(xr), log(xnrz))
  xcoef <- xf$coef
  xrst <- rstest(xr, xcoef)
  xrstrel <- xrst/xr
  
  ## get Turing estimate
  xrtry <- xr == c(xr[-1]-1, 0)
  xrstarel <- rep(0, length(xr))
  xrstarel[xrtry] <- (xr[xrtry]+1) / xr[xrtry] * c(xnr[-1], 0)[xrtry] / xnr[xrtry]
  
  ## make switch from Turing to LGT estimates
  tursd <- rep(1, length(xr))
  for (i in 1:length(xr)) {
    tursd[i] <- (i+1) / xnr[i] * sqrt(xnr[i=1] * (1 + xnr[i+1] / xnr[i]))
  }
  xrstcmbrel <- rep(0, length(xr))
  useturing <- TRUE
  for (r in 1:length(xr)) {
    if (!useturing) {
      xrstcmbrel[r] <- xrstrel[r]
    } else if (abs(xrstrel - xrstarel)[r] * r / tursd[r] > 1.65) {
      xrstcmbrel[r] <- xrstarel[r]
    } else {
      useturing <- FALSE
      xrstcmbrel[r] <- xrstrel[r]
    }
  }
  
  ## renormalize the probabilities for observed objects
  sumpraw <- sum(xrstcmbrel * xr * xnr / xN)
  xrstcmbrel <- xrstcmbrel * (1 - xnr[1]/xN) / sumpraw
  
  ## output to file
  # cat(xN, sum(xnr), file="gtanal", sep=",")
  # cat(0, xnr[1]/xN, file="gtanal", sep=",", append=TRUE)
  # for (i in 1:length(xr)) {
  #     cat(xr[i], xr[i]*xrstcmbrel[i], file="gtanal", append=TRUE)
  # }
  
  ## output matrix (0, r0est) + (xr, xnrstarnormalized)
  rrstar <- cbind(c(0, xr), c(xnr[1]/xN, xr*xrstcmbrel))
  
  ## output data table by pairs = (r = freq, rstar = freqGT)
  ## keyed (ordered) by freq.
  rrstar <- data.table(rrstar)
  colnames(rrstar) <- c("freq", "freqGT")
  setkey(rrstar, freq)
  return(rrstar)
}

