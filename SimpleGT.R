

## SimpleGT.R
## Simple Good-Turing method
## adapted from S codes: gtfuncs.S (nrzest, rstest) and gtanal.S
## by Willian A. Gale
## https://faculty.cs.byu.edu/~ringger/CS479/papers/Gale-SimpleGoodTuring.pdf

## Simple Good-Turing method:
##   1. Use Turing estimate for small r
##   2. Use Linear Good-Turing estimate for large r
##   3. Switch to LGT when r(T) - r(LGT) > 1.65*sd
##   4. Once switched to LGT, keep using LGT
##
## simpleGT takes a data table of frequencies r and frequencies of frequencies 
## Nr and calculated the smoothed frequencies by the Simple Good Turing method.
## freqhist = imput, a data table of r (freq) and Nr (NFreq)
## rrstar = output, a data table of r (freq) and rstar (freqGT)
## where freq = original freq, freqGT = freq smoothed by simple GT method.
## the first row is the pair of 0 freq = (0, freqGT_at_0)
##
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

## Test cases
## from Gale-SimpleGoodTuring.pdf referenced above.

# prosody.r <- c(1,2,3,4,5,6,7,8,9,10,12,14,15,16,17,19,20,21,23,24,
#                25,26,27,28,31,32,33,34,36,41,43,45,46,47,50,71,84,101,105,121,
#                124,146,162,193,199,224,226,254,257,339,421,456,481,483,1140,
#                1256,1322,1530,2131,2395,6925,7846)
# prosody.nr <- c(120,40,24,13,15,5,11,2,2,1,3,2,1,1,3,1,3,2,3,3,
#                 3,2,2,1,2,2,1,2,2,3,1,3,1,1,1,1,1,1,1,1,
#                 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
# 
# prosody <- cbind(prosody.r, prosody.nr)
# prosodyRstar <- simpleGT(prosody)
# 
# chinesePlurals.r <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
#                       21,22,23,24,25,26,27,28,29,31,33,39,41,46,47,
#                       50,52,53,55,57,60,74,84,108,109,177,400,1918)
# 
# chinesePlurals.nr <- c(268,112,70,41,24,14,15,14,8,11,9,6,6,3,7,9,4,4,8,2,
#                        4,2,2,3,4,4,4,1,1,2,1,3,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1)
# 
# chinesePlurals <- cbind(chinesePlurals.r, chinesePlurals.nr)
# chinesePluralsRstar <- simpleGT(chinesePlurals)