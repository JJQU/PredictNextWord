
preprocessData <- function(rawCorpus) {
  cleanCorpus <- tm_map(rawCorpus, PlainTextDocument)
  cleanCorpus <- tm_map(cleanCorpus, content_transformer(replace_contraction))
  cleanCorpus <- tm_map(cleanCorpus, content_transformer(tolower))
  #cleanCorpus <- tm_map(cleanCorpus, removeWords, badwords)
  cleanCorpus <- tm_map(cleanCorpus, removeNumbers)
  cleanCorpus <- tm_map(cleanCorpus, removePunctuation)
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
  cleanCorpus <- tm_map(cleanCorpus, toSpace, "[^a-zA-Z]")
  cleanCorpus <- tm_map(cleanCorpus, removeWords, stopwords(kind = "en"))
  cleanCorpus <- tm_map(cleanCorpus, stripWhitespace)
  
  return(cleanCorpus)
}




cleanNgram <- function(ngramDf, minFreq) {
  if (!require(stringr)) {
    stop("Library stringr is missing.")
  }
  
  colnames(ngramDf) <- c("ngram", "freq", "prob")
  ngramDt <- data.table(ngramDf)
  
  ngramDt <- ngramDt[freq >= minFreq]
  
  freqNf <- data.table(table(ngramDt[, "freq"]))
  colnames(freqNf) <- c("freq", "NFreq")
  freqNf <- sapply(freqNf, as.numeric)
  
  freqSimpleGT <- simpleGT(freqNf)
  
  setkey(ngramDt, freq)
  ngramDt <- merge(ngramDt, freqSimpleGT)
  
  
  pZero <- freqSimpleGT[1, 2]/sum(c(1, freqNf[, "NFreq"]) * freqSimpleGT[, 2])
  
  
  ngramAndPzero <- list(ngramDt=ngramDt, pZero=pZero)
  return(ngramAndPzero)
}



split1Gram <- function(ugramDt) {
  if (!require(stringr)) {
    stop("Library stringr is missing.")
  }
  
  ugramDt[, word1 := str_trim(ngram)]
  setkey(ugramDt, word1)
  
  ugramDt <- ugramDt[, freq := sum(freq), by=c("word1")]
  
  ugramTotalFreq <- sum(ugramDt$freq)
  ugramDt[, prob := freq/ugramTotalFreq]
  
  ugramDt <- ugramDt[, freqGT := sum(freqGT), by=c("word1")]
  ugramTotalFreqGT <- sum(ugramDt$freqGT)
  ugramDt[, probGT := freqGT/ugramTotalFreqGT]
  
  setkey(ugramDt, word1)
  
  setcolorder(ugramDt, c("ngram", "word1",
                         "freq", "prob", "freqGT", "probGT"))
  return(ugramDt)
}


split2Gram <- function(bgramDt, vocab) {
  if (!require(stringr)) {
    stop("Library stringr is missing.")
  }
  bgramSplits <- str_split(bgramDt$ngram, boundary("word"))
  bgramDt[, word1 := sapply(bgramSplits, function(m) m[1])]
  bgramDt[, word2 := sapply(bgramSplits, function(m) m[2])]
  
  bgramDt[!(word1 %in% vocab), word1 := "<UNK>"]
  bgramDt[!(word2 %in% vocab), word2 := "<UNK>"]
  
  bgramDt[, count_w1_w2 := sum(freq), by=c("word1", "word2")]
  bgramDt[, count_w1 := sum(freq), by=c("word1")]
  
  bgramDt[, prob := count_w1_w2/count_w1]
  bgramDt[, count_w1_w2_GT := sum(freqGT), by=c("word1", "word2")]
  bgramDt[, count_w1_GT := sum(freqGT), by=c("word1")]
  bgramDt[, probGT := count_w1_w2_GT/count_w1_GT]
  bgramDt[, c("count_w1_w2", "count_w1", "count_w1_w2_GT", "count_w1_GT") := NULL]
  
  setkey(bgramDt, word1, word2)
  setcolorder(bgramDt, c("ngram", "word1", "word2", 
                         "freq", "prob", "freqGT", "probGT"))
  return(bgramDt)
}



split3Gram <- function(tgramDt, vocab) {
  if (!require(stringr)) {
    stop("Library stringr is missing.")
  }
  
  tgramSplits <- str_split(tgramDt$ngram, boundary("word"))
  tgramDt[, word1 := sapply(tgramSplits, function(m) m[1])]
  tgramDt[, word2 := sapply(tgramSplits, function(m) m[2])]
  tgramDt[, word3 := sapply(tgramSplits, function(m) m[3])]
  
  tgramDt[!(word1 %in% vocab), word1 := "<UNK>"]
  tgramDt[!(word2 %in% vocab), word2 := "<UNK>"]
  tgramDt[!(word3 %in% vocab), word3 := "<UNK>"]
  
  tgramDt[, count_w1_w2_w3 := sum(freq), by=c("word1", "word2", "word3")]
  tgramDt[, count_w1_w2 := sum(freq), by=c("word1", "word2")]
  
  tgramDt[, prob := count_w1_w2_w3/count_w1_w2]
  
  tgramDt[, count_w1_w2_w3_GT := sum(freqGT), by=c("word1", "word2", "word3")]
  tgramDt[, count_w1_w2_GT := sum(freqGT), by=c("word1", "word2")]
  tgramDt[, probGT := count_w1_w2_w3_GT/count_w1_w2_GT]
  
  tgramDt[, c("count_w1_w2_w3", "count_w1_w2", 
              "count_w1_w2_w3_GT", "count_w1_w2_GT") := NULL]
  
  setkey(tgramDt, word1, word2, word3)
  setcolorder(tgramDt, c("ngram", "word1", "word2", "word3", 
                         "freq", "prob", "freqGT", "probGT"))
  return(tgramDt)
}


unigram <- function(ugramDf, minFreq) {
  ugramPzero <- cleanNgram(ugramDf, minFreq)
  ugramPzero$ngramDt <- split1Gram(ugramPzero$ngramDt)
  return(ugramPzero)
}

bigram <- function(bgramDf, minFreq, vocab) {
  bgramPzero <- cleanNgram(bgramDf, minFreq)
  bgramPzero$ngramDt <- split2Gram(bgramPzero$ngramDt, vocab)
  return(bgramPzero)
}

trigram <- function(tgramDf, minFreq, vocab) {
  tgramPzero <- cleanNgram(tgramDf, minFreq)
  tgramPzero$ngramDt <- split3Gram(tgramPzero$ngramDt, vocab)
  return(tgramPzero)
}


biNextWords <- function(bgramDt, newWord1) {
  bNextWords <- subset(bgramDt, word1==newWord1)
  bNextWords <- bNextWords[word2 != "<UNK>", ]
  return(bNextWords)
}

triNextWords <- function(tgramDt, newWord1, newWord2) {
  tNextWords <- subset(tgramDt, word1==newWord1 & word2==newWord2)
  tNextWords <- tNextWords[word3 != "<UNK>", ]
  return(tNextWords)
}


bimodel <- function(lastWord, coef, ugramPzero, bgramPzero, tgramPzero) {
  newWord1 <- lastWord
  
  if ("<UNK>" %in% newWord1) return(data.table())
  
  bNextWords <- biNextWords(bgramPzero$ngramDt, newWord1)
  if (dim(bNextWords)[1] > 0) {
    ## Get probabilities of unigrams = nextWord
    setkey(bNextWords, word2)
    setkey(ugramPzero$ngramDt, word1)
    bNextWords <- bNextWords[ugramPzero$ngramDt, nomatch=0L]
    names(bNextWords) <- gsub("i.", "u", names(bNextWords))
    
    ## Add probability of trigram at zero frequency
    bNextWords[, tprobGT := tgramPzero$pZero]
    
    ## Calculate trigram probabilities
    bNextWords[, predictProb := coef[1]*uprobGT + coef[2]*probGT 
               + coef[3]*tprobGT]
    
    ## Sort predicted probabilities in decreasing order
    setorder(bNextWords, -predictProb)
    predictions <- bNextWords
  } else {
    ## Get the most frequent word if trigrams and bigrams not found
    uNextWords <- ugramPzero$ngramDt[order(-probGT)][1]
    
    ## Add probabilities of bigrams and trigrams at zero frequency
    uNextWords[, bprobGT := bgramPzero$pZero]
    uNextWords[, tprobGT := tgramPzero$pZero]
    
    ## Calculate trigram probabilities
    uNextWords[, predictProb := coef[1]*probGT + coef[2]*bprobGT 
               + coef[3]*tprobGT]
    predictions <- uNextWords
  }
  
  return(predictions)
}



trimodel <- function(lastWords, coef, ugramPzero, bgramPzero, tgramPzero, nKeep) {
  newWord1 = lastWords[[1]]
  newWord2 = lastWords[[2]]
  
  if ("<UNK>" %in% newWord2) return(data.table())
  
  ## Get trigrams of two new words
  tNextWords <- triNextWords(tgramPzero$ngramDt, newWord1, newWord2)
  if (nrow(tNextWords) > 0) {
    ## Get probabilities of bigrams = newWord2-nextWord
    setkey(tNextWords, word2, word3)
    setkey(bgramPzero$ngramDt, word1, word2)
    tNextWords <- tNextWords[bgramPzero$ngramDt, nomatch=0L]
    names(tNextWords) <- gsub("i.", "b", names(tNextWords))
    
    ## Get probabilities of unigrams = nextWord
    setkey(tNextWords, word3)
    setkey(ugramPzero$ngramDt, word1)
    tNextWords <- tNextWords[ugramPzero$ngramDt, nomatch=0L]
    names(tNextWords) <- gsub("i.", "u", names(tNextWords))
    
    
    tNextWords[, predictProb := coef[1]*uprobGT + coef[2]*bprobGT 
               + coef[3]*probGT]
    
    ## Sort predicted probabilities in decreasing order
    setorder(tNextWords, -predictProb)
    predictions <- tNextWords
  } else {
    ## Get bigrams if trigrams not found
    predictions <- bimodel(newWord2, coef, ugramPzero, bgramPzero, tgramPzero)
  }
  
  if (nrow(predictions) > nKeep) {
    predictions <- predictions[1:nKeep, ]
  }
  return(predictions)
}






predict2 <- function(inputStr) {
  if (inputStr == "") {
    return("<UNK>")
  }
  
  inputCorpus <- SimpleCorpus(VectorSource(inputStr))
  inputCorpus <- preprocessData(inputCorpus)
  
  cleanStr <- concatenate(inputCorpus$content)[[1]]
  if (cleanStr == " ") {
    return("<UNK>")
  }
  
  inputWords <- str_split(cleanStr, boundary("word"))[[1]]
  
  if (length(inputWords) > 1) {
    testWords <- tail(inputWords, 2)
  } else if (length(inputWords) > 0) {
    testWords <- c("<UNK>", tail(inputWords, 1))
  } else {
    return("<UNK>")
  }
  
  predictions <- trimodel(testWords, coef2, 
                          ugramPzero.f8, 
                          bgramPzero.f2.f8, 
                          tgramPzero.f2.f8, 1)
  
  cnames <- colnames(predictions)
  if ("word3" %in% cnames) {
    nextWords <- predictions$word3
  } else if ("word2" %in% cnames) {
    nextWords <- predictions$word2
  } else {
    nextWords <- predictions$word1
  }
  return(nextWords)
}
