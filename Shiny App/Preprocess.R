
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
