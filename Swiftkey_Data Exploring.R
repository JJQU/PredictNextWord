# 1. Data Understanding
# The folder contains 4 sub-folders. Each contains four datasets for one language. Seperately they are in English, Russian, German, and Finnish.
# We use en_US datasets for this project. This dataset contains 3 files, en_US.twitter, en_US.blogs, and en_US.news.
# The whole size for this dataset is 556MB. The size for each file is, 159MB for twitter, 196MB for news, and 200MB for blogs, respectively.


library(tm)
library(wordcloud)

getwd()
setwd("C:\\Users\\JJQ\\Documents")

con1 <- file("./data/final/en_US/en_US.twitter.txt", "r")
us_twitter <- readLines(con1)
close(con1) 

str(us_twitter)
lt <- length(us_twitter)
lt

con1 <- file("./data/final/en_US/en_US.blogs.txt", "r")
us_blogs <- readLines(con1)
close(con1)

lb <- length(us_blogs)
lb

con1 <- file("./data/final/en_US/en_US.news.txt", "r")
us_news <- readLines(con1)
close(con1)

ln <- length(us_news)
ln

l <- c(lt,lb,ln)
l
sum(l)

# Overall there are over 3 million lines in en_US datasets.
# The longest line is in en_US.blogs, with 40835 characters

max(nchar(us_twitter))
max(nchar(us_blogs))
max(nchar(us_news))

# Data Understanding reveals that en_US is a huge dataset. Overall, it contains more than three million lines and seventy million words.
# Considering the required computational power and memory limits, sampling the dataset to sub-sets is a reasonable approach.


# 2. Data Preparation
# 2.1 Sampling
# Calculate en_US.twitter data with confidence level at 95% and confidence interval at 0.75, the sample size is 16951.
# The sample size for en_US.blogs is 16756, for en_US.news is 13984.

sample_twitter <- sample(us_twitter, size = 16951)
sample_blogs <- sample(us_blogs, size = 16756)
sample_news <- sample(us_news, size = 13984)

length(sample_twitter)
length(sample_blogs)
length(sample_news)

# 2.2 Data Preprocessing

Ctwitter <- Corpus(VectorSource(sample_twitter))

inspect(head(Ctwitter))

# Rplace "/", "@", and "|" , and "'s" with space

ToSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
Ctwitter <- tm_map(Ctwitter, ToSpace, "/")
Ctwitter <- tm_map(Ctwitter, ToSpace, "@")
Ctwitter <- tm_map(Ctwitter, ToSpace, "\\|")
Ctwitter <- tm_map(Ctwitter, ToSpace, "\\b[A-z]\\b{1}")

# Convert to lower case

Ctwitter <- tm_map(Ctwitter, content_transformer(tolower))

# Remove Numbers, puntuation

Ctwitter <- tm_map(Ctwitter, removeNumbers)

Ctwitter <- tm_map(Ctwitter, removePunctuation)

# Remove stopwords
Ctwitter <- tm_map(Ctwitter, removeWords, stopwords())

# Remove extra spaces
Ctwitter <- tm_map(Ctwitter, stripWhitespace)

# Stemmig the text
Ctwitter <- tm_map(Ctwitter, stemDocument)

# 3. Data Exploratory
# Create a term-document matrix for twitter corpus
dtm_twitter <- TermDocumentMatrix(Ctwitter, control = list(stopwords=TRUE))

Highfreq <- findFreqTerms(dtm_twitter, lowfreq = 2000, highfreq = Inf)
Highfreq

matrix_t <- as.matrix(dtm_twitter)
matrix_t

sort <- sort(rowSums(matrix_t), decreasing = TRUE)
df <- data.frame(word = names(sort), freq = sort)
head(df, 10)

# Generate Frequency distribution
barplot(df[1:10, ]$freq, las = 2, names.arg = df[1:10, ]$word, col = "blue", main = "The Most Frequent Word", ylab = "Frequencies")

# Generate word cloud
wordcloud(words = df$word, freq = df$ freq, min.freq=4, max.words = 200, random.order = FALSE, scale = c(3, 0.5), colors = rainbow(3))
