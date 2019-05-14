# Data Understanding
# The folder contains 4 sub-folders. Each contains four datasets for one language. Seperately they are in English, Russian, German, and Finnish.
# We use en_US datasets for this project. This dataset contains 3 files, en_US.twitter, en_US.blogs, and en_US.news.
# The whole size for this dataset is 556MB. The size for each file is, 159MB for twitter, 196MB for news, and 200MB for blogs, respectively.


library(tm)
library(stringi)

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
str(us_blogs)
lb <- length(us_blogs)
lb

con1 <- file("./data/final/en_US/en_US.news.txt", "r")
us_news <- readLines(con1)
close(con1)
str(us_news)
ln <- length(us_news)
ln

l <- c(lt,lb,ln)
l
sum(l)

# Overall there are over 3 million lines in en_US datasets.

max(nchar(us_twitter))
max(nchar(us_blogs))
max(nchar(us_news))

# The longest line is in en_US.blogs, with 40835 characters
# Find how many number of lines contain word "love" and "hate, both in lower case, in en_US.twitter data set.
l_tweet <- grep("love", us_twitter)
length(l_tweet)

h_tweet <- grep("hate", us_twitter)
length(h_tweet)

rate_lovehate = length(l_tweet)/length(h_tweet)
rate_lovehate

# Find out the sentence in en_US.twitter about "biostats"
bio_tweet <- grep("biostats", us_twitter)

us_twitter[bio_tweet]
