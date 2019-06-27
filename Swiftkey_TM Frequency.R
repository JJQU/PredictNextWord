# Understanding the Data
# Load the data

install.packages("tm")
install.packages("stringr")
install.packages("wordcloud")
install.packages("gridExtra")

library(tm)
library(stringr)
library(ggplot2)
library(gridExtra)
library(wordcloud)
library(dplyr)

Twitter <- readLines("./data/final/en_US/en_US.twitter.txt")
Blog <- readLines("./data/final/en_US/en_US.blogs.txt")
News <- readLines("./data/final/en_US/en_US.news.txt")


L <- c(length(Twitter), length(Blog), length(News))
L

Max <- c(max(nchar(Twitter)), max(nchar(Blog)), max(nchar(News)))

Words <- c(sum(str_count(Twitter, "\\S+")), sum(str_count(Blog, "\\S+")), sum(str_count(News, "\\S+")))

df <- data.frame(c("Twitter", "Blogs", "News"), L, Words, Max)

colnames(df) <- c("Source", "Lines", "Words", "MaxWords" )
df

g1 <- ggplot(df, aes(x=Source, y=Lines/1e+06)) + geom_bar(stat = "identity", fill = "blue") + 
  labs(title = "Count of lines", y = "Number of Lines in Millions")
g1

g2 <- ggplot(df, aes(x=Source, y=Words/1e+06)) + geom_bar(stat = "identity", fill = "lightblue") + 
  labs(title = "Count of Words", y = "Number of Words in Millions")
g2

grid.arrange(g1, g2, ncol = 2)

sum(df$Lines)
sum(df$Words)
# Random Sampling
# Use rbinom function to generate x% of random sample 
# sub_sample is populate with x% of random samples that are line of text, using for loop

set.seed(1234)
main <- list(Twitter, Blog, News)
random <- lapply(main, function(x) rbinom(x, 1, 0.01))
sub_sample <- list(Twitter = NA, Blog = NA, News = NA)

for(i in seq_along(main)){
  sub_sample[[i]] <- main[[i]][random[[i]]==1]
}

str(sub_sample)

length(sub_sample)

L1 <- c(length(sub_sample$Twitter), length(sub_sample$Blog), length(sub_sample$News))
L1

class(sub_sample)
# Data Preprocessing
tw_corpus <- Corpus(VectorSource(sub_sample$Twitter))
length(tw_corpus)
length(tw_corpus_en)
tw_corpus_en <- Corpus(VectorSource(sapply(tw_corpus, function(row) iconv(row, "latin1", "ASCII", sub = " "))))

blog_corpus <- Corpus(VectorSource(sub_sample$Blog))
news_corpus <- Corpus(VectorSource(sub_sample$News))

# For twitter text, replace "/", "@", and "|" , and "'s", "#", and URL 

ToSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
tw_corpus <- tm_map(tw_corpus, ToSpace, "/")
tw_corpus <- tm_map(tw_corpus, ToSpace, "@")
tw_corpus <- tm_map(tw_corpus, ToSpace, "\\|")
tw_corpus <- tm_map(tw_corpus, ToSpace, "\\b[A-z]\\b{1}")
tw_corpus <- tm_map(tw_corpus, ToSpace, "#")
tw_corpus <- tm_map(tw_corpus, ToSpace, "http:[[:alnum:]]*")

# Convert to lower case

tw_corpus <- tm_map(tw_corpus, content_transformer(tolower))

# Remove Numbers, puntuation

tw_corpus <- tm_map(tw_corpus, removeNumbers)

tw_corpus <- tm_map(tw_corpus, removePunctuation)

# Remove stopwords
tw_corpus <- tm_map(tw_corpus, removeWords, stopwords())

# Remove extra spaces
tw_corpus <- tm_map(tw_corpus, stripWhitespace)

# Stemmig the text
tw_corpus <- tm_map(tw_corpus, stemDocument)

class(tw_corpus)
# 3. Data Exploratory
# Create a term-document matrix for twitter corpus
tdm_twitter <- TermDocumentMatrix(tw_corpus, control = list(stopwords=TRUE))

Highfreq <- findFreqTerms(tdm_twitter, lowfreq = 100, highfreq = Inf)
Highfreq

matrix_tw <- as.matrix(tdm_twitter)
matrix_tw

sort <- sort(rowSums(matrix_tw), decreasing = TRUE)
df_tw <- data.frame(word = names(sort), freq = sort)
head(df_tw)
df1 <- df_tw[1:10, ]
df1
# Generate Frequency distribution

g_tw <- ggplot(df1, aes(x = reorder(word, -freq), y = freq)) + geom_bar(stat = "identity") + 
  labs(x = "Words", y = "Frequency", title = "Most Frequent Words in Twitter")

g_tw


# Generate word cloud
wordcloud(words = df_tw$word, freq = df_tw$ freq, min.freq=4, max.words = 200, random.order = FALSE, scale = c(3, 0.5), colors = rainbow(3))

# Find high frequency word in Blogs

# For twitter text, replace "/", "@", and "|" , and "'s", "#", and URL 

ToSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
blog_corpus <- tm_map(blog_corpus, ToSpace, "/")
blog_corpus <- tm_map(blog_corpus, ToSpace, "@")
blog_corpus <- tm_map(blog_corpus, ToSpace, "\\|")
blog_corpus <- tm_map(blog_corpus, ToSpace, "\\b[A-z]\\b{1}")
blog_corpus <- tm_map(blog_corpus, ToSpace, "#")
blog_corpus <- tm_map(blog_corpus, ToSpace, "http:[[:alnum:]]*")

# Convert to lower case

blog_corpus <- tm_map(blog_corpus, content_transformer(tolower))

# Remove Numbers, puntuation

blog_corpus <- tm_map(blog_corpus, removeNumbers)

blog_corpus <- tm_map(blog_corpus, removePunctuation)

# Remove stopwords
blog_corpus <- tm_map(blog_corpus, removeWords, stopwords())

# Remove extra spaces
blog_corpus <- tm_map(blog_corpus, stripWhitespace)

# Stemmig the text
blog_corpus <- tm_map(blog_corpus, stemDocument)


# 3. Data Exploratory
# Create a term-document matrix for twitter corpus
tdm_blog <- TermDocumentMatrix(blog_corpus, control = list(stopwords=TRUE))
class(tdm_blog)

Highfreq_blog <- findFreqTerms(tdm_blog, lowfreq = 500, highfreq = Inf)
Highfreq_blog

matrix_blog <- as.matrix(tdm_blog)


sort2 <- sort(rowSums(matrix_blog), decreasing = TRUE)
df_blog <- data.frame(word =names(sort2), freq = sort)


# Generate Frequency distribution
g_blog <- ggplot(df_blog[1:10, ], aes(x = reorder(word, -freq), y = freq)) + geom_bar(stat = "identity") + 
  labs(x = "Words", y = "Frequency", title = "Most Frequent Words in Blogs")

g_blog


# Find word frequency in News

# For twitter text, replace "/", "@", and "|" , and "'s", "#", and URL 

ToSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
news_corpus <- tm_map(news_corpus, ToSpace, "/")
news_corpus <- tm_map(news_corpus, ToSpace, "\\|")
news_corpus <- tm_map(news_corpus, ToSpace, "\\b[A-z]\\b{1}")


# Convert to lower case

news_corpus <- tm_map(news_corpus, content_transformer(tolower))

# Remove Numbers, puntuation

news_corpus <- tm_map(news_corpus, removeNumbers)

news_corpus <- tm_map(news_corpus, removePunctuation)

# Remove stopwords
news_corpus <- tm_map(news_corpus, removeWords, stopwords())

# Remove extra spaces
news_corpus <- tm_map(news_corpus, stripWhitespace)

# Stemmig the text
news_corpus <- tm_map(news_corpus, stemDocument)


# 3. Data Exploratory
# Create a term-document matrix for twitter corpus
tdm_news <- TermDocumentMatrix(news_corpus, control = list(stopwords=TRUE))

Highfreq_news <- findFreqTerms(tdm_news, lowfreq = 10, highfreq = Inf)
Highfreq_news

matrix_news <- as.matrix(tdm_news)
matrix_news

sort3 <- sort(rowSums(matrix_news), decreasing = TRUE)
df_news <- data.frame(word = names(sort3), freq = sort3)


# Generate Frequency distribution

g_news <- ggplot(df_news[1:10, ], aes(x = reorder(word, -freq), y = freq)) + geom_bar(stat = "identity") + 
  labs(x = "Words", y = "Frequency", title = "Most Frequent Words in News")

g_news

grid.arrange(g_tw, g_blog, g_news, nrow = 3)

# Ngrams
install.packages("tidytext")
library(tidytext)
library(tidyr)

install.packages("igraph")
install.packages("ggraph")
library(igraph)
library(ggraph)

dtm_tw <- DocumentTermMatrix(tw_corpus)
tw_td <- tidy(dtm_tw)
head(tw_td)

tw_bigram <- tw_td %>% unnest_tokens(bigram, term, token = "ngrams", n = 2)
head(tw_bigram)


bigram_count <- tw_bigram %>% count(bigram, sort = TRUE)
bigram_filtered <- bigram_count %>%
  filter(!is.na(bigram)) 

bigram_graph <- bigram_filtered %>% 
  filter(n >50) %>% 
  graph_from_data_frame()

bigram_graph

set.seed(1234)


ggraph(bigram_graph, layout = "fr") + 
  geom_edge_link() + 
  geom_node_point(color = "pink", size = 3) + 
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) + 
  theme_void()
  
head(bigram_filtered)

tw_trigram <- tw_td %>% unnest_tokens(trigram, term, token = "ngrams", n = 3)
head(tw_trigram)


tw_sentiments <- tw_td %>% 
  inner_join(get_sentiments("bing"), by = c(term = "word"))
tw_sentiments

tw_sentiments %>%
  count(sentiment, term, wt = count) %>%
  ungroup() %>% 
  filter(n >= 100) %>% 
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) + 
  geom_bar(stat = "identity") + 
  ylab("Contribution to Sentiment") + 
  coord_flip()




