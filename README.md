# Predict Next Word
Large databases comprising of text in a target language are commonly used when generating language models for various purposes. In this exercise, we will use the English database but may consider three other databases in German, Russian and Finnish. The English data set contains three subsets, en_US.twitter, en_US.blogs, en_US.news. The data set is large. en_US.blogs alone is 200MB. en_US.twitter has 2360148 lines. 5% of dataset will be sampled and used to do this project. 

1.	Build basic n-gram model - using the exploratory analysis you performed, build a basic n-gram model for predicting the next word based on the previous 1, 2, or 3 words.
2.	Build a model to handle unseen n-grams - in some cases people will want to type a combination of words that does not appear in the corpora. Build a model to handle cases where a particular n-gram isn't observed.
