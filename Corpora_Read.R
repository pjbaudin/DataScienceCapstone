# set working directory
setwd("D:/Data Science/R-Data-Science-Study/CapstoneSwiftKey")

# Load libraries
library(tm)
#library(qdap)
library(stringr)

# Set up directory
subdir <- list.files(path = "./Corpora")

# Loop to list all the files for each languages
CorpusPath <- list()

for (i in 1:length(subdir)) {
        FilePath <- paste("./Corpora", "/", subdir[i], "/",
                        list.files(path = paste("./Corpora", "/", subdir[i], sep = "")),
                        sep = "")
        CorpusPath[[i]] <- FilePath
        
}

# list of all the text files for each languages
CorpusPath

# Import the English corpora
EN_blog <- readLines(CorpusPath[[2]][1])
str(EN_blog)

# EN_tweets <- readLines(CorpusPath[[2]][3])
# str(EN_tweets)
# 
# EN_news <- readLines(CorpusPath[[2]][2])
# str(EN_news)



# The first step in building a predictive model for text is understanding the distribution and relationship between the words, tokens, and phrases in the text. The goal of this task is to understand the basic relationships you observe in the data and prepare to build your first linguistic models.


# Tasks to accomplish
 
# Exploratory analysis - perform a thorough exploratory analysis of the data, understanding the distribution of words and relationship between the words in the corpora.

# Understand frequencies of words and word pairs - build figures and tables to understand variation in the frequencies of words and word pairs in the data.

 
# Questions to consider

 
# Some words are more frequent than others - what are the distributions of word frequencies?
# What are the frequencies of 2-grams and 3-grams in the dataset?
# How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?
# How do you evaluate how many of the words come from foreign languages?
# Can you think of a way to increase the coverage -- identifying words that may not be in the corpora or using a smaller number of words in the dictionary to cover the same number of phrases?


# Pre-processing for exploratory analysis
EN_blog_source <- VectorSource(EN_blog)

EN_blog_corpus <- VCorpus(EN_blog_source)

# Cleaning function for the Corpus
clean_corpus <- function(corpus){
        corpus <- tm_map(corpus, removeNumbers)
        corpus <- tm_map(corpus, content_transformer(tolower))
        corpus <- tm_map(corpus, stripWhitespace)
        corpus <- tm_map(corpus, removePunctuation)
        return(corpus)
}

EN_blog_corpus <- clean_corpus(EN_blog_corpus)

EN_blog_dtm <- DocumentTermMatrix(EN_blog_corpus)

EN_blog_m <- as.matrix(EN_blog_dtm)

# Calculate the rowSums: term_frequency
term_frequency <- rowSums(EN_blog_m)

# Sort term_frequency in descending order
term_frequency <- sort(term_frequency, decreasing = TRUE)

# View the top 10 most common words
term_frequency[1:10]

# Plot a barchart of the 10 most common words
barplot(term_frequency[1:10], col = "tan", las = 2)