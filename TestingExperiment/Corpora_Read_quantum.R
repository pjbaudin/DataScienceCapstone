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
                          list.files(path = paste("./Corpora", "/",
                                                  subdir[i], sep = "")),
                          sep = "")
        CorpusPath[[i]] <- FilePath
        
}

# list of all the text files for each languages
CorpusPath

# Import the English corpora
EN_blog <- readLines(CorpusPath[[2]][1])


# Cleaning function for the Corpus
clean_corpus <- function(corpus){
        corpus <- tm_map(corpus, removeNumbers)
        corpus <- tm_map(corpus, content_transformer(tolower))
        corpus <- tm_map(corpus, stripWhitespace)
        corpus <- tm_map(corpus, removePunctuation)
        return(corpus)
}

# Function to convert text into a tdm matrix
# and output dataframe with words and frequency
TextProc <- function(text) {
        df <- data.frame()
        text <- VectorSource(text)
        text <- VCorpus(text)
        text <- clean_corpus(text)
        text <- TermDocumentMatrix(text)
        text <- as.matrix(text)
        df$Words <- row.names(text)
        df$num <- rowSums(text)
        df
}

##########################################################
# Loop to process text by increment
set.seed(123)
EN_blog <- sample(EN_blog)

master_m <- matrix()

for (i in 1:length(EN_blog)) {
        text <- EN_blog[i]
        
        text_m <- TextProc(text)
        
        master_m <- 
}


##########################################################
# Pre-processing for exploratory analysis for line 1
EN_blog_source1 <- VectorSource(EN_blog[1:5])

EN_blog_corpus1 <- VCorpus(EN_blog_source1)

EN_blog_corpus1 <- clean_corpus(EN_blog_corpus1)

EN_blog_dtm1 <- TermDocumentMatrix(EN_blog_corpus1)

EN_blog_m1 <- as.matrix(EN_blog_dtm1)

# Calculate the rowSums: term_frequency
term_frequency1 <- colSums(EN_blog_m1)

# Sort term_frequency in descending order
term_frequency1 <- sort(term_frequency1, decreasing = TRUE)

# View the top 10 most common words
term_frequency1[1:10]

# Plot a barchart of the 10 most common words
barplot(term_frequency1[1:10], col = "tan", las = 2)


##########################################################
# Pre-processing for exploratory analysis for line 2
EN_blog_source2 <- VectorSource(EN_blog[6:10])

EN_blog_corpus2 <- VCorpus(EN_blog_source2)

EN_blog_corpus2 <- clean_corpus(EN_blog_corpus2)

EN_blog_dtm2 <- DocumentTermMatrix(EN_blog_corpus2)

EN_blog_m2 <- as.matrix(EN_blog_dtm2)

# Calculate the rowSums: term_frequency
term_frequency2 <- colSums(EN_blog_m2)

# Sort term_frequency in descending order
term_frequency2 <- sort(term_frequency2, decreasing = TRUE)

# View the top 10 most common words
term_frequency2[1:10]

# Plot a barchart of the 10 most common words
barplot(term_frequency2[1:10], col = "tan", las = 2)


##############################
# Combining corpora

EN_blog_combined <- c(EN_blog_dtm1, EN_blog_dtm2)

EN_blog_mCombined <- as.matrix(EN_blog_combined)

# Calculate the rowSums: term_frequency
term_frequencyCombined <- colSums(EN_blog_mCombined)

# Sort term_frequency in descending order
term_frequencyCombined <- sort(term_frequencyCombined, decreasing = TRUE)

# View the top 10 most common words
term_frequencyCombined[1:10]

# Plot a barchart of the 10 most common words
barplot(term_frequencyCombined[1:10], col = "tan", las = 2)


###################
# Note for the processing of the corpus
# 
# 1 Randomly pick lines and preprocess into dtm (sample dtm)
# 2 Combine sample dtm with already processed dtm
# 3 Compute term frequency
# 4 Compare before/after adding sample dtm
# 5 if addition of sample does not provide enough info, stop adding sample
# 
# Need reference about the number of words
# Calculate coverrage with unigram/bigrams/trigrams
# 
# Number of unique words as percentage of the total number of words
# Need to determine appropriate ratio
# 