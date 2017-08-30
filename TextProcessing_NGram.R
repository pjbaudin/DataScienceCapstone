setwd("D:/Ubuntu Desktop/CapstoneSwiftKey")

library(ggplot2)
library(dplyr)
library(stringr)
library(readr)
library(stringr)
library(tau) 
library(data.table)
library(stringi)

# given a string vector and size of ngrams this function returns
# word ngrams with corresponding frequencies
createNgram <- function(stringVector, ngramSize){
        ngram <- data.table()
        ng <- textcnt(stringVector,
                      method = "string", n = ngramSize,
                      split = "[[:space:]]+")
        
        if (ngramSize == 1) {
                ngram <- data.table(w1 = names(ng),
                                    freq = unclass(ng), length = nchar(names(ng)))  
        }
        else {
                ngram <- data.table(wordcomb = names(ng),
                                    freq = unclass(ng), length = nchar(names(ng)))
        }
        
        return(ngram)
}

TextProcessing <- function(xlen) {
        
        print(paste("Start time:", Sys.time(), sep = " "))
        
        blogfile <- "./Corpora/en_US/en_US.blogs.txt"
        newsfile <- "./Corpora/en_US/en_US.news.txt"
        twitterfile <- "./Corpora/en_US/en_US.twitter.txt"
        
        corpus <- c(read_lines_raw(blogfile, progress = FALSE),
                         read_lines_raw(newsfile, progress = FALSE),
                         read_lines_raw(twitterfile, progress = FALSE))
        
        print(paste("Text imported at:", Sys.time(), sep = " "))
        
        corpus <- sample(corpus, xlen) %>%
                # Convert character vectors between encoding
                iconv(from = "UTF-8", to = "latin1", sub = " ") %>%
                # Conver all to lower case
                str_to_lower() %>%
                # Remove all numbers and punctuation except for caret
                str_replace_all("[^[:alnum:][:space:]'\"]","") %>%
                # Shrink all to one white space
                str_replace_all("[\\s]+", " ") %>%
                # Remove all alphanumric
                str_replace_all("[0-9]+", "")
        
        # Applying function
        corpus <- stri_join(corpus, collapse = " ")
        
        print(paste("Corpus cleaned at:", Sys.time(), sep = " "))
        
        for (i in 2:5) {
                tdm <- createNgram(corpus, i)
                tdm <- tdm[order(-freq)]
                
                if (i == 2) {
                        tdm <- tdm %>%
                                mutate(second_word = str_split(wordcomb, pattern = " ",
                                                               n = as.integer(i), simplify = TRUE)[, 1],
                                       first_word = str_split(wordcomb, pattern = " ",
                                                              n = as.integer(i), simplify = TRUE)[, 2]) %>%
                                select(wordcomb, second_word, first_word, freq)
                        
                        print(paste(Sys.time(), "2-gram completed!", sep = ""))
                        
                } else if (i == 3) {
                        tdm <- tdm %>%
                                mutate(third_word = str_split(wordcomb, pattern = " ",
                                                              n = as.integer(i), simplify = TRUE)[, 1],
                                       second_word = str_split(wordcomb, pattern = " ",
                                                               n = as.integer(i), simplify = TRUE)[, 2],
                                       first_word = str_split(wordcomb, pattern = " ", 
                                                              n = as.integer(i), simplify = TRUE)[, 3]) %>%
                                select(wordcomb, third_word, second_word, first_word, freq)
                        
                        print(paste(Sys.time(),"3-gram completed!", sep = ""))
                        
                } else if (i == 4) {
                        tdm <- tdm %>%
                                mutate(fourth_word = str_split(wordcomb, pattern = " ", 
                                                               n = as.integer(i), simplify = TRUE)[, 1],
                                       third_word = str_split(wordcomb, pattern = " ",
                                                              n = as.integer(i), simplify = TRUE)[, 2],
                                       second_word = str_split(wordcomb, pattern = " ", 
                                                               n = as.integer(i), simplify = TRUE)[, 3],
                                       first_word = str_split(wordcomb, pattern = " ", 
                                                              n = as.integer(i), simplify = TRUE)[, 4]) %>%
                                select(wordcomb, fourth_word, third_word, second_word, first_word, freq)
                        
                        print(paste(Sys.time(),"4-gram completed!", sep = ""))
                        
                } else if (i == 5) {
                        tdm <- tdm %>%
                                mutate(fifth_word = str_split(wordcomb, pattern = " ", 
                                                              n = as.integer(i), simplify = TRUE)[, 1],
                                       fourth_word = str_split(wordcomb, pattern = " ",
                                                               n = as.integer(i), simplify = TRUE)[, 2],
                                       third_word = str_split(wordcomb, pattern = " ",
                                                              n = as.integer(i), simplify = TRUE)[, 3],
                                       second_word = str_split(wordcomb, pattern = " ", 
                                                               n = as.integer(i), simplify = TRUE)[, 4],
                                       first_word = str_split(wordcomb, pattern = " ", 
                                                              n = as.integer(i), simplify = TRUE)[, 5]) %>%
                                select(wordcomb, fifth_word, fourth_word, third_word, second_word, first_word, freq)
                        
                        print(paste(Sys.time(),"5-gram completed!", sep = ""))
                        
                }
                
                File <- paste("./DataScienceCapstone/", Sys.Date(), "Sampling_", i, "-gram", ".rds", sep = "")
                saveRDS(tdm, file = File)
                
        }
        
}

library(tictoc)

tic()
TextProcessing(100)
toc()
