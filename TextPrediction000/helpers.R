
library(shiny)
library(dplyr)
library(stringr)
library(stringi)
library(rebus)

setwd("D:/Ubuntu Desktop/CapstoneSwiftKey")

# Load the n-gram dataframe 
bigram <- readRDS("./DataScienceCapstone/2017-08-24Sampling_2-gram.rds")
trigram <- readRDS("./DataScienceCapstone/2017-08-24Sampling_3-gram.rds")
quadgram <- readRDS("./DataScienceCapstone/2017-08-24Sampling_4-gram.rds")
quintgram <- readRDS("./DataScienceCapstone/2017-08-24Sampling_5-gram.rds")


predictNextWord <- function(CleanInput){
        # Clean input text
        CleanInput <- CleanInput %>%
                # Convert character vectors between encoding
                iconv(from = "UTF-8", to = "latin1", sub = " ") %>%
                # Conver all to lower case
                str_to_lower() %>%
                # Remove all numbers and punctuation except for caret
                str_replace_all("[^[:alnum:][:space:]'\"]","") %>%
                # Shrink all to one white space
                str_replace_all("[\\s]+", " ") %>%
                # Remove all alphanumric
                str_replace_all("[0-9]+", "") %>%
                # trim white space
                str_trim(side = "both") %>%
                str_split(pattern = " ", simplify = TRUE) %>%
                c()
        
        # Use bigram to predict
        # find pattern in combine word
        next_2 <- bigram %>%
                filter(str_detect(wordcomb, str_c(CleanInput[length(CleanInput)],
                                                  collapse = " ")),
                       !str_detect(first_word, CleanInput[length(CleanInput)])) %>%
                head(n = 1) %>%
                select(-wordcomb, -freq) %>%
                unlist(use.names = FALSE)
        
        if (length(next_2) > 0) {
                next_2 <- sub(str_c(".*", str_c(CleanInput[length(CleanInput)],
                                                collapse = " "), ".*?", " "),
                              "", str_c(next_2, collapse = " "))
        } else {
                next_2 <- as.character("")
        }
        
        # Use trigram to predict
        # find pattern in combine word and filster match in last word of input
        next_3 <- trigram %>%
                filter(str_detect(wordcomb, str_c(CleanInput[max((length(CleanInput) - 1), 1):length(CleanInput)],
                                                  collapse = " ")),
                       !str_detect(first_word, CleanInput[length(CleanInput)])) %>%
                head(n = 1) %>%
                select(-wordcomb, -freq) %>%
                unlist(use.names = FALSE)
        
        if (length(next_3) > 0) {
                next_3 <- sub(str_c(".*", str_c(CleanInput[(length(CleanInput) - 1):length(CleanInput)],
                                                collapse = " "), ".*?", " "),
                              "", str_c(next_3, collapse = " "))
        } else {
                next_3 <- as.character("")
        }
        
        # use quadrigram to predict
        # find pattern in combine word
        next_4 <- quadgram %>%
                filter(str_detect(wordcomb, str_c(CleanInput[max((length(CleanInput) - 2), 1):length(CleanInput)],
                                                  collapse = " ")),
                       !str_detect(first_word, CleanInput[length(CleanInput)])) %>%
                head(n = 1) %>%
                select(-wordcomb, -freq) %>%
                unlist(use.names = FALSE)
        
        if (length(next_4) > 0) {
                next_4 <- sub(str_c(".*", str_c(CleanInput[max((length(CleanInput) - 2), 1):length(CleanInput)],
                                                collapse = " "), ".*?", " "),
                              "", str_c(next_4, collapse = " "))
        } else {
                next_4 <- as.character("")
        }
        
        # use quintgram to predict
        # find pattern in combine word
        next_5 <- quintgram %>%
                filter(str_detect(wordcomb, str_c(CleanInput[max((length(CleanInput) - 3), 1):length(CleanInput)],
                                                  collapse = " ")),
                       !str_detect(first_word, CleanInput[length(CleanInput)])) %>%
                head(n = 1) %>%
                select(-wordcomb, -freq) %>%
                unlist(use.names = FALSE)
        
        if (length(next_5) > 0) {
                next_5 <- sub(str_c(".*", str_c(CleanInput[max((length(CleanInput) - 3), 1):length(CleanInput)],
                                                collapse = " "), ".*?", " "),
                              "", str_c(next_5, collapse = " "))
        } else {
                next_5 <- as.character("")
        }
        
        wordpred <- data.frame(`From 2-gram` = next_2,
                               `From 3-gram` = next_3,
                               `From 4-gram` = next_4,
                               `From 5-gram` = next_5)
        
        wordpred
}
