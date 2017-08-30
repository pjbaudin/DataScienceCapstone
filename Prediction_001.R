setwd("D:/Ubuntu Desktop/CapstoneSwiftKey")

library(dplyr)
library(stringr)
library(stringi)
library(rebus)
# 
bigram <- readRDS("./DataScienceCapstone/2017-08-24Sampling_2-gram.rds")

trigram <- readRDS("./DataScienceCapstone/2017-08-24Sampling_3-gram.rds")

quadgram <- readRDS("./DataScienceCapstone/2017-08-24Sampling_4-gram.rds")

quintgram <- readRDS("./DataScienceCapstone/2017-08-24Sampling_5-gram.rds")


CleanInput <- function(input) {
        input %>%
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
}

predictNextWord <- function(CleanInput){
        # Use bigram to predict
        # find pattern in combine word
        next_2 <- bigram[str_detect(bigram$wordcomb,
                                    pattern = str_c(CleanInput[length(CleanInput)],
                                                    collapse = " ")), ] %>%
                filter(first_word != CleanInput[length(CleanInput)])
        # Index match in first word
        ind <- str_detect(next_2$first_word, pattern = str_c(".*", CleanInput[length(CleanInput)], ".*"))
        # remove match in first word and select top row (highest fequency)
        next_2 <- next_2[!ind, ] %>%
                head(n = 1) %>%
                select(-wordcomb, -freq) %>%
                unlist(use.names = FALSE)
        
        if (length(next_2) > 0) {
                next_2 <- sub(str_c(".*", str_c(CleanInput[length(CleanInput)], collapse = " "), " "),
                              "", str_c(next_2, collapse = " "))
        } else {
                next_2 <- as.character("")
        }
        
        print(next_2)
        
        # Use trigram to predict
        # find pattern in combine word
        next_3 <- trigram[str_detect(trigram$wordcomb,
                                     pattern = str_c(CleanInput[(length(CleanInput) - 1):length(CleanInput)],
                                                     collapse = " ")), ] %>%
                filter(first_word != CleanInput[length(CleanInput)])
        # Index match in first word
        ind <- str_detect(next_3$first_word, pattern = str_c(".*", CleanInput[length(CleanInput)], ".*"))
        # remove match in first word and select top row (highest fequency)
        next_3 <- next_3[!ind, ] %>%
                head(n = 1) %>%
                select(-wordcomb, -freq) %>%
                unlist(use.names = FALSE)
        
        if (length(next_3) > 0) {
                next_3 <- sub(str_c(".*", str_c(CleanInput[(length(CleanInput) - 1):length(CleanInput)], collapse = " "), " "),
                              "", str_c(next_3, collapse = " "))
        } else {
                next_3 <- as.character("")
        }
        
        print(next_3)
        
        # use quadrigram to predict
        # find pattern in combine word
        next_4 <- quadgram[str_detect(quadgram$wordcomb,
                                      pattern = str_c(CleanInput[(length(CleanInput) - 2):length(CleanInput)],
                                                      collapse = " ")), ] %>%
                filter(first_word != CleanInput[length(CleanInput)])
        # Index match in first word
        ind <- str_detect(next_4$first_word, pattern = str_c(".*", CleanInput[length(CleanInput)], ".*"))
        # remove match in first word and select top row (highest fequency)
        next_4 <- next_4[!ind, ] %>%
                head(n = 1) %>%
                select(-wordcomb, -freq) %>%
                unlist(use.names = FALSE)
        
        if (length(next_4) > 0) {
                next_4 <- sub(str_c(".*", str_c(CleanInput[(length(CleanInput) - 2):length(CleanInput)], collapse = " "), " "),
                              "", str_c(next_4, collapse = " "))
        } else {
                next_4 <- as.character("")
        }
        
        print(next_4)
        
        # use quintgram to predict
        # find pattern in combine word
        next_5 <- quintgram[str_detect(quintgram$wordcomb,
                                       pattern = str_c(CleanInput[(length(CleanInput) - 3):length(CleanInput)],
                                                       collapse = " ")), ] %>%
                filter(first_word != CleanInput[length(CleanInput)])
        # Index match in first word
        ind <- str_detect(next_5$first_word, pattern = str_c(".*", CleanInput[length(CleanInput)], ".*"))
        # remove match in first word and select top row (highest fequency)
        next_5 <- next_5[!ind, ] %>%
                head(n = 1) %>%
                select(-wordcomb, -freq) %>%
                unlist(use.names = FALSE)
        
        if (length(next_5) > 0) {
                next_5 <- sub(str_c(".*", str_c(CleanInput[(length(CleanInput) - 3):length(CleanInput)], collapse = " "), " "),
                              "", str_c(next_5, collapse = " "))

        } else {
                next_5 <- as.character("")
        }
        
        print(next_5)
        
}

input <- "I don't think I remove"
predictNextWord(CleanInput(input))