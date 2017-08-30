#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(stringr)
library(stringi)
library(rebus)

# Define server logic required to draw a histogram
CleanText <- function(input) {
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
        next_2 <- bigram %>%
                filter(str_detect(wordcomb, str_c(CleanInput[length(CleanInput)],
                                                  collapse = " ")),
                       !str_detect(first_word, CleanInput[length(CleanInput)])) %>%
                head(n = 1) %>%
                select(-wordcomb, -freq) %>%
                unlist(use.names = FALSE)
        
        if (length(next_2) > 0) {
                next_2 <- sub(str_c(".*", str_c(CleanInput[length(CleanInput)], collapse = " "), ".*?", " "),
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
                next_3 <- sub(str_c(".*", str_c(CleanInput[(length(CleanInput) - 1):length(CleanInput)], collapse = " "), ".*?", " "),
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
                next_4 <- sub(str_c(".*", str_c(CleanInput[max((length(CleanInput) - 2), 1):length(CleanInput)], collapse = " "), ".*?", " "),
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
                next_5 <- sub(str_c(".*", str_c(CleanInput[max((length(CleanInput) - 3), 1):length(CleanInput)], collapse = " "), ".*?", " "),
                              "", str_c(next_5, collapse = " "))
        } else {
                next_5 <- as.character("")
        }
        
        wordpred <- c(next_2, next_3, next_4, next_5)
        
        wordpred
}

predictNextWord(CleanText("and"))

shinyServer(
        function(input, output) {
                predictedword <- reactive({
                        predictNextWord(CleanInput(input$textinput))
                })
                
                output$enter <- renderText({
                        input$Go
                        input$textinput
                })
                
                output$nextWord <- predictedword
        }
)