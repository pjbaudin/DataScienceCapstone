source("helpers.R")

shinyServer(function(input, output) {
        
        output$value <- renderTable({ predictNextWord(input$caption) })
})