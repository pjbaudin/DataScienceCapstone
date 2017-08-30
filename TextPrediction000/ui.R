library(shiny)
library(shinythemes)

shinyUI(
        navbarPage("Data Science CapStone",
                   theme = shinytheme("cosmo"),   
                   tabPanel("Next Word Prediction",
                            fluidRow(
                                    column(2, offset = 2,
                                           textInput(inputId = 'textinput', label = h3("Please Enter Your Text")),
                                           helpText('Note: Do not enter profanity words.Only English is supported.Click Go button to get the predicted next word.'),
                                           submitButton('Go')
                                    ),
                                    column(1),
                                    column(4,
                                           h3('What You Entered'),
                                           tags$span(style = "color:blue",tags$strong(tags$h3(textOutput("enter")))),
                                           hr(),
                                           h3('Predicted next word is:'),
                                           helpText('Probability is in decreasing order.'),
                                           tags$span(style = "color:blue", tags$strong(tags$h4(tableOutput("nextWord")))),
                                           hr(),
                                           tags$span(style = "color:darked", tags$footer(("Developed by pjbaudin @ Aug 2017")))
                                    )
                            )
                   ),
                   tabPanel("About",
                            fluidRow(
                                    column(2)
                                    #column(8,includeMarkdown("./Readme.md"))
                            )         
                   )
        )
) 