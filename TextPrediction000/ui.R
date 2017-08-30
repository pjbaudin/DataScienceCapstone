library(shiny)
library(shinydashboard)

# Header ----
header <- dashboardHeader(title = "App",
                          dropdownMenu(type = "messages",
                                       messageItem(
                                               from = "Support",
                                               message = "For feedback, email: pierre.evolt@live.com",
                                               icon = icon("life-ring")
                                       )
                          )
)

# Sidebar ----
# Sidebar setup

sidebar <- dashboardSidebar(
        sidebarMenu(
                # Main Dashboard
                menuItem("Text Prediction App", tabName = "MainDash",
                         icon = icon("check-square")),
                # Incoming quality control monitoring
                menuItem("About the App", tabName = "App",
                         icon = icon("gear"))
        )
)

# Body ----
# 
body <- dashboardBody(
         tabItems(
                 # Main dashboard body ----
                 tabItem(tabName = "MainDash",
                         h2("Text Prediction App"),
                         
                         h3("Input your text"),
                         p("Predict next word(s) based on n-gram model and backward prediction"),
                         p("Example: \"What %would &$I\""),
                         fluidRow(
                                 box(textInput("caption", "Enter your text here:", " ")
                                     )
                                 ),
                         p("Note: The prediction is automatically updated as you type in your text."),
                         
                         h3("Prediction"),
                         p("Based on your input, the algorithm predict the following words as follow:"),
                         fluidRow(
                                 box(titel = "Next word prediction:",
                                     tableOutput("value"))
                         ),
                         p("Note:"),
                         p("Prediction are based on a n-gram model and backward evaluation. The accuracy of the prediction can seem inadequate. This may be related to the input n-gram model output. Due to limited processing power, the prediction table is restricted and doesn't cover all the possibilities.")
                 ),
                 
                 tabItem(tabName = "App",
                         h3("About the application"),
                         p("Note:"),
                         p("Prediction are based on a n-gram model and backward evaluation. The accuracy of the prediction can seem inadequate. This may be related to the input n-gram model which might not be enough to cover all the possibilities.")
                 )
         )
)
                       


# Dashboard Run ----
dashboardPage(
        header,
        skin = "green",
        sidebar,
        body
)
