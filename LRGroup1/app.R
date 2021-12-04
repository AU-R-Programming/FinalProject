#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

devtools::install_github("AU-R-Programming/FinalProject")
library(FinalProject)


library(ggplot2)




# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Simple Linear Regression Tool"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(

            textInput("X", "X", value = "1, 2, 3, 4, 5"),
            textInput("y", "y", value = "14, 24, 32, 55, 61"),
            textInput("a", "a", value = "0.05")

        ),


        mainPanel(
           textOutput("beta"),
           textOutput("CI"),
           textOutput("Cp"),
           textOutput("Fstat"),
           textOutput("Pf"),

           plotOutput("RvsF"),
           plotOutput("RQQ"),
           plotOutput("HistR")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$beta <- (lmG1(y = input$y, X = input$X, a = input$a))$beta
    output$CI <- (lmG1(y = input$y, X = input$X, a = input$a))$confint
    output$Cp <- (lmG1(y = input$y, X = input$X, a = input$a))$mallow_cp
    output$Fstat <- (lmG1(y = input$y, X = input$X, a = input$a))$f_stat
    output$Pf <- (lmG1(y = input$y, X = input$X, a = input$a))$pf_value

    output$RvsF <- renderPlot({
        # generate bins based on input$bins from ui.R

        lmG1(X = input$X, y = input$y)
    })

    output$RQQ <- renderPlot({


    })

    output$HistR <- renderPlot({


    })




}

# Run the application
shinyApp(ui = ui, server = server)
