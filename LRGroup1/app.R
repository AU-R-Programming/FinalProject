library(shiny)
devtools::install_github("AU-R-Programming/FinalProject")
library(lmG1)
library(ggplot2)

ui <- fluidPage(

    titlePanel("Simple Linear Regression Tool"),

    sidebarLayout(
        sidebarPanel(
            # Ask the user for inputs X, y, and a
            # Future updates: matrix indexes, maybe a more clever input field?
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




server <- function(input, output) {

    output$beta <- (lmG1(y = input$y, X = input$X, a = input$a))$beta
    output$CI <- (lmG1(y = input$y, X = input$X, a = input$a))$confint
    output$Cp <- (lmG1(y = input$y, X = input$X, a = input$a))$mallow_cp
    output$Fstat <- (lmG1(y = input$y, X = input$X, a = input$a))$f_stat
    output$Pf <- (lmG1(y = input$y, X = input$X, a = input$a))$pf_value

    output$RvsF <- renderPlot({ # Residuals vs Fitted plot

        ggplot(mapping = aes(x = y_hat, y = residual)) +
            labs(title = 'Residuals vs Fitted', x = 'Fitted values', y = 'Residuals') +
            geom_point() + geom_smooth()

    })

    output$RQQ <- renderPlot({ # Residual QQ-plot

        ggplot(mapping = aes(sample = residual)) +
            labs(title = 'Residual QQ-plot', x = 'Theoretical Quantiles', y = 'Sample Quantiles') +
            stat_qq()
    })

    output$HistR <- renderPlot({ # Histogram of Residuals

        ggplot(mapping = aes(x = residual)) +
            geom_histogram(fill = 'steelblue', color = 'black') +
            labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency')
    })

}

# Run the application
shinyApp(ui = ui, server = server)


# Error with this version: Can't access reactive value 'y' outside of reactive consumer
# The Shiny App closes by itself after running
# This should have something to do with reactive variables (y, as pointed out by the program; and possibly X and a as well)
# Can't work out a solution for this problem
