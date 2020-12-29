library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Soccer Player Market Value Prediction Tool"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("age", "Age (in years):", min = 1, max = 100, value = 28),
            numericInput("potential", "Development Potential (0-100):", min = 0, max = 100, value = 78),
            numericInput("international_reputation", "International Reputation (0-5):", min = 0, max = 5, value = 2)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           valueBoxOutput("marketvalue_box"),
           plotOutput("barplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    boxplot_dataframe <- reactive({
        data.frame(metric = c("age", "potential", "international_reputation"),
                   value = as.integer(c(input$age, input$potential, input$international_reputation)))
    }) 
    
    prediction_dataframe <- reactive({
        data.frame(age = input$age, potential = input$potential, international_reputation = input$international_reputation)
    })
    
    output$marketvalue_box <- renderValueBox({
        df <- prediction_dataframe()
        readRDS("../models/rf_model.rds")
        df['market_value_prediction'] <- predict(rf.1, df)
        
        valueBox(value = df$market_value_prediction,
                 subtitle = "Predicted Player Market Value",
                 icon = icon("euro"))
    })
    
    output$barplot <- renderPlot({
        df <- boxplot_dataframe()
        ggplot2::ggplot(df, aes(x = metric, y = value)) +
            geom_bar(stat = "identity", width = 0.4) +
            coord_flip() +
            coord_cartesian(ylim = c(0,100))
    }) 
}

# Run the application 
shinyApp(ui = ui, server = server)
