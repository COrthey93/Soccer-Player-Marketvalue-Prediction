# WEB APP LIBRARIES
library(shiny)
library(ggplot2)
library(shinydashboard)
library(fmsb)
library(shinythemes)

# 0.    SOURCE FILES 
#source("../data/data_preparation.R")

# 1.    IMPORT TRAINED ML MODELS
ml_model <- readRDS("../models/rf_model.rds")

# 2.    DEFINE WEB APP UI
ui <- fluidPage(
    theme = shinytheme("united"),
    title = "Soccer Player Market Value Prediction Tool",
    h3("Soccer Player Market Value Prediction Tool"),
    fluidRow(
        column(
            h3("Player Attributes"),
            width = 8,
            column(
                width = 4,
                sliderInput("wage_eur", "Player Wage (in €):", min = 500, max = 560000, value = 100000, step = 1000),
                sliderInput("movement_reactions", "Movement Reactions (in %):", min = 0, max = 100, value = 85, step = 1),
                sliderInput("potential", "Potential (in %):", min = 0, max = 100, value = 75, step = 1)
            ),
            column(
                width = 4,
                sliderInput("international_reputation", "International Reputation (0-5):", min = 0, max = 5, value = 4, step = 1),
                sliderInput("age", "Age of the Player (in years):", min = 0, max = 100, value = 22, step = 1),
                sliderInput("attacking_finishing", "Finishing Rate (in %):", min = 0, max = 100, value = 81, step = 1)
            ),
            column(
                width = 4,
                sliderInput("power_stamina", "Stamina (in %):", min = 0, max = 100, value = 40, step = 1),
                sliderInput("movement_agility", "Movement Agility (in %):", min = 0, max = 100, value = 80, step = 1),
                sliderInput("attacking_short_passing", "Short Passing Rate (in %):", min = 0, max = 100, value = 75, step = 1)
            ),
        ),
        column(
            h3("Predicted Player Market Values"),
            width = 4,
            valueBoxOutput("marketvalue_box")
        )
    ),
    fluidRow(
        column(
            h3("Player Attribute Graphic"),
            width = 12,
            plotOutput("barplot")   
        )
    )
)

# 3.    DEFINE WEB APP SERVER
server <- function(input, output) {
    
    boxplot_dataframe <- reactive({
        data.frame(metric = c("Wage", "Movement Reactions", "Potential", 
                              "International Reputation", "Age", "Finishing Rate", "Stamina", "Movement Agility", 
                              "Short Passing Rate"),
                   value = as.integer(c(input$wage_eur, 
                                        input$movement_reactions, 
                                        input$potential, 
                                        input$international_reputation, 
                                        input$age, 
                                        input$attacking_finishing, 
                                        input$power_stamina, 
                                        input$movement_agility, 
                                        input$attacking_short_passing)
                                      )
                   )
    }) 
    
    prediction_dataframe <- reactive({
        data.frame(wage_eur                 = as.integer(req(input$wage_eur)), 
                   movement_reactions       = as.integer(req(input$movement_reactions)), 
                   potential                = as.integer(req(input$potential)),
                   international_reputation = as.integer(req(input$international_reputation)), 
                   age                      = as.integer(req(input$age)), 
                   attacking_finishing      = as.integer(req(input$attacking_finishing)), 
                   power_stamina            = as.integer(req(input$power_stamina)),
                   movement_agility         = as.integer(req(input$movement_agility)), 
                   attacking_short_passing  = as.integer(req(input$attacking_short_passing))
                   )
    })
    
    output$marketvalue_box <- renderValueBox({
        df <- prediction_dataframe()
        df['market_value_prediction'] <- predict(ml_model, df)

        valueBox(value = paste0(prettyNum(round(df$market_value_prediction,2), big.mark = ","), "\u20AC"),
                 subtitle = "Predicted Player Market Value")
    })
    
    output$barplot <- renderPlot({
        df <- boxplot_dataframe()
        ggplot2::ggplot(df, aes(x = metric, y = value)) +
            geom_bar(stat = "identity", width = 0.4)
    }) 
}

# 4.    RUN WEB APP
shinyApp(ui = ui, server = server)
