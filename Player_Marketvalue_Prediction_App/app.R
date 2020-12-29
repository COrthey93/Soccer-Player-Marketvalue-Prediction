# WEB APP LIBRARIES
library(shiny)
library(ggplot2)
library(shinydashboard)
library(fmsb)
library(shinythemes)

# 0.    SOURCE FILES 
# 0.1   DATA PREPARATION
source("../data/data_preparation.R")

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
                sliderInput("age", "Age (in years):", min = 1, max = 100, value = 28),
                sliderInput("potential", "Development Potential (0-100):", min = 0, max = 100, value = 78),
                sliderInput("international_reputation", "International Reputation (0-5):", min = 0, max = 5, value = 2)
            ),
            column(
                width = 4,
                sliderInput("overall", "Overall Skill Level (0-100):", min = 0, max = 100, value = 80),
                sliderInput("skill_moves", "Special Move Ability (0-5):", min = 0, max = 5, value = 4),
                sliderInput("pace", "Pace in Running (0-100):", min = 0, max = 100, value = 70),
                sliderInput("shooting", "Shooting Ability (0-100):", min = 0, max = 100, value = 89)
            ),
            column(
                width = 4,
                sliderInput("passing", "Passing Ability (0-100):", min = 0, max = 100, value = 30),
                sliderInput("dribbling", "Dribbling Ability (0-100):", min = 0, max = 100, value = 81),
                sliderInput("defending", "Defensive Ability (0-100):", min = 0, max = 100, value = 20),
                sliderInput("physic", "Physical Ability (0-100):", min = 0, max = 100, value = 20)
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
        data.frame(metric = c("age", "potential", "international_reputation", "overall", "skill_moves", "pace", "shooting", "passing", "dribbling", "defending", "physic"),
                   value = as.integer(c(input$age, input$potential, input$international_reputation, input$overall, input$skill_moves, input$pace, input$shooting, input$passing, input$dribbling, input$defending, input$physic)))
    }) 
    
    prediction_dataframe <- reactive({
        data.frame(age = as.integer(req(input$age)), 
                   potential = as.integer(req(input$potential)), 
                   international_reputation = as.integer(req(input$international_reputation)),
                   overall = as.integer(req(input$overall)), 
                   skill_moves = as.integer(req(input$skill_moves)), 
                   pace = as.integer(req(input$pace)), 
                   shooting = as.integer(req(input$shooting)),
                   passing = as.integer(req(input$passing)), 
                   dribbling = as.integer(req(input$dribbling)), 
                   defending = as.integer(req(input$defending)), 
                   physic = as.integer(req(input$physic)))
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
