library(shiny)
library(shinydashboard)
library(fmsb)

ui <- fluidPage(
    titlePanel("Soccer Player Market Value Prediction Tool"),
    sidebarLayout(
        sidebarPanel(
            numericInput("age", "Age (in years):", min = 1, max = 100, value = 28),
            numericInput("potential", "Development Potential (0-100):", min = 0, max = 100, value = 78),
            numericInput("international_reputation", "International Reputation (0-5):", min = 0, max = 5, value = 2),
            numericInput("overall", "Overall Skill Level (0-100):", min = 0, max = 100, value = 80),
            numericInput("skill_moves", "Special Move Ability (0-5):", min = 0, max = 5, value = 4),
            numericInput("pace", "Pace in Running (0-100):", min = 0, max = 100, value = 70),
            numericInput("shooting", "Shooting Ability (0-100):", min = 0, max = 100, value = 89),
            numericInput("passing", "Passing Ability (0-100):", min = 0, max = 100, value = 30),
            numericInput("dribbling", "Dribbling Ability (0-100):", min = 0, max = 100, value = 81),
            numericInput("defending", "Defensive Ability (0-100):", min = 0, max = 100, value = 20),
            numericInput("physic", "Physical Ability (0-100):", min = 0, max = 100, value = 20)
        ),
        mainPanel(
           valueBoxOutput("marketvalue_box"),
           plotOutput("barplot"),
           plotOutput("spiderplot")
        )
    )
)

server <- function(input, output) {

    boxplot_dataframe <- reactive({
        data.frame(metric = c("age", "potential", "international_reputation", "overall", "skill_moves", "pace", "shooting", "passing", "dribbling", "defending", "physic"),
                   value = as.integer(c(input$age, input$potential, input$international_reputation, input$overall, input$skill_moves, input$pace, input$shooting, input$passing, input$dribbling, input$defending, input$physic)))
    }) 
    
    prediction_dataframe <- reactive({
        data.frame(age = input$age, potential = input$potential, international_reputation = input$international_reputation,
                   overall = input$overall, skill_moves = input$skill_moves, pace = input$pace, shooting = input$shooting,
                   passing = input$passing, dribbling = input$dribbling, defending = input$defending, physic = input$physic)
    })
    
    output$marketvalue_box <- renderValueBox({
        df <- prediction_dataframe()
        readRDS("../models/rf_model.rds")
        df['market_value_prediction'] <- predict(rf.1, df)
        
        valueBox(value = paste0(round(df$market_value_prediction,2), "\u20AC"),
                 subtitle = "Predicted Player Market Value",
                 icon = icon("euro"))
    })
    
    output$barplot <- renderPlot({
        df <- boxplot_dataframe()
        ggplot2::ggplot(df, aes(x = metric, y = value)) +
            geom_bar(stat = "identity", width = 0.4)
            #+coord_flip() 
            #+coord_cartesian(ylim = c(0,100))
    }) 
    
    output$spiderplot <- renderPlot({
        df <- prediction_dataframe()
        radarchart(df)
    })
}

shinyApp(ui = ui, server = server)
