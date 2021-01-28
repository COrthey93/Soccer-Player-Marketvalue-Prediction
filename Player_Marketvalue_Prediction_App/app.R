# WEB APP LIBRARIES
library(shiny)
library(ggplot2)
library(shinydashboard)
library(fmsb)
library(shinythemes)
library(scales)

# 0.    SOURCE FILES 
source("../data/data_preparation.R")

# 1.    IMPORT TRAINED ML MODELS
ml_model <- readRDS("../models/nn_model.rds")

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
                sliderInput("age", "Age of the Player (in years):", min = 12, max = 40, value = 26, step = 1),
                sliderInput("potential", "Potential (in %):", min = 0, max = 100, value = 89, step = 1),
                sliderInput("international_reputation", "International Reputation (1-5):", min = 1, max = 5, value = 3, step = 1)
            ),
            column(
                width = 4,
                sliderInput("wage_eur", "Wage of the Player (in €):", min = 500, max = 560000, value = 190000, step = 500),
                sliderInput("power_stamina", "Stamina (in %):", min = 0, max = 100, value = 78, step = 1),
                sliderInput("attacking_finishing", "Finishing Rate (in %):", min = 0, max = 100, value = 84, step = 1)
            ),
            column(
                width = 4,
                sliderInput("attacking_short_passing", "Short Passing Rate (in %):", min = 0, max = 100, value = 87, step = 1),
                sliderInput("skill_dribbling", "Dribbling Ability (in %):", min = 0, max = 100, value = 91, step = 1),
                sliderInput("movement_reactions", "Movement Reactions (in %):", min = 0, max = 100, value = 84, step = 1)
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
            h3("Player Attribute Graphics"),
            width = 12,
            column(
                width = 6,
                plotOutput("barplot_pct", height = 400)   
            ),
            column(
                width = 2,
                plotOutput("barplot_age", height = 400)   
            ),
            column(
                width = 2,
                plotOutput("barplot_rep", height = 400)   
            ),
            column(
                width = 2,
                plotOutput("barplot_wage", height = 400)   
            )
        )
    )
)

# 3.    DEFINE WEB APP SERVER
server <- function(input, output) {
    
    barplot_dataframe <- reactive({
        data.frame(metric = c("Age", "Potential", "International Reputation", 
                              "Wage", "Power Stamina", "Finishing Rate", "Short Passing", "Dribbling", 
                              "Movement Reactions"),
                   value = as.integer(c(input$age, 
                                        input$potential, 
                                        input$international_reputation,
                                        input$wage_eur,
                                        input$power_stamina, 
                                        input$attacking_finishing, 
                                        input$attacking_short_passing, 
                                        input$skill_dribbling, 
                                        input$movement_reactions)
                                      )
                   )
    }) 
    
    prediction_dataframe <- reactive({
        data.frame(age                         = as.integer(req(input$age)), 
                   potential                   = as.integer(req(input$potential)), 
                   international_reputation    = as.integer(req(input$international_reputation)),
                   wage_eur                    = as.integer(req(input$wage_eur)),
                   power_stamina               = as.integer(req(input$power_stamina)), 
                   attacking_finishing         = as.integer(req(input$attacking_finishing)), 
                   attacking_short_passing     = as.integer(req(input$attacking_short_passing)),
                   skill_dribbling             = as.integer(req(input$skill_dribbling)), 
                   movement_reactions          = as.integer(req(input$movement_reactions))
                   )
    })
    
    output$marketvalue_box <- renderValueBox({
        df <- prediction_dataframe()
        df <- normalize(df, minval = minvec, maxval = maxvec)
        df['value_eur'] <- predict(ml_model, df)
        df <- denormalize(df, minval = minvec, maxval = maxvec)
        
        dispval <- if(df$value_eur >= 0){paste0(prettyNum(round(df$value_eur,2), big.mark = ","), "\u20AC")} else {"attribute combination not meaningful"}
        
        valueBox(value = dispval,
                 subtitle = "")
    })
    
    output$barplot_pct <- renderPlot({
        df <- barplot_dataframe()
        df <- df[df$metric %in% c("Potential", "Power Stamina", "Finishing Rate", "Short Passing", "Dribbling", "Movement Reactions"),]
        ggplot2::ggplot(df, aes(x = metric, y = value)) +
            geom_bar(stat = "identity", width = 0.4) +
            coord_cartesian(ylim = c(0,100)) +
            xlab(label = "") +
            ylab(label = "attribute level (0-100)")
    }) 
    
    output$barplot_age <- renderPlot({
        df <- barplot_dataframe()
        df <- df[df$metric %in% c("Age"),]
        ggplot2::ggplot(df, aes(x = metric, y = value)) +
            geom_bar(stat = "identity", width = 0.4) +
            coord_cartesian(ylim = c(0,39)) +
            xlab(label = "") +
            ylab(label = "age (in years)")
    }) 
    
    output$barplot_rep <- renderPlot({
        df <- barplot_dataframe()
        df <- df[df$metric %in% c("International Reputation"),]
        ggplot2::ggplot(df, aes(x = metric, y = value)) +
            geom_bar(stat = "identity", width = 0.4) +
            coord_cartesian(ylim = c(0,5)) +
            xlab(label = "") +
            ylab(label = "reputation level (1-5)")
    }) 
    
    output$barplot_wage <- renderPlot({
        df <- barplot_dataframe()
        df <- df[df$metric %in% c("Wage"),]
        ggplot2::ggplot(df, aes(x = metric, y = value)) +
            geom_bar(stat = "identity", width = 0.4) +
            scale_y_continuous(labels = scales::comma) +    
            xlab(label = "") +
            ylab(label = "wage (in EUR)")
    }) 
}

# 4.    RUN WEB APP
shinyApp(ui = ui, server = server)
