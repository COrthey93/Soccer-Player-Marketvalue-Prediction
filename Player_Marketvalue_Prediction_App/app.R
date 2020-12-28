library(shiny)

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
           tableOutput("dataset"),
           plotOutput("barplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    datasetInput <- reactive({
        data.frame(metric = c("age", "potential", "international_reputation"),
                   value = as.integer(c(input$age, input$potential, input$international_reputation)))
    }) 
    
    output$dataset <- renderTable(datasetInput())
    
    output$barplot <- renderPlot({
        df <- datasetInput()
        ggplot2::ggplot(df, aes(y = metric)) +
            geom_bar()
    }) 
}

# Run the application 
shinyApp(ui = ui, server = server)
