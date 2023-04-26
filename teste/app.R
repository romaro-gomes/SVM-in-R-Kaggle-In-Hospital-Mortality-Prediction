library(shiny)

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            numericInput('A', label = 'A', value = 55),
            numericInput('B', label = 'B', value = 90)),
        
        mainPanel(tableOutput('barplot')))
)

server <- function(input, output, session){
    
    output$barplot <- renderTable({
        
        data <- data.frame(cat = c('A', 'B'),
                           val = c(input$A, input$B))
        
        data
    })
}

shinyApp(ui, server)