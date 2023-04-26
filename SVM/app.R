library(here)
library(tidyverse)
library(e1071)
library(shiny)
library(shinythemes)
# Load objects
data=readRDS(here('object/data_to_model.R'))
model=readRDS(here('object/svm_model_simples.R'))
data |> str()
summary(data)
# App
ui = fluidPage(
    theme = shinytheme("cerulean"),
    titlePanel("SVM Model for prediction Death in Hospital"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput(
                        "gendera",
                        label = "Sex of Patient",
                        choices =c("Male"="0",
                                   "Female"="1"),
                        multiple = FALSE,
                        selected = NULL
                        ),
            selectInput(
                        "hypertensive",
                        label = "Hipertensive?",
                        choices =c("Yes"="0",
                                   "No"="1"),
                        multiple = FALSE,
                        selected = NULL
                        ),
            selectInput(
                        "atrialfibrillation",
                        label = "Arrhythmia?",
                        choices =c("Yes"="0",
                                   "No"="1"),
                        multiple = FALSE,
                        selected = NULL
                        ),
            selectInput(
                        "hyperlipemia",
                        label = "Dyslipidemia?",
                        choices =c("Yes"="0",
                                   "No"="1"),
                        multiple = FALSE,
                        selected = NULL
                        ),
            sliderInput(
                        "age", "Age of Patientes",
                        min = min(data$age) |> round(2) ,
                        max=max(data$age) |> round(2),
                        value = median(data$age) |> round(2)
                        ),
            sliderInput(
                        "bmi", "BMI",
                        min = min(data$BMI) |> round(2),
                        max=max(data$BMI) |> round(2),
                        value = median(data$BMI) |> round(2) 
                        ),
            sliderInput(
                        "heart_rate", "Heart Rate",
                        min = min(data$heart_rate) |> round(2) ,
                        max=max(data$heart_rate) |> round(2),
                        value = median(data$heart_rate) |> round(2)
                        ),
            sliderInput(
                        "ph", "pH of Blood",
                        min = min(data$PH) |> round(2),
                        max=max(data$PH)+10 |> round(2),
                        value = median(data$PH) |> round(2)
                        ),
            sliderInput("respiratory_rate",
                        "Respiratory Rate",
                        min = min(data$Respiratory_rate) |> round(2),
                        max=max(data$Respiratory_rate) |> round(2),
                        value = median(data$Respiratory_rate) |> round(2)
                        ),
            actionButton("enter","Enter")
        ),
        mainPanel(
            tableOutput('table'),
            verbatimTextOutput('pred')
            
        )
    )
    
)
    

server <- function(input, output, session) {
    df=reactive({
        data = data.frame(gendera=input$gendera,
                          hypertensive=input$hypertensive,
                          atrialfibrillation=input$atrialfibrillation,
                          Hyperlipemia=input$hyperlipemia,
                          age=input$age,
                          BMI=input$bmi,
                          heart_rate=input$heart_rate,
                          PH=input$ph,
                          Respiratory_rate=input$respiratory_rate)
    }
    )
    
    press =eventReactive(
                            input$enter, {
                                if(input$enter >0){
                                    df()
                                   
                                }}
                         )
    
    
    output$table = renderTable(press())
    
    output$pred = renderText({
        pred = predict(model, df())
        attr(pred,"probabilities")
        
    })
     
}


# Run the application 
shinyApp(ui = ui, server = server)
