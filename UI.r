library(shiny)
require(shiny)
library(shinydashboard)
library(gbm)
library(shinythemes)
library(caret)
library(ggplot2)
library(caretEnsemble)
library(ipred)

#UI 
ui <- fluidPage(theme = shinytheme("cerulean"),
                headerPanel("Heart Disease Risk Predictor"),
  sidebarLayout(
    sidebarPanel(
      # Input widgets for user data entry
      textInput("age", "Enter Age"),
      selectInput("sex", "Select Gender Male:1/Female:0", choices = c(" ","1", "0")),
      selectInput("cp", "Select Chest Pain Typical Angina:0/Atypical Angina:1/Non-Anginal Pain:2/Asymptomatic:3", choices = c(" ","0","1", "2","3")),
      textInput("trestbps","Enter Blood Pressure in mm/HG"),
      textInput("chol","Enter Serum Cholestrol in mg/dl"),
      selectInput("fbs", "Select Blood Suger Level On Fasting>120 mg/dl Yes:1/No:0", choices = c(" ","1", "0")),
      selectInput("restecg", "Select ECG Normal:0/Having ST-T Wave :1/Showing Probable or Definite Left ventricular hypertrophyby Esters :2", choices = c(" ","0","1", "2")),
      textInput("thalach","Enter Maximum Heart Rate Achieved"),
      selectInput("exang", "Select Angina Exercise Depicting Yes:1/Depicting No:0 ", choices = c(" ","1", "0")),
      textInput("oldpeak"," Exercise induced ST-depression in relative with the state of rest Oldpeak in float value"),
      selectInput("slope", "Select Slope Up Sloping:0/Flat:1/Down Sloping:2", choices = c(" ","0","1", "2")),
      selectInput("ca", "Select The No. of Major Vessel", choices = c(" ","0", "1","2","3")),
      selectInput("thal", "Select Blood disorder called Thalassemia NULL:0/Normal Bloodflow:1/Fixed Defect:2/Reversible Defect:3", choices = c(" ","0", "1","2","3")),
      # Include other input widgets for relevant features
      actionButton("predictButton", "Predict") # Button to trigger predictions
    ),
    
    # Show result
    mainPanel(
      # Output for displaying predictions
      h4("Predicted Risk of Heart Disease:"),
      textOutput("text2")
    )
  )
)




#SERVER

server <- function(input, output) {
  loaded_model <- readRDS("C:/Users/91869/OneDrive/Desktop/R_project/my_model.rds")
  
  
  call_me <- eventReactive(input$predictButton, {
    
    age = (input$age)
    sex = (input$sex)
    cp = (input$cp)
    fbs= (input$fbs)
    trestbps = (input$trestbps)
    chol = (input$chol)
    restecg = (input$restecg)
    thalach = (input$thalach)
    exang = (input$exang)
    oldpeak = (input$oldpeak)
    slope = (input$slope)
    ca = (input$ca)
    thal = (input$thal)
    
    data_input <- data.frame(age, sex, cp, fbs , trestbps, chol, restecg, thalach, exang, oldpeak, slope, ca, thal)
    
    
    #PREPROCESS INPUT DATA
    ##REPLACE DATA WITH NEW factor data 
      data_input$age <- as.factor(data_input$age)
      data_input$sex <- as.factor(data_input$sex)
      data_input$cp <- as.factor(data_input$cp)
      data_input$fbs <- as.factor(data_input$fbs)
      data_input$trestbps <- as.factor(data_input$trestbps)
      data_input$chol <- as.factor(data_input$chol)
      data_input$restecg <- as.factor(data_input$restecg)
      data_input$thalach <- as.factor(data_input$thalach)
      data_input$exang <- as.factor(data_input$exang)
      data_input$oldpeak<- as.factor(data_input$oldpeak)
      data_input$slope <- as.factor(data_input$slope)
      data_input$ca <- as.factor(data_input$ca)
      data_input$thal <- as.factor(data_input$thal)
    
      rf_pred <- predict(loaded_model, newdata = data_input)
    return(rf_pred)
  })
  
  output$text2 <- renderText({
    result <- call_me()
    paste("The heart disease prediction is:", result)
    if (result == 1) {
      "The heart disease prediction is Positive.Based on the information provided, the model predicts a Higher risk of heart disease."
    } else {
      "The heart disease prediction is Negative.Based on the information provided, the model predicts a Lower risk of heart disease."
    }
    
  })
  
}

shinyApp(ui = ui, server = server)