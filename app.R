library(shiny)

ui<-fluidPage(
  titlePanel("Body Mass Index (BMI) Calculator"),
  sidebarLayout(
    sidebarPanel(
      h5("Please enter your height in centimeter and weight in kilogram"),
      numericInput(inputId="height", label="Height (cm)", value=1, min=1, max=100, step=1),
      numericInput(inputId="weight", label="Weight (kg)", value=1, min=1, max=100, step=1),
      submitButton("Submit")
    ),
    mainPanel(
      h5("Your Height (cm):"),
      verbatimTextOutput("height"),
      h5("Your Weight (kg):"),
      verbatimTextOutput("weight"),
      h5("Your BMI:"),
      verbatimTextOutput("bmi"),
      h5("Your BMI Category:"),
      verbatimTextOutput("bmicat")
    )
  )
)

bmiCalculation <- function(height, weight) {
  
  bmi <- weight / ((height/100)^2)
  return(bmi)
  
}

bmiCategory <- function(bmi) {
  
  if (is.null(bmi) || is.na(bmi)){
    return()
  }
  else if (bmi < 18.5) {
    return("Under Weight")
  }
  else if (bmi >= 18.5 & bmi <= 24.9) {
    return("Normal")
  }
  else if (bmi >= 25 & bmi <= 29.9) {
    return("Over Weight")
  }
  else if (bmi >= 30) {
    return("Obesity")
  }
  else {
    return()
  }
}

server <- function(input,output) {
  output$height <- renderPrint(input$height)
  output$weight <- renderPrint(input$weight)
  bmi <- reactive({bmiCalculation(input$height, input$weight)})
  output$bmi <- renderPrint({bmi()})
  output$bmicat <- renderPrint({bmiCategory(bmi())})
}

shinyApp(ui=ui, server=server)