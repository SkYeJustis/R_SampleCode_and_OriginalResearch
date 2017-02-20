shinyUI(pageWithSidebar(
  headerPanel("Monster Prediction Application"),
  sidebarPanel(
    sliderInput("bonelength", "Bone Length",
                min = 0.00, max = 1.00, value = 0.5,
                step = 0.05),
    sliderInput("rotflesh", "Rotting Flesh",
                min = 0.00, max = 1.00, value = 0.5,
                step = 0.05),
    sliderInput("hairlength", "Hair Length",
                min = 0.00, max = 1.00, value = 0.5,
                step = 0.05),
    sliderInput("hassoul", "Has Soul",
                min = 0.00, max = 1.00, value = 0.5,
                step = 0.05),
    selectInput("color", "What color is it?",
                choices = c("black", "blood",
                            "blue", "clear",
                            "green", "white")),
    submitButton("Predict")
  ),
  
  mainPanel(
    verbatimTextOutput("monsterpred")
  )
))