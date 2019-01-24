library(shiny)

ui <- fluidPage(
  
  sliderInput(inputId = "num", 
              label = "Choose a number", 
              value = 25, min = 1, max = 100),
  plotOutput("hist")
)

server <- function(input, output) {
  nums <- rnorm(100)
  output$hist <- renderPlot({
    hist(nums[0:input$num])
  })
}

shinyApp(ui = ui, server = server)