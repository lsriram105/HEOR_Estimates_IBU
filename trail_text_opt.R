library(shiny)
library(reticulate)

# Load transformers (ensure Python environment is set up)
transformers <- import("transformers")
tokenizer <- transformers$AutoTokenizer$from_pretrained("distilgpt2")
model <- transformers$AutoModelForCausalLM$from_pretrained("distilgpt2")

ui <- fluidPage(
  titlePanel("Chart with AI Explanation"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins", "Number of bins:", min = 5, max = 50, value = 30)
    ),
    mainPanel(
      plotOutput("barChart"),
      textOutput("chartExplanation")
    )
  )
)

server <- function(input, output) {
  data <- reactive({ faithful$waiting })  # Example dataset
  
  output$barChart <- renderPlot({
    hist(data(), bins = input$bins, main = "Histogram of Waiting Times",
         xlab = "Waiting Time (minutes)", col = "blue")
  })
  
  output$chartExplanation <- renderText({
    chart_desc <- paste("A histogram of waiting times with", input$bins, "bins.")
    generate_chart_explanation(chart_desc)
  })
}

shinyApp(ui = ui, server = server)