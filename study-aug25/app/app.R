library(shiny)

ui <- fluidPage(
  titlePanel("Study Aug 2025 — Demo App"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("n", "Points:", min = 10, max = 500, value = 100)
    ),
    mainPanel(
      plotOutput("p")
    )
  )
)

server <- function(input, output, session) {
  output$p <- renderPlot({
    set.seed(1); x <- rnorm(input$n); y <- rnorm(input$n)
    plot(x, y, pch = 19)
  })
}

shinyApp(ui, server)
