# study-aug25/app/app.R
library(shiny)

# 1) Data source (raw GitHub). If you prefer bundling a copy with the app,
#    put it at: study-aug25/app/data/df_lebe.csv  (the code will auto-use it).
DATA_URL <- "https://raw.githubusercontent.com/deanbaltiansky/left-behind/main/study-aug25/data/df_lebe.csv"

# Optional: restrict to your hand-picked continuous vars (leave empty to auto-detect)
numeric_vars_user <- c(
  "left_behind_score","culprit","any_gov","frac_systemic",
  "how","otherself","antidem","trust","antiest","elect","polparticipation",
  "trust_con","trust_gov","trust_jud","trust_sci","trust_pol","trust_news",
  "ideo_con","ideo_lib","ideo_demsoc","ideo_lbrtn","ideo_prog",
  "rep_bin","dem_bin","ind_bin",
  "age","edu_num","income_num","white_bin","man_bin"
)

load_data <- function() {
  local_path <- "data/df_lebe.csv"           # only within the app/ folder
  if (file.exists(local_path)) {
    read.csv(local_path, check.names = FALSE)
  } else {
    read.csv(DATA_URL, check.names = FALSE)
  }
}

ui <- fluidPage(
  titlePanel("Study Aug 2025 — Correlation Explorer"),
  sidebarLayout(
    sidebarPanel(
      helpText("Pick two continuous variables to explore their linear relationship."),
      selectInput("xvar", "X axis", choices = NULL),
      selectInput("yvar", "Y axis", choices = NULL)
    ),
    mainPanel(
      plotOutput("scatter", height = 420),
      tags$hr(),
      verbatimTextOutput("stats")
    )
  )
)

server <- function(input, output, session) {
  df <- load_data()
  
  # Build choices: use your list if provided, else auto-detect numeric columns
  numeric_auto <- names(df)[vapply(df, is.numeric, logical(1))]
  numeric_choices <- if (length(numeric_vars_user)) {
    intersect(numeric_vars_user, names(df))
  } else numeric_auto
  
  # Populate dropdowns
  updateSelectInput(session, "xvar", choices = numeric_choices,
                    selected = if (length(numeric_choices)) numeric_choices[1])
  updateSelectInput(session, "yvar", choices = numeric_choices,
                    selected = if (length(numeric_choices) > 1) numeric_choices[2])
  
  pair_data <- reactive({
    req(input$xvar, input$yvar)
    d <- df[, c(input$xvar, input$yvar)]
    stats::na.omit(d)
  })
  
  output$scatter <- renderPlot({
    d <- pair_data()
    validate(
      need(nrow(d) >= 3, "Not enough non-missing pairs to plot."),
      need(stats::sd(d[[1]]) > 0, "X has no variance."),
      need(stats::sd(d[[2]]) > 0, "Y has no variance.")
    )
    plot(d[[1]], d[[2]], pch = 19, xlab = input$xvar, ylab = input$yvar)
    fit <- lm(d[[2]] ~ d[[1]])
    abline(fit, lwd = 2)
  })
  
  output$stats <- renderText({
    d <- pair_data()
    if (nrow(d) < 3 || any(c(stats::sd(d[[1]]), stats::sd(d[[2]])) == 0)) {
      return("Insufficient variance or data.")
    }
    ct <- cor.test(d[[1]], d[[2]], method = "pearson")
    sprintf("Pearson r = %.3f   (p = %.3g,  n = %d)",
            unname(ct$estimate), ct$p.value, nrow(d))
  })
}

shinyApp(ui, server)
