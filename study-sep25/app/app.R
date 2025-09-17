# study-sep25/app/app.R
library(shiny)

# ---- Data sources ----
DATA_URL <- "https://raw.githubusercontent.com/deanbaltiansky/left-behind/main/study-sep25/data/df_lebe.csv"
VARINFO_URL <- "https://raw.githubusercontent.com/deanbaltiansky/left-behind/main/study-sep25/data/var_info.csv"

numeric_vars_user <- c(
  "lebe_cont","system_R","gov","econcult","history","trajectory",
  "leftbehind_gpt","system_to_ind_gpt","gov_gpt","econ_to_cult_gpt","history_gpt","trajectory_gpt",
  "antidem","trust","antiest","elect_exp","elect_change","elect_econ","elect_cult",
  "trust_con","trust_gov","trust_jud","trust_sci","trust_pol","trust_news",
  "ideo_con","ideo_lib","ideo_demsoc","ideo_lbrtn","ideo_prog",
  "rep_bin","dem_bin","ind_bin",
  "age","edu_num","income_num","white_bin","man_bin"
)

load_data <- function() {
  local_path <- "data/df_lebe.csv"  # relative to app/ folder
  if (file.exists(local_path)) {
    read.csv(local_path, check.names = FALSE)
  } else {
    read.csv(DATA_URL, check.names = FALSE)
  }
}

load_var_info <- function() {
  # var_info.csv should have at least: var,label,description
  local_path <- "data/var_info.csv"
  if (file.exists(local_path)) {
    read.csv(local_path, check.names = FALSE, stringsAsFactors = FALSE)
  } else {
    # If you don’t want a fallback, just return an empty data.frame here.
    tryCatch(
      read.csv(VARINFO_URL, check.names = FALSE, stringsAsFactors = FALSE),
      error = function(e) data.frame(var=character(), label=character(), description=character(), stringsAsFactors = FALSE)
    )
  }
}

ui <- fluidPage(
  titlePanel("Study Sep 2025 — Correlations"),
  sidebarLayout(
    sidebarPanel(
      helpText("Pick two continuous variables to explore their linear relationship."),
      selectInput("xvar", "X axis", choices = NULL),
      selectInput("yvar", "Y axis", choices = NULL)
    ),
    mainPanel(
      plotOutput("scatter", height = 420),
      tags$hr(),
      verbatimTextOutput("stats"),
      tags$hr(),
      tags$h4("Variable info"),
      uiOutput("xinfo"),
      uiOutput("yinfo")
    )
  )
)

server <- function(input, output, session) {
  df <- load_data()
  var_info <- load_var_info()
  
  # Helpers to get label/description; fallback to var name if unknown
  get_label <- function(var) {
    if (is.null(var) || !nchar(var)) return("")
    hit <- var_info$label[var_info$var == var]
    if (length(hit) == 1 && nzchar(hit)) hit else var
  }
  get_desc <- function(var) {
    if (is.null(var) || !nchar(var)) return("")
    hit <- var_info$description[var_info$var == var]
    if (length(hit) == 1 && nzchar(hit)) hit else "No description found."
  }
  
  # Build choices: use your list if provided, else auto-detect numeric columns
  numeric_auto <- names(df)[vapply(df, is.numeric, logical(1))]
  numeric_choices_raw <- if (length(numeric_vars_user)) {
    intersect(numeric_vars_user, names(df))
  } else numeric_auto
  
  # Map pretty labels -> actual var names
  pretty <- setNames(nm = numeric_choices_raw, object = vapply(numeric_choices_raw, get_label, character(1)))
  # populate dropdowns
  updateSelectInput(session, "xvar", choices = pretty,
                    selected = if (length(numeric_choices_raw)) numeric_choices_raw[1])
  updateSelectInput(session, "yvar", choices = pretty,
                    selected = if (length(numeric_choices_raw) > 1) numeric_choices_raw[2])
  
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
    plot(d[[1]], d[[2]], pch = 19,
         xlab = get_label(input$xvar), ylab = get_label(input$yvar))
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
  
  output$xinfo <- renderUI({
    req(input$xvar)
    tagList(
      tags$b("X axis:"), tags$span(get_label(input$xvar)), tags$br(),
      tags$em(get_desc(input$xvar))
    )
  })
  output$yinfo <- renderUI({
    req(input$yvar)
    tagList(
      tags$b("Y axis:"), tags$span(get_label(input$yvar)), tags$br(),
      tags$em(get_desc(input$yvar))
    )
  })
}

shinyApp(ui, server)
