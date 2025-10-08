# study-oct25/app/app.R
library(shiny)

# ---- Data sources (shinylive = local files only) ----
numeric_vars_user <- c(
  "lebe_cont","cand_rightwing","cand_change_R","cand_exp",
  "mob","soccon","lonely","selfplacement","ideo",
  "rep_bin","dem_bin","ind_bin","age","edu_num","income_num","white_bin","man_bin"
)

load_data <- function() {
  p <- "data/df_lebe.csv"                      # must be bundled with the app
  if (!file.exists(p)) stop("Missing data/df_lebe.csv in app/data/")
  read.csv(p, check.names = FALSE, stringsAsFactors = FALSE)
}

load_var_info <- function() {
  p <- "data/var_info.csv"
  if (!file.exists(p)) {
    return(data.frame(var = character(), label = character(), description = character(), 
                      stringsAsFactors = FALSE))
  }
  vi <- read.csv(p, check.names = FALSE, stringsAsFactors = FALSE)
  # normalize columns and trim whitespace
  names(vi) <- tolower(names(vi))
  if (!"var" %in% names(vi)) vi$var <- character(0)
  if (!"label" %in% names(vi)) vi$label <- vi$var
  if (!"description" %in% names(vi)) vi$description <- ""
  vi$var <- trimws(vi$var)
  vi$label <- ifelse(nzchar(trimws(vi$label)), trimws(vi$label), vi$var)
  vi$description <- trimws(vi$description)
  unique(vi[c("var","label","description")])
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
      tags$h4("Variable descriptions"),
      uiOutput("xdesc"),
      uiOutput("ydesc")
    )
  )
)

server <- function(input, output, session) {
  df <- load_data()
  var_info <- load_var_info()
  
  # helpers
  get_label <- function(v) {
    if (is.null(v) || !nzchar(v)) return("")
    hit <- var_info$label[var_info$var == v]
    if (length(hit) == 1 && nzchar(hit)) hit else v
  }
  get_desc <- function(v) {
    if (is.null(v) || !nzchar(v)) return("")
    hit <- var_info$description[var_info$var == v]
    if (length(hit) == 1 && nzchar(hit)) hit else "No description found."
  }
  
  # choices: only columns that actually exist
  available <- intersect(numeric_vars_user, names(df))
  if (length(available) < 2) {
    stop("Fewer than 2 valid numeric columns found. Check df_lebe.csv column names.")
  }
  
  # names = labels shown; values = actual column names returned
  choice_labels <- vapply(available, get_label, character(1))
  choices <- setNames(object = available, nm = choice_labels)
  
  updateSelectInput(session, "xvar", choices = choices, selected = available[1])
  updateSelectInput(session, "yvar", choices = choices, selected = available[2])
  
  # data for selected pair; coerce numeric-like characters
  pair_data <- reactive({
    req(input$xvar, input$yvar)
    vars <- c(input$xvar, input$yvar)
    # guard: both vars must exist
    vars <- intersect(vars, names(df))
    validate(need(length(vars) == 2, "Pick two valid variables."))
    
    d <- df[, vars, drop = FALSE]
    d[] <- lapply(d, function(x) if (is.character(x)) suppressWarnings(as.numeric(x)) else x)
    d <- stats::na.omit(d)
    validate(
      need(nrow(d) >= 3, "Not enough non-missing pairs to plot."),
      need(stats::sd(d[[1]]) > 0, "X has no variance."),
      need(stats::sd(d[[2]]) > 0, "Y has no variance.")
    )
    d
  })
  
  output$scatter <- renderPlot({
    d <- pair_data()
    plot(
      d[[1]], d[[2]], pch = 19,
      xlab = get_label(input$xvar),
      ylab = get_label(input$yvar)
    )
    fit <- lm(d[[2]] ~ d[[1]])
    abline(fit, lwd = 2)
  })
  
  output$stats <- renderText({
    d <- pair_data()
    ct <- cor.test(d[[1]], d[[2]], method = "pearson")
    sprintf("Pearson r = %.3f   (p = %.3g,  n = %d)",
            unname(ct$estimate), ct$p.value, nrow(d))
  })
  
  # show ONLY descriptions under the plot
  output$xdesc <- renderUI({
    req(input$xvar)
    tags$p(tags$strong("X: "), tags$em(get_desc(input$xvar)))
  })
  output$ydesc <- renderUI({
    req(input$yvar)
    tags$p(tags$strong("Y: "), tags$em(get_desc(input$yvar)))
  })
}

shinyApp(ui, server)
