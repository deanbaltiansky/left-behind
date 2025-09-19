# study-sep25-lm-table/app/app.R
library(shiny)

# ---- Your curated list for Outcome/Predictor/Moderator ----
numeric_vars_user <- c(
  "lebe_cont","system_R","gov","econcult","history","trajectory",
  "leftbehind_gpt","system_to_ind_gpt","gov_gpt","econ_to_cult_gpt","history_gpt","trajectory_gpt",
  "mrl","mpcs",
  "antidem","trust","antiest","elect_exp","elect_change","elect_econ","elect_cult",
  "trust_con","trust_gov","trust_jud","trust_sci","trust_pol","trust_news",
  "ideo_con","ideo_lib","ideo_demsoc","ideo_lbrtn","ideo_prog",
  "rep_bin","dem_bin","ind_bin","age","edu_num","income_num","white_bin","man_bin"
)

# ---- Data loaders (local files: shinylive-safe) ----
load_data <- function() {
  p <- "data/df_lebe.csv"
  if (!file.exists(p)) stop("Missing data/df_lebe.csv in app/data/")
  read.csv(p, check.names = FALSE, stringsAsFactors = FALSE)
}
load_var_info <- function() {
  p <- "data/var_info.csv"
  if (!file.exists(p)) return(data.frame(var=character(),label=character(),description=character()))
  vi <- read.csv(p, check.names = FALSE, stringsAsFactors = FALSE)
  names(vi) <- tolower(names(vi))
  if (!"var" %in% names(vi)) vi$var <- character(0)
  if (!"label" %in% names(vi)) vi$label <- vi$var
  if (!"description" %in% names(vi)) vi$description <- ""
  vi$var <- trimws(vi$var); vi$label <- trimws(vi$label); vi$description <- trimws(vi$description)
  unique(vi[c("var","label","description")])
}

# ---- Helpers ----
is_numlike <- function(x) {
  if (is.numeric(x)) return(TRUE)
  if (!is.character(x)) return(FALSE)
  ok <- suppressWarnings(!is.na(as.numeric(x)))
  mean(ok, na.rm = TRUE) > 0.9
}
coerce_for_lm <- function(df) {
  # Numeric-like -> numeric; everything else -> factor (safe for lm)
  for (nm in names(df)) {
    if (is_numlike(df[[nm]])) {
      df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
    } else if (!is.factor(df[[nm]])) {
      df[[nm]] <- factor(trimws(as.character(df[[nm]])))
    }
  }
  df
}

ui <- fluidPage(
  titlePanel("Study Sep 2025 — Linear Model Table"),
  sidebarLayout(
    sidebarPanel(
      helpText("Build a linear model and view a tidy coefficient table."),
      selectInput("yvar", "Outcome (Y)", choices = NULL),
      selectInput("xvar", "Predictor (X)", choices = NULL),
      selectInput("zvar", "Moderator (Z) — optional", choices = NULL),
      selectizeInput("controls", "Controls (0+)", choices = NULL, multiple = TRUE,
                     options = list(plugins = list("remove_button"), placeholder = "Add any variables")),
      tags$hr(),
      helpText("Model:"),
      verbatimTextOutput("formtxt")
    ),
    mainPanel(
      tableOutput("lm_table"),
      tags$hr(),
      tags$h4("Notes"),
      tags$p("• Term labels come from var_info.csv when available."),
      tags$p("• If a moderator is chosen, the model includes main effects and the interaction (X * Z).")
    )
  )
)

server <- function(input, output, session) {
  df0 <- load_data(); names(df0) <- trimws(names(df0))
  var_info <- load_var_info()
  
  # Choices for Y/X/Z from your curated list (present in data)
  available_core <- intersect(numeric_vars_user, names(df0))
  validate(need(length(available_core) >= 2, "At least two of your selected variables must be present."))
  
  # Labels for curated vars
  lab_core <- var_info$label[match(available_core, var_info$var)]
  lab_core[is.na(lab_core) | !nzchar(lab_core)] <- available_core
  choices_core <- as.list(available_core); names(choices_core) <- lab_core
  
  updateSelectInput(session, "yvar", choices = choices_core, selected = available_core[1])
  updateSelectInput(session, "xvar", choices = choices_core, selected = available_core[2])
  updateSelectInput(session, "zvar", choices = c("None" = "", choices_core), selected = "")
  
  # Controls from the entire dataset (excluding current Y/X/Z dynamically)
  observe({
    cur_exclude <- unique(c(input$yvar, input$xvar, input$zvar))
    all_controls <- setdiff(names(df0), cur_exclude[cur_exclude != ""])
    # Nice labels for controls too (fallback to var name)
    lab_all <- var_info$label[match(all_controls, var_info$var)]
    lab_all[is.na(lab_all) | !nzchar(lab_all)] <- all_controls
    ctrl_choices <- as.list(all_controls); names(ctrl_choices) <- lab_all
    updateSelectizeInput(session, "controls", choices = ctrl_choices, server = TRUE)
  })
  
  # Pretty label lookup (handles interactions like "a:b" => "Label(a) × Label(b)")
  term_label <- function(term) {
    if (term == "(Intercept)") return("Intercept")
    parts <- strsplit(term, ":", fixed = TRUE)[[1]]
    labs <- var_info$label[match(parts, var_info$var)]
    labs[is.na(labs) | !nzchar(labs)] <- parts
    if (length(parts) > 1) paste(labs, collapse = " × ") else labs
  }
  
  # Build formula string and model
  model_spec <- reactive({
    req(input$yvar, input$xvar)
    y <- input$yvar
    x <- input$xvar
    z <- input$zvar
    ctrls <- input$controls
    rhs <- if (nzchar(z)) paste(c(x, z, paste0(x, ":", z), ctrls), collapse = " + ")
    else paste(c(x, ctrls), collapse = " + ")
    as.formula(paste(y, "~", rhs))
  })
  
  output$formtxt <- renderText({
    f <- model_spec()
    paste(capture.output(f), collapse = "\n")
  })
  
  lm_fit <- reactive({
    f <- model_spec()
    d <- df0
    # Keep only referenced columns to reduce coercion footprint
    vars_needed <- all.vars(f)
    d <- d[, intersect(vars_needed, names(d)), drop = FALSE]
    d <- coerce_for_lm(d)
    lm(f, data = d)
  })
  
  output$lm_table <- renderTable({
    fit <- lm_fit()
    sm  <- summary(fit)
    co  <- sm$coefficients
    rn  <- rownames(co)
    df  <- sm$df[2]  # residual df
    
    # Build result frame
    out <- data.frame(
      term     = vapply(rn, term_label, character(1)),
      beta     = unname(co[, "Estimate"]),
      t        = unname(co[, "t value"]),
      df       = rep(df, length(rn)),
      p_value  = unname(co[, "Pr(>|t|)"]),
      stringsAsFactors = FALSE
    )
    
    # Formatting
    out$beta    <- sprintf("%.4f", out$beta)
    out$t       <- sprintf("%.3f", out$t)
    out$df      <- as.integer(out$df)
    out$p_value <- ifelse(out$p_value < .001, "< .001", sprintf("%.3f", out$p_value))
    
    out
  }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "m")
}

shinyApp(ui, server)
