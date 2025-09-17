# study-sep25-interactions/app/app.R
library(shiny)

# ---- Your curated variable list (same as correlation explorer) ----
numeric_vars_user <- c(
  "lebe_cont","system_R","gov","econcult","history","trajectory",
  "leftbehind_gpt","system_to_ind_gpt","gov_gpt","econ_to_cult_gpt","history_gpt","trajectory_gpt",
  "antidem","trust","antiest","elect_exp","elect_change","elect_econ","elect_cult",
  "trust_con","trust_gov","trust_jud","trust_sci","trust_pol","trust_news",
  "ideo_con","ideo_lib","ideo_demsoc","ideo_lbrtn","ideo_prog",
  "rep_bin","dem_bin","ind_bin","age","edu_num","income_num","white_bin","man_bin"
)

# ---- Data loaders (shinylive/browser-safe: local files only) ----
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
numish <- function(x) if (is.numeric(x)) x else if (is.character(x)) suppressWarnings(as.numeric(x)) else x

ui <- fluidPage(
  titlePanel("Study Sep 2025 — Interaction Explorer (Y ~ X:Z)"),
  sidebarLayout(
    sidebarPanel(
      helpText("Pick outcome (Y), predictor (X), and moderator (Z). Model: lm(Y ~ X:Z)"),
      selectInput("yvar", "Outcome (Y)", choices = NULL),
      selectInput("xvar", "Predictor (X)", choices = NULL),
      selectInput("zvar", "Moderator (Z)", choices = NULL),
      checkboxInput("show_ci", "Show 95% CI in plot (when available)", TRUE),
      helpText("Tip: use continuous / numeric-like variables.")
    ),
    mainPanel(
      verbatimTextOutput("coefbox"),
      plotOutput("iplot", height = 460),
      tags$hr(),
      tags$small("Plot uses interactions::interact_plot() when available; otherwise a base-R fallback.")
    )
  )
)

server <- function(input, output, session) {
  df0 <- load_data(); names(df0) <- trimws(names(df0))
  var_info <- load_var_info()
  
  # Keep only your curated vars that are present
  available <- intersect(numeric_vars_user, names(df0))
  validate(need(length(available) >= 3, "Need at least three of your selected variables present in df_lebe.csv."))
  
  # Labels from var_info.csv (fallback to var name)
  lab_map <- var_info$label[match(available, var_info$var)]
  lab_map[is.na(lab_map) | !nzchar(lab_map)] <- available
  choices <- as.list(available); names(choices) <- lab_map
  
  updateSelectInput(session, "yvar", choices = choices, selected = available[1])
  updateSelectInput(session, "xvar", choices = choices, selected = available[2])
  updateSelectInput(session, "zvar", choices = choices, selected = available[3])
  
  # Data triplet, numeric-coerced, NA-omitted
  triplet <- reactive({
    req(input$yvar, input$xvar, input$zvar)
    vars <- c(input$yvar, input$xvar, input$zvar)
    validate(need(all(vars %in% names(df0)), "Pick three valid variables."))
    d <- df0[, vars, drop = FALSE]
    d[] <- lapply(d, numish); names(d) <- c("Y","X","Z")
    d <- stats::na.omit(d)
    validate(
      need(nrow(d) >= 5, "Not enough complete cases."),
      need(stats::sd(d$Y) > 0, "Outcome Y has no variance."),
      need(stats::sd(d$X) > 0, "Predictor X has no variance."),
      need(stats::sd(d$Z) > 0, "Moderator Z has no variance.")
    )
    d
  })
  
  # Fit: interaction-only (per your spec)
  fit <- reactive({ lm(Y ~ X:Z, data = triplet()) })
  
  # (1) Interaction term beta / t / p
  output$coefbox <- renderText({
    sm <- summary(fit())
    rn <- rownames(sm$coefficients)
    if (!"X:Z" %in% rn) return("Could not find interaction coefficient X:Z.")
    b <- sm$coefficients["X:Z","Estimate"]
    t <- sm$coefficients["X:Z","t value"]
    p <- sm$coefficients["X:Z","Pr(>|t|)"]
    n <- fit()$df.residual + nrow(sm$coefficients)
    sprintf("Interaction (X:Z): beta = %.4f, t = %.3f, p = %.3g (n = %d)", b, t, p, n)
  })
  
  # (2) Interaction plot: prefer interactions::interact_plot()
  output$iplot <- renderPlot({
    d <- triplet(); m <- fit()
    if (requireNamespace("interactions", quietly = TRUE)) {
      interactions::interact_plot(
        m,
        pred = "X",
        modx = "Z",
        interval = isTRUE(input$show_ci)
      )
      return(invisible())
    }
    # Fallback base-R interaction curve (3 moderator quantiles)
    x_seq <- seq(min(d$X), max(d$X), length.out = 80)
    z_vals <- as.numeric(stats::quantile(d$Z, c(0.2,0.5,0.8)))
    grid <- do.call(rbind, lapply(z_vals, function(zv) data.frame(X = x_seq, Z = zv)))
    pr <- predict(m, newdata = grid, se.fit = TRUE)
    grid$fit <- pr$fit; grid$se <- pr$se.fit
    plot(NA, xlim = range(x_seq), ylim = range(grid$fit, na.rm=TRUE),
         xlab = names(choices)[match(input$xvar, unlist(choices))],
         ylab = paste0("Predicted ", names(choices)[match(input$yvar, unlist(choices))]))
    cols <- c("black","gray40","gray60")
    for (i in seq_along(z_vals)) {
      sli <- ((i-1)*length(x_seq)+1):(i*length(x_seq))
      lines(x_seq, grid$fit[sli], lwd = 2, col = cols[i])
      if (isTRUE(input$show_ci)) {
        ci <- 1.96*grid$se[sli]
        lines(x_seq, grid$fit[sli]+ci, lty=2, col=cols[i])
        lines(x_seq, grid$fit[sli]-ci, lty=2, col=cols[i])
      }
    }
    legend("topleft",
           legend = paste0("Z = ", round(z_vals,2)),
           lty = 1, lwd = 2, col = cols, bty = "n",
           title = names(choices)[match(input$zvar, unlist(choices))])
  })
}

shinyApp(ui, server)
