# study-sep25-interactions/app/app.R
library(shiny)

numeric_vars_user <- c(
  "lebe_cont","system_R","gov","econcult","history","trajectory",
  "leftbehind_gpt","system_to_ind_gpt","gov_gpt","econ_to_cult_gpt","history_gpt","trajectory_gpt",
  "antidem","trust","antiest","elect_exp","elect_change","elect_econ","elect_cult",
  "trust_con","trust_gov","trust_jud","trust_sci","trust_pol","trust_news",
  "ideo_con","ideo_lib","ideo_demsoc","ideo_lbrtn","ideo_prog",
  "rep_bin","dem_bin","ind_bin","age","edu_num","income_num","white_bin","man_bin"
)

binary_mods <- c("rep_bin","dem_bin","ind_bin","white_bin","man_bin")

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
      tags$hr(),
      checkboxInput("show_ci", "Show 95% CI bands", TRUE),
      helpText("If moderator is binary (0/1): red=0, blue=1. Otherwise: −1 SD (red), mean (black), +1 SD (blue).")
    ),
    mainPanel(
      verbatimTextOutput("coefbox"),
      plotOutput("iplot", height = 460),
      tags$hr(),
      tags$h4("Selected variable descriptions"),
      uiOutput("ydesc"),
      uiOutput("xdesc"),
      uiOutput("zdesc")
    )
  )
)

server <- function(input, output, session) {
  df0 <- load_data(); names(df0) <- trimws(names(df0))
  var_info <- load_var_info()
  
  available <- intersect(numeric_vars_user, names(df0))
  validate(need(length(available) >= 3, "Need at least three of your selected variables present in df_lebe.csv."))
  
  lab_map <- var_info$label[match(available, var_info$var)]
  lab_map[is.na(lab_map) | !nzchar(lab_map)] <- available
  choices <- as.list(available); names(choices) <- lab_map
  
  updateSelectInput(session, "yvar", choices = choices, selected = available[1])
  updateSelectInput(session, "xvar", choices = choices, selected = available[2])
  updateSelectInput(session, "zvar", choices = choices, selected = available[3])
  
  get_label <- function(v) {
    hit <- var_info$label[match(v, var_info$var)]
    if (is.na(hit) || !nzchar(hit)) v else hit
  }
  get_desc <- function(v) {
    hit <- var_info$description[match(v, var_info$var)]
    if (is.na(hit) || !nzchar(hit)) "No description found." else hit
  }
  
  triplet <- reactive({
    req(input$yvar, input$xvar, input$zvar)
    vars <- c(input$yvar, input$xvar, input$zvar)
    validate(need(all(vars %in% names(df0)), "Pick three valid variables."))
    d <- df0[, vars, drop = FALSE]
    d[] <- lapply(d, numish)
    names(d) <- c("Y","X","Z")
    d <- stats::na.omit(d)
    validate(
      need(nrow(d) >= 5, "Not enough complete cases."),
      need(stats::sd(d$Y) > 0, "Outcome Y has no variance."),
      need(stats::sd(d$X) > 0, "Predictor X has no variance."),
      need(stats::sd(d$Z) > 0, "Moderator Z has no variance.")
    )
    d
  })
  
  fit <- reactive({ lm(Y ~ X:Z, data = triplet()) })
  
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
  
  output$iplot <- renderPlot({
    d <- triplet(); m <- fit()
    
    # X grid
    x_seq <- seq(min(d$X, na.rm=TRUE), max(d$X, na.rm=TRUE), length.out = 160)
    
    is_binary <- input$zvar %in% binary_mods
    
    if (is_binary) {
      # Ensure both 0 and 1 exist
      uniq <- sort(unique(d$Z))
      validate(need(all(c(0,1) %in% uniq), "Binary moderator needs both 0 and 1 present."))
      
      z_vals <- c(0, 1)
      z_lbls <- c("Z = 0", "Z = 1")
      cols   <- c("red","blue")
      fillc  <- sapply(cols, function(cl) grDevices::adjustcolor(cl, alpha.f = 0.4))
      lw     <- 4
      
      # Prediction grid for Z=0 and Z=1
      grid <- do.call(rbind, lapply(seq_along(z_vals), function(i) {
        data.frame(X = x_seq, Z = z_vals[i], level = i)
      }))
      pr <- predict(m, newdata = grid, se.fit = TRUE)
      grid$fit <- pr$fit; grid$se <- pr$se.fit
      
      # Axes
      xlab <- get_label(input$xvar)
      ylab <- paste0("Predicted ", get_label(input$yvar))
      if (isTRUE(input$show_ci)) {
        y_range <- range(grid$fit + 1.96*grid$se, grid$fit - 1.96*grid$se, na.rm = TRUE)
      } else {
        y_range <- range(grid$fit, na.rm = TRUE)
      }
      plot(NA, xlim = range(x_seq), ylim = y_range, xlab = xlab, ylab = ylab)
      
      for (i in seq_along(z_vals)) {
        sli <- which(grid$level == i)
        xx  <- grid$X[sli]
        mu  <- grid$fit[sli]
        se  <- grid$se[sli]
        if (isTRUE(input$show_ci)) {
          ci <- 1.96 * se
          upper <- mu + ci; lower <- mu - ci
          polygon(c(xx, rev(xx)), c(upper, rev(lower)), border = NA, col = fillc[i])
          lines(xx, upper, lty = 2, lwd = 2, col = cols[i])
          lines(xx, lower, lty = 2, lwd = 2, col = cols[i])
        }
        lines(xx, mu, lwd = lw, col = cols[i])
      }
      
      legend("topleft",
             legend = z_lbls,
             lty = 1, lwd = lw, col = cols, bty = "n",
             title = paste0("Moderator (", get_label(input$zvar), ")"))
      
    } else {
      # Continuous moderator: −1 SD / mean / +1 SD
      z_mean <- mean(d$Z, na.rm = TRUE)
      z_sd   <- stats::sd(d$Z, na.rm = TRUE)
      z_vals <- c(z_mean - z_sd, z_mean, z_mean + z_sd)
      z_lbls <- c("Z = −1 SD", "Z = mean", "Z = +1 SD")
      cols   <- c("red","black","blue")
      fillc  <- sapply(cols, function(cl) grDevices::adjustcolor(cl, alpha.f = 0.4))
      lw     <- 4
      
      grid <- do.call(rbind, lapply(seq_along(z_vals), function(i) {
        data.frame(X = x_seq, Z = z_vals[i], level = i)
      }))
      pr <- predict(m, newdata = grid, se.fit = TRUE)
      grid$fit <- pr$fit; grid$se <- pr$se.fit
      
      xlab <- get_label(input$xvar)
      ylab <- paste0("Predicted ", get_label(input$yvar))
      if (isTRUE(input$show_ci)) {
        y_range <- range(grid$fit + 1.96*grid$se, grid$fit - 1.96*grid$se, na.rm = TRUE)
      } else {
        y_range <- range(grid$fit, na.rm = TRUE)
      }
      plot(NA, xlim = range(x_seq), ylim = y_range, xlab = xlab, ylab = ylab)
      
      for (i in seq_along(z_vals)) {
        sli <- which(grid$level == i)
        xx  <- grid$X[sli]
        mu  <- grid$fit[sli]
        se  <- grid$se[sli]
        if (isTRUE(input$show_ci)) {
          ci <- 1.96 * se
          upper <- mu + ci; lower <- mu - ci
          polygon(c(xx, rev(xx)), c(upper, rev(lower)), border = NA, col = fillc[i])
          lines(xx, upper, lty = 2, lwd = 2, col = cols[i])
          lines(xx, lower, lty = 2, lwd = 2, col = cols[i])
        }
        lines(xx, mu, lwd = lw, col = cols[i])
      }
      
      legend("topleft",
             legend = z_lbls,
             lty = 1, lwd = lw, col = cols, bty = "n",
             title = paste0("Moderator (", get_label(input$zvar), ")"))
    }
  })
  
  output$ydesc <- renderUI({ tags$p(tags$strong("Outcome (Y): "), tags$em(get_desc(input$yvar))) })
  output$xdesc <- renderUI({ tags$p(tags$strong("Predictor (X): "), tags$em(get_desc(input$xvar))) })
  output$zdesc <- renderUI({ tags$p(tags$strong("Moderator (Z): "), tags$em(get_desc(input$zvar))) })
}

shinyApp(ui, server)
