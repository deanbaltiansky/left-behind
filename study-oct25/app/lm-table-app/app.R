# study-oct25/app/lm-table-app/app.R
library(shiny)

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---- Data loaders ----
load_data <- function() {
  p <- "data/df_lebe_elg.csv"
  if (!file.exists(p)) stop("Missing data/df_lebe_elg.csv in study-oct25/app/lm-table-app/data/")
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
  titlePanel("Left-Behind — Linear Model Table"),
  sidebarLayout(
    sidebarPanel(
      helpText("Build a linear model and view a tidy coefficient table."),
      selectInput("xvar", "Predictor (X)", choices = NULL),
      selectInput("yvar", "Outcome (Y)", choices = NULL),
      selectInput("zvar", "Moderator (Z) — optional", choices = NULL),
      selectizeInput(
        "controls", "Controls (0+)", choices = NULL, multiple = TRUE,
        options = list(
          plugins = list("remove_button"),
          persist = TRUE,
          openOnFocus = TRUE,
          closeAfterSelect = FALSE,
          placeholder = "Add any variables"
        )
      ),
      tags$hr(),
      helpText("Model:"),
      verbatimTextOutput("formtxt")
    ),
    mainPanel(
      tableOutput("lm_table"),
      plotOutput("viz", height = 420),
      tags$hr(),
      tags$h4("Selected variable info"),
      tableOutput("var_info_table"),
      tags$hr(),
      tags$h4("Notes"),
      tags$p("• Only variables listed in var_info.csv (and present in the data) are available."),
      tags$p("• If a moderator is chosen, the model includes main effects and the interaction (X × Z)."),
      uiOutput("mod_note")
    )
  )
)

server <- function(input, output, session) {
  # Load once
  df0 <- load_data(); names(df0) <- trimws(names(df0))
  var_info <- load_var_info()
  
  # ---- Your binary indicators for special plotting behavior ----
  bin_vars <- c("rep_bin","dem_bin","ind_bin","white_bin","man_bin")
  
  # label/description helpers
  lab_of <- function(v) {
    if (!nzchar(v)) return("")
    lb <- var_info$label[match(v, var_info$var)]
    ifelse(is.na(lb) | !nzchar(lb), v, lb)
  }
  get_desc <- function(v) {
    if (!nzchar(v)) return("")
    ds <- var_info$description[match(v, var_info$var)]
    ifelse(is.na(ds) | !nzchar(ds), "", ds)
  }
  
  # Populate inputs inside a reactive context (prevents blank dropdowns)
  observe({
    listed <- unique(trimws(var_info$var))
    available_all <- intersect(names(df0), listed)
    
    if (length(available_all) < 2) {
      showNotification(
        "Need at least two variables present in both df_lebe_elg.csv and var_info.csv.",
        type = "error", duration = NULL
      )
      updateSelectInput(session, "xvar", choices = list())
      updateSelectInput(session, "yvar", choices = list())
      updateSelectInput(session, "zvar", choices = c("None" = ""))
      return()
    }
    
    labs <- vapply(available_all, lab_of, character(1))
    choices_all <- as.list(available_all); names(choices_all) <- labs
    
    updateSelectInput(session, "xvar", choices = choices_all, selected = available_all[1])
    updateSelectInput(session, "yvar", choices = choices_all, selected = available_all[min(2, length(available_all))])
    updateSelectInput(session, "zvar", choices = c("None" = "", choices_all), selected = "")
    
    # Controls pool updates dynamically as user picks x/y/z
    prev_ctrl_pool <- reactiveVal(character(0))
    observe({
      cur_exclude <- unique(c(input$yvar, input$xvar, input$zvar))
      cur_exclude <- cur_exclude[nzchar(cur_exclude)]
      ctrl_pool <- setdiff(available_all, cur_exclude)
      
      if (identical(ctrl_pool, prev_ctrl_pool())) return(NULL)
      
      lab_ctrl <- vapply(ctrl_pool, lab_of, character(1))
      ctrl_choices <- as.list(ctrl_pool); names(ctrl_choices) <- lab_ctrl
      keep <- intersect(isolate(input$controls) %||% character(0), ctrl_pool)
      
      freezeReactiveValue(input, "controls")
      updateSelectizeInput(session, "controls",
                           choices = ctrl_choices,
                           selected = keep,
                           server = TRUE)
      prev_ctrl_pool(ctrl_pool)
    })
  })
  
  # Model formula
  model_spec <- reactive({
    req(input$yvar, input$xvar)
    y <- input$yvar; x <- input$xvar; z <- input$zvar; ctrls <- input$controls
    rhs <- if (nzchar(z)) paste(c(x, z, paste0(x, ":", z), ctrls), collapse = " + ")
    else paste(c(x, ctrls), collapse = " + ")
    as.formula(paste(y, "~", rhs))
  })
  
  output$formtxt <- renderText({
    paste(capture.output(model_spec()), collapse = "\n")
  })
  
  # Fit model
  lm_fit <- reactive({
    d <- df0
    vars_needed <- all.vars(model_spec())
    d <- d[, intersect(vars_needed, names(d)), drop = FALSE]
    d <- coerce_for_lm(d)
    lm(model_spec(), data = d, na.action = na.omit)
  })
  
  # Table
  output$lm_table <- renderTable({
    sm <- summary(lm_fit()); co <- sm$coefficients; rn <- rownames(co); rdf <- sm$df[2]
    data.frame(
      term     = vapply(rn, function(tt) {
        if (tt == "(Intercept)") return("Intercept")
        parts <- strsplit(tt, ":", fixed = TRUE)[[1]]
        lbls  <- vapply(parts, lab_of, character(1))
        if (length(parts) > 1) paste(lbls, collapse = " × ") else lbls
      }, character(1)),
      beta     = sprintf("%.4f", unname(co[, "Estimate"])),
      t        = sprintf("%.3f", unname(co[, "t value"])),
      df       = as.integer(rdf),
      p_value  = ifelse(unname(co[, "Pr(>|t|)"]) < .001, "< .001",
                        sprintf("%.3f", unname(co[, "Pr(>|t|)"]))),
      stringsAsFactors = FALSE
    )
  })
  
  # Viz
  output$viz <- renderPlot({
    fit <- lm_fit()
    d_fit <- model.frame(fit)
    x <- input$xvar; y <- input$yvar; z <- input$zvar
    
    lwd_line <- 2.6
    col_blue <- "blue"; col_red <- "red"; col_green <- "green"; col_gray <- "darkgray"
    pt_col <- rgb(0,0,0,0.12)
    
    op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE)
    par(xpd = NA); par(mar = par("mar") + c(2,0,0,0))
    
    typical_val <- function(v) {
      vv <- d_fit[[v]]
      if (is.numeric(vv)) mean(vv, na.rm = TRUE)
      else if (is.factor(vv)) factor(names(sort(table(vv), decreasing=TRUE))[1], levels=levels(vv))
      else names(sort(table(vv), decreasing=TRUE))[1]
    }
    rep_n <- function(val,n) if (is.factor(val)) factor(rep(as.character(val),n),levels=levels(val)) else rep(val,n)
    
    xv <- d_fit[[x]]; yv <- d_fit[[y]]
    if (!is.numeric(xv)) {
      x_fac <- factor(xv); x_num <- as.numeric(x_fac)
      plot(jitter(x_num,0.2), yv, pch=16, col=pt_col,
           xaxt="n", xlab=lab_of(x), ylab=lab_of(y), main="Mean outcome by X (X is non-numeric)")
      axis(1, at=seq_along(levels(x_fac)), labels=levels(x_fac))
      pts_mean <- tapply(yv, x_fac, mean, na.rm=TRUE)
      points(seq_along(pts_mean), pts_mean, pch=19, cex=1.2, col=col_blue)
      return()
    }
    
    x_seq <- seq(min(xv,na.rm=TRUE), max(xv,na.rm=TRUE), length.out=100); N <- length(x_seq)
    nd_row <- lapply(names(d_fit), typical_val); names(nd_row) <- names(d_fit)
    nd <- as.data.frame(nd_row, stringsAsFactors=FALSE)[rep(1,N),,drop=FALSE]
    for (nm in names(d_fit)) nd[[nm]] <- rep_n(nd[[nm]][1], N)
    nd[[x]] <- x_seq
    
    use_z <- nzchar(z)
    z_in_fit <- if (use_z) d_fit[[z]] else NULL
    z_is_numeric <- use_z && is.numeric(z_in_fit)
    z_is_binary <- use_z && (z %in% bin_vars)
    
    plot(xv, yv, pch=16, col=pt_col,
         xlab=lab_of(x), ylab=lab_of(y),
         main=if (use_z&&z_is_numeric) paste0("Simple slopes at ",lab_of(z)," = {−1 SD, mean, +1 SD}")
         else if (use_z&&z_is_binary) paste0("Effect of ",lab_of(x)," on ",lab_of(y)," by ",lab_of(z)," (0 vs 1)")
         else "Model-implied trend")
    
    if (use_z && z_is_binary) {
      is_factor_z <- is.factor(z_in_fit); levs <- if (is_factor_z) levels(z_in_fit) else NULL
      leg <- c(); leg_cols <- c(col_gray,col_blue)
      for (i in 1:2) {
        nd_i <- nd
        if (is_factor_z) {
          val <- if (all(c("0","1")%in%levs)) c("0","1")[i] else levs[i]
          nd_i[[z]] <- factor(rep(val,N), levels=levs)
          leg[i] <- paste(lab_of(z),"=",val)
        } else {
          val <- i-1
          nd_i[[z]] <- rep(val,N)
          leg[i] <- paste(lab_of(z),"=",val)
        }
        yhat <- predict(fit,newdata=nd_i)
        lines(x_seq,yhat,lwd=lwd_line,col=leg_cols[i])
      }
      legend("bottom", inset=-0.18, xpd=NA, horiz=TRUE,
             legend=leg, lty=1, lwd=lwd_line, col=leg_cols, bty="n")
      output$mod_note <- renderUI(HTML("&nbsp;"))
      
    } else if (use_z && z_is_numeric) {
      z_mean <- mean(z_in_fit,na.rm=TRUE); z_sd <- sd(z_in_fit,na.rm=TRUE)
      z_vals <- c(z_mean-z_sd, z_mean, z_mean+z_sd)
      leg <- c(paste(lab_of(z),"= -1 SD"),paste(lab_of(z),"= Mean"),paste(lab_of(z),"= +1 SD"))
      leg_cols <- c(col_red,col_blue,col_green)
      for (i in 1:3) {
        nd_i <- nd; nd_i[[z]] <- rep(z_vals[i],N)
        yhat <- predict(fit,newdata=nd_i)
        lines(x_seq,yhat,lwd=lwd_line,col=leg_cols[i])
      }
      legend("bottom", inset=-0.18, xpd=NA, horiz=TRUE,
             legend=leg, lty=1, lwd=lwd_line, col=leg_cols, bty="n")
      output$mod_note <- renderUI(HTML("&nbsp;"))
    } else {
      yhat <- predict(fit,newdata=nd)
      lines(x_seq,yhat,lwd=lwd_line,col=col_blue)
      output$mod_note <- renderUI(HTML("&nbsp;"))
    }
  })
  
  # Selected variable info
  output$var_info_table <- renderTable({
    x <- input$xvar; y <- input$yvar; z <- input$zvar
    rows <- list(
      c(role="Predictor (X)", var=x, label=lab_of(x), description=get_desc(x)),
      c(role="Outcome (Y)", var=y, label=lab_of(y), description=get_desc(y))
    )
    if (nzchar(z)) rows <- c(rows, list(c(role="Moderator (Z)", var=z, label=lab_of(z), description=get_desc(z))))
    as.data.frame(do.call(rbind, rows), stringsAsFactors=FALSE)
  })
}

shinyApp(ui, server)
