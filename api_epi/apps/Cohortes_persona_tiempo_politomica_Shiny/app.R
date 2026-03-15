library(shiny)

# --- UI Module ---
cohortes_politomica_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("title_ui")),
    sidebarLayout(
      sidebarPanel(
        h4(uiOutput(ns("h_params"))),
        textInput(ns("lexpo"), "Levels of Exposure (comma separated):", "0, 1, 2, 3.5"),
        textInput(ns("a"), "Cases (comma separated):", "82, 140, 150, 141"),
        textInput(ns("t"), "Person-Time (comma separated):", "31966, 49672, 44112, 37545"),
        radioButtons(ns("rc"), "Reference Category:", choices = c("First (F)" = "F", "Last (L)" = "L"), selected = "F"),
        # Removed calculate button for real-time update
        br(), br()
      ),
      mainPanel(
        h4(uiOutput(ns("h_res"))),
        verbatimTextOutput(ns("results"))
      )
    ),
    div(
      class = "print-template",
      uiOutput(ns("print_view"))
    )
  )
}

# --- Server Module ---
cohortes_politomica_Server <- function(id, lang) {
  moduleServer(id, function(input, output, session) {
    output$title_ui <- renderUI({
      tr <- get_translations(lang(), "cohortes_pt_politomica")
      h3(tr$title)
    })
    output$h_params <- renderUI({
      tr <- get_translations(lang(), "cohortes_pt_politomica")
      tr$h_params
    })
    output$h_res <- renderUI({
      tr <- get_translations(lang(), "cohortes_pt_politomica")
      tr$h_res
    })

    observeEvent(lang(), {
      tr <- get_translations(lang(), "cohortes_pt_politomica")
      updateTextInput(session, "lexpo", label = tr$lbl_lexpo)
      updateTextInput(session, "a", label = tr$lbl_a)
      updateTextInput(session, "t", label = tr$lbl_t)
      updateRadioButtons(session, "rc", label = tr$lbl_rc, choices = tr$rc_choices, selected = input$rc)
    })

    # Converted to reactive for real-time updates
    results_computed <- reactive({
      tr <- get_translations(lang(), "cohortes_pt_politomica")

      lexpo <- tryCatch(as.numeric(unlist(strsplit(input$lexpo, ","))), error = function(e) NULL)
      a <- tryCatch(as.numeric(unlist(strsplit(input$a, ","))), error = function(e) NULL)
      tm <- tryCatch(as.numeric(unlist(strsplit(input$t, ","))), error = function(e) NULL)
      rc <- input$rc

      if (is.null(lexpo) || is.null(a) || is.null(tm) ||
        any(is.na(lexpo)) || any(is.na(a)) || any(is.na(tm)) ||
        length(lexpo) != length(a) || length(a) != length(tm) ||
        length(lexpo) == 0) {
        return(tr$err_len)
      }

      n_levels <- length(lexpo)
      ir <- numeric(n_levels)
      ir_ref <- if (rc == "F") a[1] / tm[1] else a[n_levels] / tm[n_levels]

      for (i in 1:n_levels) {
        ir[i] <- (a[i] / tm[i]) / ir_ref
      }

      scases <- sum(a)
      stotal <- sum(tm)
      ss <- sum(lexpo * tm) / stotal

      sa <- sum(lexpo * a)
      sls2 <- sum(tm * (lexpo - ss)^2)

      z <- (sa - scases * ss) / sqrt(scases / stotal * sls2)
      p_value <- 2 * (1 - pnorm(abs(z)))

      paste(
        tr$res_header,
        "Level |", paste(sprintf("%7.2f", lexpo), collapse = "|"), "|   TOTAL |\n",
        "------+", paste(rep("-------+", n_levels), collapse = ""), "---------+\n",
        "Cases |", paste(sprintf("%7.0f", a), collapse = "|"), "|", sprintf("%8.0f", scases), " |\n",
        "P-Time|", paste(sprintf("%7.0f", tm), collapse = "|"), "|", sprintf("%8.0f", stotal), " |\n",
        "------+", paste(rep("-------+", n_levels), collapse = ""), "---------+\n",
        "Rate R|", paste(sprintf("%7.3f", ir), collapse = "|"), "|\n",
        "\nTrend Z =", sprintf("%9.5f", z), "      P-Value =", sprintf("%7.5f", p_value), "\n"
      )
    })

    output$results <- renderText({
      req(results_computed())
      results_computed()
    })

    # ===== Print Template =====
    output$print_view <- renderUI({
      ns <- session$ns
      tr <- get_translations(lang(), "cohortes_pt_politomica")
      txt <- tryCatch(results_computed(), error = function(e) "")
      tagList(
        div(
          class = "printable-section",
          h3(tr$title),
          p(paste0("Exposure: ", input$lexpo, " | Cases: ", input$a, " | PT: ", input$t))
        ),
        div(class = "printable-section", h4(tr$h_res), tags$pre(txt))
      )
    })
    outputOptions(output, "print_view", suspendWhenHidden = FALSE)
  })
}
