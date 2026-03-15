# --- UI Module ---
fap_multivariable_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("title_ui")),
    sidebarLayout(
      sidebarPanel(
        uiOutput(ns("header_ui")),

        # Inputs for Cohort Study
        uiOutput(ns("cohort_header_ui")),
        uiOutput(ns("input_rr_ui")),
        uiOutput(ns("input_pe_ui")),
        hr(),

        # Inputs for Case-Control Study
        uiOutput(ns("cc_header_ui")),
        uiOutput(ns("input_or_ui")),
        uiOutput(ns("input_pce_ui")),
        hr(),

        # Removed calculate button for real-time update
      ),
      mainPanel(
        uiOutput(ns("results_header_ui")),
        verbatimTextOutput(ns("resultadoCohortes")),
        verbatimTextOutput(ns("resultadoCasosControles"))
      )
    ),
    div(
      class = "print-template",
      uiOutput(ns("print_view"))
    )
  )
}

# --- Server Module ---
fap_multivariable_Server <- function(id, lang) {
  moduleServer(id, function(input, output, session) {
    # Render static UI elements
    output$title_ui <- renderUI({
      tr <- get_translations(lang(), "fap_multivariable")
      h3(tr$title)
    })
    output$header_ui <- renderUI({
      tr <- get_translations(lang(), "fap_multivariable")
      h3(tr$header)
    })
    output$cohort_header_ui <- renderUI({
      tr <- get_translations(lang(), "fap_multivariable")
      h4(tr$cohort_header)
    })
    output$cc_header_ui <- renderUI({
      tr <- get_translations(lang(), "fap_multivariable")
      h4(tr$cc_header)
    })
    output$results_header_ui <- renderUI({
      tr <- get_translations(lang(), "fap_multivariable")
      h4(tr$res_header)
    })

    # Render inputs to update labels dynamically
    output$input_rr_ui <- renderUI({
      tr <- get_translations(lang(), "fap_multivariable")
      numericInput(session$ns("RR"), tr$lbl_rr, value = 1.2, min = 0, step = 0.01)
    })
    output$input_pe_ui <- renderUI({
      tr <- get_translations(lang(), "fap_multivariable")
      numericInput(session$ns("P_e"), tr$lbl_pe, value = 0.936, min = 0, max = 1, step = 0.01)
    })
    output$input_or_ui <- renderUI({
      tr <- get_translations(lang(), "fap_multivariable")
      numericInput(session$ns("OR"), tr$lbl_or, value = 1.5, min = 0, step = 0.01)
    })
    output$input_pce_ui <- renderUI({
      tr <- get_translations(lang(), "fap_multivariable")
      numericInput(session$ns("P_ce"), tr$lbl_pce, value = 0.12, min = 0, max = 1, step = 0.01)
    })

    # Update button label


    # Calculation Logic
    # Converted to observe() for real-time updates
    observe({
      req(input$RR, input$P_e, input$OR, input$P_ce)

      # FAP Cohort
      FAP_cohorte <- input$P_e * (input$RR - 1) / input$RR

      # FAP Case-Control
      FAP_cc <- input$P_ce * (input$OR - 1) / input$OR

      output$resultadoCohortes <- renderText({
        tr <- get_translations(lang(), "fap_multivariable")
        paste(tr$res_cohort, round(FAP_cohorte, 4))
      })

      output$resultadoCasosControles <- renderText({
        tr <- get_translations(lang(), "fap_multivariable")
        paste(tr$res_cc, round(FAP_cc, 4))
      })
    })

    # ===== Print Template =====
    output$print_view <- renderUI({
      ns <- session$ns
      tr <- get_translations(lang(), "fap_multivariable")

      coh_text <- tryCatch(
        {
          req(input$RR) # Changed from input$RR_cohorte to input$RR
          rr <- input$RR
          pe <- input$P_e # Changed from input$Pe_cohorte / 100 to input$P_e
          fap <- pe * (rr - 1) / rr # Simplified formula based on original calculation
          paste0("RR = ", rr, ", Pe = ", round(pe * 100, 2), "%\nFAP = ", round(fap * 100, 2), "%")
        },
        error = function(e) ""
      )

      cc_text <- tryCatch(
        {
          req(input$OR) # Changed from input$OR_cc to input$OR
          or_val <- input$OR
          pce <- input$P_ce # Changed from input$Pce_cc / 100 to input$P_ce
          fap <- pce * (or_val - 1) / or_val
          paste0("OR = ", or_val, ", Pce = ", round(pce * 100, 2), "%\nFAP = ", round(fap * 100, 2), "%")
        },
        error = function(e) ""
      )

      tagList(
        div(class = "printable-section", h3(tr$title)),
        div(class = "printable-section", h4(tr$cohort_header), tags$pre(coh_text)), # Changed tr$h_coh to tr$cohort_header
        div(class = "printable-section", h4(tr$cc_header), tags$pre(cc_text)) # Changed tr$h_cc to tr$cc_header
      )
    })
    outputOptions(output, "print_view", suspendWhenHidden = FALSE)
  })
}
