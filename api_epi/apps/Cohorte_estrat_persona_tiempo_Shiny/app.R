library(shiny)
library(epiR)
library(shinyjs)

# --- UI Module ---
cohorte_estrat_pt_UI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    uiOutput(ns("title_ui")),
    sidebarLayout(
      sidebarPanel(
        h4(uiOutput(ns("h_stratum1"))),
        numericInput(ns("casos1"), "Casos / Cases (Exp)", 30, min = 0),
        numericInput(ns("tiempo1"), "Persona-Tiempo / Person-Time (Exp)", 5000, min = 0),
        numericInput(ns("casos2"), "Casos / Cases (Non-Exp)", 15, min = 0),
        numericInput(ns("tiempo2"), "Persona-Tiempo / Person-Time (Non-Exp)", 7000, min = 0),
        hr(),
        h4(uiOutput(ns("h_stratum2"))),
        numericInput(ns("casos3"), "Casos / Cases (Exp)", 18, min = 0),
        numericInput(ns("tiempo3"), "Persona-Tiempo / Person-Time (Exp)", 4000, min = 0),
        numericInput(ns("casos4"), "Casos / Cases (Non-Exp)", 9, min = 0),
        numericInput(ns("tiempo4"), "Persona-Tiempo / Person-Time (Non-Exp)", 6000, min = 0),
        hr(),
        # Removed analyze button for real-time update
        br(), br()
      ),
      mainPanel(
        h4(uiOutput(ns("res_gen"))),
        verbatimTextOutput(ns("results")),
        h4(uiOutput(ns("res_s1"))),
        verbatimTextOutput(ns("results_estrato1")),
        h4(uiOutput(ns("res_s2"))),
        verbatimTextOutput(ns("results_estrato2"))
      )
    ),
    div(
      class = "print-template",
      uiOutput(ns("print_view"))
    )
  )
}

# --- Server Module ---
cohorte_estrat_pt_Server <- function(id, lang) {
  moduleServer(id, function(input, output, session) {
    output$title_ui <- renderUI({
      tr <- get_translations(lang(), "cohorte_estrat_pt")
      h3(tr$title)
    })

    observeEvent(lang(), {
      tr <- get_translations(lang(), "cohorte_estrat_pt")

      updateNumericInput(session, "casos1", label = tr$lbl_c_exp)
      updateNumericInput(session, "tiempo1", label = tr$lbl_pt_exp)
      updateNumericInput(session, "casos2", label = tr$lbl_c_ne)
      updateNumericInput(session, "tiempo2", label = tr$lbl_pt_ne)

      updateNumericInput(session, "casos3", label = tr$lbl_c_exp)
      updateNumericInput(session, "tiempo3", label = tr$lbl_pt_exp)
      updateNumericInput(session, "casos4", label = tr$lbl_c_ne)
      updateNumericInput(session, "tiempo4", label = tr$lbl_pt_ne)


      # Static UI
      ids_map <- list(
        "h_stratum1" = "s1",
        "h_stratum2" = "s2",
        "res_gen" = "res_gen",
        "res_s1" = "res_s1",
        "res_s2" = "res_s2"
      )
      update_static_ui(session, ids_map, tr, session$ns)
    })

    results <- reactiveVal()
    results_estrato1 <- reactiveVal()
    results_estrato2 <- reactiveVal()

    # Converted to observe() for real-time updates
    observe({
      req(
        input$casos1, input$tiempo1, input$casos2, input$tiempo2,
        input$casos3, input$tiempo3, input$casos4, input$tiempo4
      )

      dat.m04 <- matrix(c(
        input$casos1, input$tiempo1, 1,
        input$casos2, input$tiempo2, 1,
        input$casos3, input$tiempo3, 2,
        input$casos4, input$tiempo4, 2
      ), nrow = 4, ncol = 3, byrow = TRUE)

      colnames(dat.m04) <- c("obs", "tar", "grp")
      dat.df04 <- data.frame(dat.m04)

      dat.tab04 <- sapply(1:length(unique(dat.df04$grp)), function(x) {
        as.matrix(dat.df04[dat.df04$grp == x, 1:2], ncol = 2, byrow = TRUE)
      },
      simplify = "array"
      )

      resultado_epiR <- epi.2by2(
        dat = dat.tab04, method = "cohort.time", digits = 2,
        conf.level = 0.95, units = 1000 * 365.25,
        interpret = FALSE, outcome = "as.columns"
      )

      dat.e1 <- matrix(c(input$casos1, input$tiempo1, input$casos2, input$tiempo2), nrow = 2, byrow = TRUE)
      dat.e2 <- matrix(c(input$casos3, input$tiempo3, input$casos4, input$tiempo4), nrow = 2, byrow = TRUE)

      res_e1 <- epi.2by2(dat = dat.e1, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000 * 365.25, interpret = FALSE, outcome = "as.columns")
      res_e2 <- epi.2by2(dat = dat.e2, method = "cohort.time", digits = 2, conf.level = 0.95, units = 1000 * 365.25, interpret = FALSE, outcome = "as.columns")

      results(resultado_epiR)
      results_estrato1(res_e1)
      results_estrato2(res_e2)
    })

    output$results <- renderPrint({
      req(results())
      results()
    })
    output$results_estrato1 <- renderPrint({
      req(results_estrato1())
      results_estrato1()
    })
    output$results_estrato2 <- renderPrint({
      req(results_estrato2())
      results_estrato2()
    })

    # ===== Print Template =====
    output$print_view <- renderUI({
      ns <- session$ns
      tr <- get_translations(lang(), "cohorte_estrat_pt")

      gen_text <- tryCatch(capture.output(print(results())), error = function(e) "")
      s1_text <- tryCatch(capture.output(print(results_estrato1())), error = function(e) "")
      s2_text <- tryCatch(capture.output(print(results_estrato2())), error = function(e) "")

      tagList(
        div(
          class = "printable-section",
          h3(tr$title),
          p(paste0("S1: Exp(", input$casos1, "/", input$tiempo1, "), Non-Exp(", input$casos2, "/", input$tiempo2, ")")),
          p(paste0("S2: Exp(", input$casos3, "/", input$tiempo3, "), Non-Exp(", input$casos4, "/", input$tiempo4, ")"))
        ),
        div(class = "printable-section", h4(tr$res_gen), tags$pre(paste(gen_text, collapse = "\n"))),
        div(class = "printable-section", h4(tr$res_s1), tags$pre(paste(s1_text, collapse = "\n"))),
        div(class = "printable-section", h4(tr$res_s2), tags$pre(paste(s2_text, collapse = "\n")))
      )
    })
    outputOptions(output, "print_view", suspendWhenHidden = FALSE)
  })
}
