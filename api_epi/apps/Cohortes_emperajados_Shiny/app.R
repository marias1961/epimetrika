library(shiny)

# --- UI Module ---
cohortes_emparejados_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("title_ui")),
    sidebarLayout(
      sidebarPanel(
        h4(uiOutput(ns("h_params"))),
        numericInput(ns("a"), "Expuestos - Casos (a) / Exposed Cases:", 30),
        numericInput(ns("b"), "Expuestos - No Casos (b) / Exposed Non-Cases:", 50),
        numericInput(ns("c"), "No Expuestos - Casos (c) / Unexposed Cases:", 10),
        numericInput(ns("d"), "No Expuestos - No Casos (d) / Unexposed Non-Cases:", 210),
        hr(),
        # Removed calculate button for real-time update
        br(), br()
      ),
      mainPanel(
        h3(uiOutput(ns("h_results"))),
        h4(uiOutput(ns("h_tab"))),
        verbatimTextOutput(ns("tabla_pareada")),
        h4(uiOutput(ns("h_mc"))),
        verbatimTextOutput(ns("mcnemar_results")),
        h4(uiOutput(ns("h_rr"))),
        verbatimTextOutput(ns("RR_results")),
        h4(uiOutput(ns("h_rr_mh"))),
        verbatimTextOutput(ns("RR_MH_results")),
        h4(uiOutput(ns("h_imp"))),
        verbatimTextOutput(ns("impact_results")),
        h4(uiOutput(ns("h_fap"))),
        verbatimTextOutput(ns("fpp_fae_results"))
      )
    ),
    div(
      class = "print-template",
      uiOutput(ns("print_view"))
    )
  )
}

# --- Server Module ---
cohortes_emparejados_Server <- function(id, lang) {
  moduleServer(id, function(input, output, session) {
    output$title_ui <- renderUI({
      tr <- get_translations(lang(), "cohortes_emparejados")
      h3(tr$title)
    })

    observeEvent(lang(), {
      tr <- get_translations(lang(), "cohortes_emparejados")
      updateNumericInput(session, "a", label = tr$lbl_a)
      updateNumericInput(session, "b", label = tr$lbl_b)
      updateNumericInput(session, "c", label = tr$lbl_c)
      updateNumericInput(session, "d", label = tr$lbl_d)


      # Static UI
      ids_map <- list(
        "h_params" = "h_params",
        "h_results" = "h_results",
        "h_tab" = "h_tab_cont",
        "h_mc" = "h_mc",
        "h_rr" = "h_rr",
        "h_rr_mh" = "h_rr_mh",
        "h_imp" = "h_imp",
        "h_fap" = "h_fap"
      )
      update_static_ui(session, ids_map, tr, session$ns)
    })

    resultados <- reactiveValues()

    calcular_fae_fap_fpe_fpp <- function(RR_MH, a, b, c) {
      if (RR_MH >= 1) {
        FAE <- (RR_MH - 1) / RR_MH
        FAP <- (a + b) / ((a + b) + (a + c)) / 100
        return(list(FAE = FAE, FAP = FAP))
      } else {
        FPE <- 1 - RR_MH
        FPP <- (a + b) / ((a + b) + (a + c)) * (1 - RR_MH) /
          ((a + b) / ((a + b) + (a + c))) + ((1 - RR_MH) + RR_MH)
        return(list(FPE = FPE, FPP = FPP))
      }
    }

    # Converted to observe() for real-time updates
    observe({
      req(lang())
      tr <- get_translations(lang(), "cohortes_emparejados")

      resultados$tabla_pareada <- matrix(
        c(input$a, input$b, input$c, input$d),
        nrow = 2, byrow = TRUE,
        dimnames = list(tr$tab_cont_rows[1:2], tr$tab_cont_cols[1:2])
      )

      a <- input$a
      b <- input$b
      c <- input$c
      d <- input$d
      N <- a + b + c + d

      Ie <- (a + b) / N
      Io <- (a + c) / N
      resultados$RR <- Ie / Io
      se_logRR <- sqrt((1 / (a + b) + (1 / (a + c)) - (1 / N) - (1 / N)))
      resultados$IC_low <- exp(log(resultados$RR) - 1.96 * se_logRR)
      resultados$IC_high <- exp(log(resultados$RR) + 1.96 * se_logRR)

      resultados$RR_MH <- (a + b) / (a + c)
      se_logRR_MH <- sqrt((b + c) / ((a + c) * (a + b)))
      resultados$IC_low_MH <- exp(log(resultados$RR_MH) - 1.96 * se_logRR_MH)
      resultados$IC_high_MH <- exp(log(resultados$RR_MH) + 1.96 * se_logRR_MH)

      resultados$DR <- ((a + c) - (a + b)) / N
      resultados$resultados_fae_fap <- calcular_fae_fap_fpe_fpp(resultados$RR_MH, a, b, c)
      resultados$mcnemar_test <- mcnemar.test(resultados$tabla_pareada)
    })

    output$tabla_pareada <- renderPrint({
      req(resultados$tabla_pareada)
      resultados$tabla_pareada
    })

    output$mcnemar_results <- renderText({
      req(resultados$mcnemar_test)
      tr <- get_translations(lang(), "cohortes_emparejados")
      paste0(
        tr$txt_mc, " ", round(resultados$mcnemar_test$statistic, 2),
        "\n", tr$txt_p, " ", round(resultados$mcnemar_test$p.value, 4)
      )
    })

    output$RR_results <- renderText({
      req(resultados$RR)
      tr <- get_translations(lang(), "cohortes_emparejados")
      paste0(
        tr$txt_rr, " ", round(resultados$RR, 2),
        "\n", tr$txt_ic, " [", round(resultados$IC_low, 2), ", ", round(resultados$IC_high, 2), "]"
      )
    })

    output$RR_MH_results <- renderText({
      req(resultados$RR_MH)
      tr <- get_translations(lang(), "cohortes_emparejados")
      paste0(
        "RR_MH: ", round(resultados$RR_MH, 2),
        "\n", tr$txt_ic, " [", round(resultados$IC_low_MH, 2), ", ", round(resultados$IC_high_MH, 2), "]"
      )
    })

    output$impact_results <- renderText({
      req(resultados$DR)
      tr <- get_translations(lang(), "cohortes_emparejados")
      paste0(tr$txt_dr, " ", round(resultados$DR, 2))
    })

    output$fpp_fae_results <- renderText({
      req(resultados$resultados_fae_fap)
      if (resultados$RR_MH >= 1) {
        paste(
          "FAE:", round(resultados$resultados_fae_fap$FAE, 2),
          "\nFAP:", round(resultados$resultados_fae_fap$FAP, 2)
        )
      } else {
        paste(
          "FPE:", round(resultados$resultados_fae_fap$FPE, 2),
          "\nFPP:", round(resultados$resultados_fae_fap$FPP, 2)
        )
      }
    })

    # ===== Print Template =====
    output$print_view <- renderUI({
      req(lang())
      ns <- session$ns
      tr <- get_translations(lang(), "cohortes_emparejados")

      mc_text <- ""
      if (!is.null(resultados$mcnemar_test)) {
        mc_text <- paste0(
          tr$txt_mc, " ", round(resultados$mcnemar_test$statistic, 2),
          "\n", tr$txt_p, " ", round(resultados$mcnemar_test$p.value, 4)
        )
      }
      rr_text <- ""
      if (!is.null(resultados$RR)) {
        rr_text <- paste0(
          tr$txt_rr, " ", round(resultados$RR, 2),
          "\n", tr$txt_ic, " [", round(resultados$IC_low, 2), ", ", round(resultados$IC_high, 2), "]"
        )
      }
      rr_mh_text <- ""
      if (!is.null(resultados$RR_MH)) {
        rr_mh_text <- paste0(
          "RR_MH: ", round(resultados$RR_MH, 2),
          "\n", tr$txt_ic, " [", round(resultados$IC_low_MH, 2), ", ", round(resultados$IC_high_MH, 2), "]"
        )
      }
      dr_text <- if (!is.null(resultados$DR)) paste0(tr$txt_dr, " ", round(resultados$DR, 2)) else ""
      fae_text <- ""
      if (!is.null(resultados$resultados_fae_fap)) {
        if (resultados$RR_MH >= 1) {
          fae_text <- paste(
            "FAE:", round(resultados$resultados_fae_fap$FAE, 2),
            "\nFAP:", round(resultados$resultados_fae_fap$FAP, 2)
          )
        } else {
          fae_text <- paste(
            "FPE:", round(resultados$resultados_fae_fap$FPE, 2),
            "\nFPP:", round(resultados$resultados_fae_fap$FPP, 2)
          )
        }
      }

      tagList(
        div(
          class = "printable-section",
          h3(tr$title),
          p(paste0("a=", input$a, ", b=", input$b, ", c=", input$c, ", d=", input$d))
        ),
        div(class = "printable-section", h4(tr$h_mc), tags$pre(mc_text)),
        div(class = "printable-section", h4(tr$h_rr), tags$pre(rr_text)),
        div(class = "printable-section", h4(tr$h_rr_mh), tags$pre(rr_mh_text)),
        div(class = "printable-section", h4(tr$h_imp), tags$pre(dr_text)),
        div(class = "printable-section", h4(tr$h_fap), tags$pre(fae_text))
      )
    })
    outputOptions(output, "print_view", suspendWhenHidden = FALSE)
  })
}
