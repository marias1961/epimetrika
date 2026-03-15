# Instalar y cargar los paquetes necesarios si no los tienes
# install.packages(c("shiny", "shinythemes"))

library(shiny)
library(shinythemes)

# --- Modulo UI ---
or_rr_UI <- function(id) {
  ns <- NS(id)
  tagList(
    # Título de la aplicación
    uiOutput(ns("title_ui")),


    # Panel lateral con las entradas de datos unificadas
    sidebarLayout(
      sidebarPanel(
        uiOutput(ns("input_header_ui")),
        uiOutput(ns("help_text_ui")),

        # Botón de ayuda
        actionButton(ns("ayuda_btn"), "Info", class = "btn-info"),
        hr(), # Separador

        # Entradas unificadas
        numericInput(inputId = ns("or_cruda"), label = "OR/RR Cruda:", value = 5.9, step = 0.1),
        numericInput(inputId = ns("or_ajustada"), label = "OR/RR Ajustada:", value = 2.3, step = 0.1),
        numericInput(inputId = ns("or_estrato1"), label = "OR/RR Estrato 1:", value = 2.3, step = 0.1),
        numericInput(inputId = ns("or_estrato2"), label = "OR/RR Estrato 2:", value = 2.3, step = 0.1)
      ),

      # Panel principal con la tabla de resultados
      mainPanel(
        uiOutput(ns("results_header_ui")),
        tableOutput(ns("tabla_resultados")),
        # Print Template (hidden on screen via CSS)
        div(
          class = "print-template",
          uiOutput(ns("print_view"))
        )
      )
    )
  )
}

# --- Modulo Server ---
or_rr_Server <- function(id, lang) {
  moduleServer(id, function(input, output, session) {
    # Renderizados Dinámicos de Texto
    output$title_ui <- renderUI({
      tr <- get_translations(lang(), "or_rr_confusion_interaccion")
      h3(tr$title)
    })
    output$input_header_ui <- renderUI({
      tr <- get_translations(lang(), "or_rr_confusion_interaccion")
      tags$h4(tr$input_header, style = "color: #2c3e50;")
    })
    output$help_text_ui <- renderUI({
      tr <- get_translations(lang(), "or_rr_confusion_interaccion")
      helpText(tr$help)
    })
    output$results_header_ui <- renderUI({
      tr <- get_translations(lang(), "or_rr_confusion_interaccion")
      h3(tr$results_header)
    })

    # Observe Language Change for Inputs
    observeEvent(lang(), {
      tr <- get_translations(lang(), "or_rr_confusion_interaccion")
      updateActionButton(session, "ayuda_btn", label = tr$btn_help)
      updateNumericInput(session, "or_cruda", label = tr$lbl_cruda)
      updateNumericInput(session, "or_ajustada", label = tr$lbl_ajustada)
      updateNumericInput(session, "or_estrato1", label = tr$lbl_e1)
      updateNumericInput(session, "or_estrato2", label = tr$lbl_e2)
    })

    # Lógica para mostrar la ventana de ayuda
    observeEvent(input$ayuda_btn, {
      tr <- get_translations(lang(), "or_rr_confusion_interaccion")
      t <- tr$modal_body

      showModal(modalDialog(
        title = tr$modal_title,
        tags$h4(t$c1), tags$p(t$t1),
        if (!is.null(t$msg_conf)) tags$p(tags$em(t$msg_conf)),
        tags$hr(),
        tags$h4(t$c2), tags$p(t$t2),
        if (!is.null(t$msg_int)) tags$p(tags$em(t$msg_int)),
        easyClose = TRUE, footer = NULL
      ))
    })

    # Lógica para la tabla de resultados
    output$tabla_resultados <- renderTable(
      {
        req(input$or_cruda, input$or_ajustada, input$or_estrato1, input$or_estrato2)

        tr <- get_translations(lang(), "or_rr_confusion_interaccion")

        cambio_pct <- ((input$or_ajustada - input$or_cruda) / input$or_cruda) * 100
        hay_confusion <- (abs(cambio_pct) > 10)
        hay_interaccion <- (input$or_estrato1 != input$or_estrato2)

        t_res <- tr$res
        conclusion <- ""

        if (hay_confusion && !hay_interaccion) {
          conclusion <- t_res$c_conf
        } else if (!hay_confusion && hay_interaccion) {
          conclusion <- t_res$c_int
        } else if (hay_confusion && hay_interaccion) {
          conclusion <- t_res$c_both
        } else {
          conclusion <- t_res$c_none
        }

        df_out <- data.frame(
          Desc = c(t_res$r1, t_res$r2, t_res$r3, t_res$r4, t_res$r5, t_res$r6),
          Val = c(
            as.character(round(input$or_cruda, 2)),
            as.character(round(input$or_ajustada, 2)),
            as.character(round(cambio_pct, 2)),
            as.character(round(input$or_estrato1, 2)),
            as.character(round(input$or_estrato2, 2)),
            conclusion
          )
        )
        setNames(df_out, c(t_res$desc, t_res$val))
      },
      striped = TRUE,
      hover = TRUE,
      bordered = TRUE,
      width = "100%"
    )

    # ========= Print Table Output (Duplicate) =========
    output$tabla_resultados_print <- renderTable(
      {
        req(input$or_cruda, input$or_ajustada, input$or_estrato1, input$or_estrato2)
        tr <- get_translations(lang(), "or_rr_confusion_interaccion")
        cambio_pct <- ((input$or_ajustada - input$or_cruda) / input$or_cruda) * 100
        hay_confusion <- (abs(cambio_pct) > 10)
        hay_interaccion <- (input$or_estrato1 != input$or_estrato2)
        t_res <- tr$res
        conclusion <- ""
        if (hay_confusion && !hay_interaccion) {
          conclusion <- t_res$c_conf
        } else if (!hay_confusion && hay_interaccion) {
          conclusion <- t_res$c_int
        } else if (hay_confusion && hay_interaccion) {
          conclusion <- t_res$c_both
        } else {
          conclusion <- t_res$c_none
        }
        df_out <- data.frame(
          Desc = c(t_res$r1, t_res$r2, t_res$r3, t_res$r4, t_res$r5, t_res$r6),
          Val = c(
            as.character(round(input$or_cruda, 2)),
            as.character(round(input$or_ajustada, 2)),
            as.character(round(cambio_pct, 2)),
            as.character(round(input$or_estrato1, 2)),
            as.character(round(input$or_estrato2, 2)),
            conclusion
          )
        )
        setNames(df_out, c(t_res$desc, t_res$val))
      },
      striped = TRUE,
      hover = TRUE,
      bordered = TRUE,
      width = "100%"
    )

    # ========= Print View Generator =========
    output$print_view <- renderUI({
      ns <- session$ns
      tr <- get_translations(lang(), "or_rr_confusion_interaccion")
      tagList(
        div(
          class = "printable-section",
          h3(tr$title),
          p(paste0(
            tr$lbl_cruda, " ", input$or_cruda,
            " | ", tr$lbl_ajustada, " ", input$or_ajustada,
            " | ", tr$lbl_e1, " ", input$or_estrato1,
            " | ", tr$lbl_e2, " ", input$or_estrato2
          ))
        ),
        div(
          class = "printable-section",
          h4(tr$results_header),
          tableOutput(ns("tabla_resultados_print"))
        )
      )
    })

    # Force Rendering of Hidden Print Outputs
    outputOptions(output, "print_view", suspendWhenHidden = FALSE)
    outputOptions(output, "tabla_resultados_print", suspendWhenHidden = FALSE)
  })
}
