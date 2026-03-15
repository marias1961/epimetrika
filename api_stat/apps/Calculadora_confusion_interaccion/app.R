library(shiny)
library(shinythemes)
library(DT)
library(rmarkdown)

# ---- UI Module ----
confusion_interaccion_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("title_ui")),
    sidebarLayout(
      sidebarPanel(
        tags$h4(id = ns("lbl_meas"), "Seleccione la medida", style = "color: #2c3e50;"),
        radioButtons(ns("tipo_medida"), NULL, # Label updated via server
          choices = c("Odds Ratio (OR)" = "or", "Riesgo Relativo (RR)" = "rr"),
          selected = "or"
        ),
        uiOutput(ns("inputs_dinamicos")),
        br(),
        actionButton(ns("leyenda_btn"), "Info", class = "btn-info")
      ),
      mainPanel(
        h3(id = ns("res_header"), "Resultados"),
        DTOutput(ns("tabla_resultados")),
        br(),
        htmlOutput(ns("interpretacion_html")),
        br(),
        htmlOutput(ns("notaNA")),
        # Print Template (hidden on screen via CSS)
        div(
          class = "print-template",
          uiOutput(ns("print_view"))
        )
      )
    )
  )
}

# ---- Server Module ----
confusion_interaccion_Server <- function(id, lang) {
  moduleServer(id, function(input, output, session) {
    # Static UI rendering
    output$title_ui <- renderUI({
      tr <- get_translations(lang(), "confusion_interaccion_complex")
      h3(tr$title)
    })

    # Helper: update IDs
    observeEvent(lang(), {
      tr <- get_translations(lang(), "confusion_interaccion_complex")
      ns <- session$ns

      # Navbar and Headers
      ids_map <- list(
        "lbl_meas" = "lbl_meas",
        "res_header" = "res_header"
      )
      update_static_ui(session, ids_map, tr, ns)

      # Radio Buttons
      updateRadioButtons(session, "tipo_medida",
        label = tr$lbl_meas,
        choices = setNames(c("or", "rr"), c(tr$meas_or, tr$meas_rr)),
        selected = input$tipo_medida
      )

      # Buttons
      # updateActionButton(session, "calc_medida", label = tr$calculate)
      # Download button label is harder to update if using downloadButton, but we can try
      # However, we usually just leave it or use a custom one.
      # Let's just update the action button for info.
      updateActionButton(session, "leyenda_btn", label = tr$btn_info_legend)
    })

    # Dynamic inputs (labels depends on lang and selection)
    output$inputs_dinamicos <- renderUI({
      tr <- get_translations(lang(), "confusion_interaccion_complex")
      ns <- session$ns

      if (input$tipo_medida == "or") {
        tagList(
          numericInput(ns("cruda"), tr$lbl_or_c, value = 5.9, step = 0.1),
          numericInput(ns("ajustada"), tr$lbl_or_a, value = 2.3, step = 0.1),
          tags$h5(paste0(tr$meas_or, " ", tr$s1)), # S1/S2 usually common or in mod
          numericInput(ns("e1"), tr$lbl_or_e1, value = 2.3, step = 0.1),
          numericInput(ns("e1_low"), tr$lbl_or_e1_l, value = NA, step = 0.01),
          numericInput(ns("e1_high"), tr$lbl_or_e1_h, value = NA, step = 0.01),
          tags$h5(paste0(tr$meas_or, " ", tr$s2)),
          numericInput(ns("e2"), tr$lbl_or_e2, value = 2.4, step = 0.1),
          numericInput(ns("e2_low"), tr$lbl_or_e2_l, value = NA, step = 0.01),
          numericInput(ns("e2_high"), tr$lbl_or_e2_h, value = NA, step = 0.01)
        )
      } else {
        tagList(
          numericInput(ns("cruda"), tr$lbl_rr_c, value = 3.2, step = 0.1),
          numericInput(ns("ajustada"), tr$lbl_rr_a, value = 2.0, step = 0.1),
          tags$h5(paste0(tr$meas_rr, " ", tr$s1)),
          numericInput(ns("e1"), tr$lbl_rr_e1, value = 1.9, step = 0.1),
          numericInput(ns("e1_low"), tr$lbl_rr_e1_l, value = NA, step = 0.01),
          numericInput(ns("e1_high"), tr$lbl_rr_e1_h, value = NA, step = 0.01),
          tags$h5(paste0(tr$meas_rr, " ", tr$s2)),
          numericInput(ns("e2"), tr$lbl_rr_e2, value = 2.1, step = 0.1),
          numericInput(ns("e2_low"), tr$lbl_rr_e2_l, value = NA, step = 0.01),
          numericInput(ns("e2_high"), tr$lbl_rr_e2_h, value = NA, step = 0.01)
        )
      }
    })

    # Helper: calculate SE from IC
    calc_se <- function(low, high) {
      if (is.na(low) || is.na(high) || low <= 0 || high <= 0) {
        return(NA_real_)
      }
      (log(high) - log(low)) / (2 * 1.96)
    }

    # Formal homogeneity Z-test p-value
    homog_pval <- function(e1, e2, se1, se2) {
      if (any(is.na(c(e1, e2, se1, se2)))) {
        return(NA_real_)
      }
      if (e1 <= 0 || e2 <= 0) {
        return(NA_real_)
      }
      z <- (log(e1) - log(e2)) / sqrt(se1^2 + se2^2)
      2 * pnorm(-abs(z))
    }

    # Interpretations
    calc_confusion_text <- function(cruda, ajustada, tr) {
      if (is.na(cruda) || is.na(ajustada) || cruda == 0) {
        return("")
      }
      cambio_pct <- abs((ajustada - cruda) / cruda) * 100
      if (cambio_pct > 10) tr$int_conf_yes else tr$int_conf_no
    }

    exploratory_interaction_text <- function(e1, e2, tr) {
      if (is.na(e1) || is.na(e2)) {
        return(paste0(tr$int_expl, ": ", tr$expl_int_insuf))
      }
      diff_rel <- abs(e1 - e2) / pmin(e1, e2)
      if (diff_rel > 0.20) {
        paste0(tr$int_expl, ": ", tr$expl_int_yes)
      } else {
        paste0(tr$int_expl, ": ", tr$expl_int_no)
      }
    }

    # Central reactive: build results
    results <- reactive({
      req(input$tipo_medida, input$cruda, input$ajustada, input$e1, input$e2)
      se1 <- calc_se(input$e1_low, input$e1_high)
      se2 <- calc_se(input$e2_low, input$e2_high)
      pval <- homog_pval(input$e1, input$e2, se1, se2)

      list(
        tipo = input$tipo_medida,
        cruda = input$cruda, ajustada = input$ajustada,
        e1 = input$e1, e1_low = input$e1_low, e1_high = input$e1_high,
        e2 = input$e2, e2_low = input$e2_low, e2_high = input$e2_high,
        pval = pval
      )
    })

    # Web table
    output$tabla_resultados <- renderDT({
      req(results())
      res <- results()
      tr <- get_translations(lang(), "confusion_interaccion_complex")

      rows <- if (res$tipo == "or") tr$tab_rows$or else tr$tab_rows$rr

      df <- data.frame(
        Medida = rows,
        Valor = c(res$cruda, res$ajustada, res$e1, res$e2),
        IC95_Lower = c(NA, NA, res$e1_low, res$e2_low),
        IC95_Upper = c(NA, NA, res$e1_high, res$e2_high),
        stringsAsFactors = FALSE
      )

      # Format
      fmt <- function(x) ifelse(is.na(x), "NA", sprintf("%.3f", x))
      df$Valor <- fmt(df$Valor)
      df$IC95_Lower <- fmt(df$IC95_Lower)
      df$IC95_Upper <- fmt(df$IC95_Upper)

      colnames(df) <- tr$tab_cols

      datatable(df,
        escape = FALSE, rownames = FALSE,
        options = list(
          dom = "t", ordering = FALSE, paging = FALSE,
          columnDefs = list(list(className = "dt-right", targets = 1:3))
        )
      )
    })

    # Interpretation
    output$interpretacion_html <- renderUI({
      req(results())
      res <- results()
      tr <- get_translations(lang(), "confusion_interaccion_complex")

      conf_text <- calc_confusion_text(res$cruda, res$ajustada, tr)

      inter_text <- if (!is.na(res$pval)) {
        if (res$pval < 0.05) tr$int_int_yes else tr$int_int_no
      } else {
        exploratory_interaction_text(res$e1, res$e2, tr)
      }

      tipo_lbl <- if (res$tipo == "or") tr$meas_or else tr$meas_rr

      HTML(paste0("<b>", tipo_lbl, ":</b> ", conf_text, " | <b>Interacción:</b> ", inter_text))
    })

    output$notaNA <- renderUI({
      tr <- get_translations(lang(), "confusion_interaccion_complex")
      HTML(paste0("<em>", tr$note_na, "</em>"))
    })

    # Legenda modal
    observeEvent(input$leyenda_btn, {
      tr <- get_translations(lang(), "confusion_interaccion_complex")
      showModal(modalDialog(
        title = tr$modal_title,
        tags$p(tr$modal_body),
        tags$pre(tr$modal_formula),
        tags$p(tr$modal_p),
        easyClose = TRUE, footer = NULL
      ))
    })

    # Manual PDF Download removed in favor of unified global print

    # ========= Print View Generator =========
    output$tab_res_print <- renderTable({
      req(results())
      res <- results()
      tr <- get_translations(lang(), "confusion_interaccion_complex")
      rows_lbl <- if (res$tipo == "or") tr$tab_rows$or else tr$tab_rows$rr
      df <- data.frame(
        Medida = rows_lbl,
        Valor = c(res$cruda, res$ajustada, res$e1, res$e2),
        IC95_Lower = c(NA, NA, res$e1_low, res$e2_low),
        IC95_Upper = c(NA, NA, res$e1_high, res$e2_high),
        stringsAsFactors = FALSE
      )
      colnames(df) <- tr$tab_cols
      df
    })

    output$print_view <- renderUI({
      req(results())
      res <- results()
      tr <- get_translations(lang(), "confusion_interaccion_complex")
      ns <- session$ns

      conf_text <- calc_confusion_text(res$cruda, res$ajustada, tr)
      inter_text <- if (!is.na(res$pval)) {
        if (res$pval < 0.05) tr$int_int_yes else tr$int_int_no
      } else {
        exploratory_interaction_text(res$e1, res$e2, tr)
      }

      tagList(
        div(class = "printable-section", h3(tr$title), tableOutput(ns("tab_res_print"))),
        div(
          class = "printable-section",
          h4(tr$report_interp),
          p(paste0("Confusion: ", conf_text)),
          p(paste0("Interaction: ", inter_text))
        )
      )
    })

    outputOptions(output, "print_view", suspendWhenHidden = FALSE)
  })
}
