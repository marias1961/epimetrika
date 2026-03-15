library(shiny)
library(ggplot2)
library(DT)
library(rmarkdown)
library(knitr)
library(kableExtra)

# --- UI Module ---
conf_inter_graphics_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("title_ui")),
    sidebarLayout(
      sidebarPanel(
        h4(uiOutput(ns("lbl_meas_ui"))),
        radioButtons(ns("tipo_medida"), NULL, # Label updated via server
          choices = c("Odds Ratio (OR)" = "or", "Riesgo Relativo (RR)" = "rr"),
          selected = "or"
        ),
        uiOutput(ns("inputs_dinamicos")),
        br(),
        checkboxInput(ns("mh_manual_chk"), "Enter M-H manually", value = FALSE),
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("mh_manual_chk")),
          numericInput(ns("mh_est"), "M-H Estimate:", value = NA, step = 0.01),
          numericInput(ns("mh_low"), "M-H Lower CI:", value = NA, step = 0.01),
          numericInput(ns("mh_high"), "M-H Upper CI:", value = NA, step = 0.01)
        ),
        br(),
        actionButton(ns("leyenda_btn"), "Info", class = "btn-info")
      ),
      mainPanel(
        h3(uiOutput(ns("res_header_ui"))),
        DTOutput(ns("tabla_resultados")),
        br(),
        htmlOutput(ns("interpretacion_html")),
        br(),
        fluidRow(
          column(6, plotOutput(ns("plot_forest"), height = "450px")),
          column(6, plotOutput(ns("plot_confusion"), height = "450px"))
        ),
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

# --- Server Module ---
conf_inter_graphics_Server <- function(id, lang) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$title_ui <- renderUI({
      tr <- get_translations(lang(), "confusion_interaccion_graphics")
      h3(tr$title)
    })
    output$lbl_meas_ui <- renderUI({
      tr <- get_translations(lang(), "confusion_interaccion_graphics")
      tr$h_meas
    })
    output$res_header_ui <- renderUI({
      tr <- get_translations(lang(), "confusion_interaccion_graphics")
      tr$h_res
    })

    observeEvent(lang(), {
      tr <- get_translations(lang(), "confusion_interaccion_graphics")
      updateRadioButtons(session, "tipo_medida",
        label = tr$h_meas,
        choices = setNames(c("or", "rr"), c(tr$meas_or, tr$meas_rr)),
        selected = input$tipo_medida
      )
      updateCheckboxInput(session, "mh_manual_chk", label = tr$chk_mh)
      updateNumericInput(session, "mh_est", label = tr$lbl_mh_est)
      updateNumericInput(session, "mh_low", label = tr$lbl_mh_low)
      updateNumericInput(session, "mh_high", label = tr$lbl_mh_high)
      updateActionButton(session, "leyenda_btn", label = tr$btn_info_legend)
    })

    output$inputs_dinamicos <- renderUI({
      tr <- get_translations(lang(), "confusion_interaccion_graphics")
      ns <- session$ns
      if (input$tipo_medida == "or") {
        tagList(
          numericInput(ns("cruda"), tr$lbl_crude, value = 5.9, step = 0.1),
          numericInput(ns("ajustada"), tr$lbl_adj, value = 2.3, step = 0.1),
          tags$h5(tr$s1),
          numericInput(ns("e1"), tr$lbl_est, value = 2.3, step = 0.1),
          numericInput(ns("e1_low"), tr$lbl_low, value = NA, step = 0.01),
          numericInput(ns("e1_high"), tr$lbl_high, value = NA, step = 0.01),
          tags$h5(tr$s2),
          numericInput(ns("e2"), tr$lbl_est, value = 2.4, step = 0.1),
          numericInput(ns("e2_low"), tr$lbl_low, value = NA, step = 0.01),
          numericInput(ns("e2_high"), tr$lbl_high, value = NA, step = 0.01)
        )
      } else {
        tagList(
          numericInput(ns("cruda"), tr$lbl_crude, value = 3.2, step = 0.1),
          numericInput(ns("ajustada"), tr$lbl_adj, value = 2.0, step = 0.1),
          tags$h5(tr$s1),
          numericInput(ns("e1"), tr$lbl_est, value = 1.9, step = 0.1),
          numericInput(ns("e1_low"), tr$lbl_low, value = NA, step = 0.01),
          numericInput(ns("e1_high"), tr$lbl_high, value = NA, step = 0.01),
          tags$h5(tr$s2),
          numericInput(ns("e2"), tr$lbl_est, value = 2.1, step = 0.1),
          numericInput(ns("e2_low"), tr$lbl_low, value = NA, step = 0.01),
          numericInput(ns("e2_high"), tr$lbl_high, value = NA, step = 0.01)
        )
      }
    })

    # --- Helpers ---
    calc_se <- function(low, high) {
      if (is.na(low) || is.na(high) || low <= 0 || high <= 0) {
        return(NA_real_)
      }
      (log(high) - log(low)) / (2 * 1.96)
    }

    is_incoherent_func <- function(est, low, high) {
      if (is.na(est)) {
        return(FALSE)
      }
      if (!is.na(low) && low > est) {
        return(TRUE)
      }
      if (!is.na(high) && high < est) {
        return(TRUE)
      }
      FALSE
    }

    homog_pval_func <- function(e1, e2, se1, se2) {
      if (any(is.na(c(e1, e2, se1, se2)))) {
        return(NA_real_)
      }
      z <- (log(e1) - log(e2)) / sqrt(se1^2 + se2^2)
      2 * pnorm(-abs(z))
    }

    calc_mh_iv <- function(ests, lows, highs) {
      se <- mapply(calc_se, lows, highs)
      valid_idx <- which(!is.na(ests) & !is.na(se) & ests > 0 & se > 0)
      if (length(valid_idx) != length(ests)) {
        return(list(est = NA_real_, low = NA_real_, high = NA_real_, se = NA_real_, ok = FALSE))
      }
      logs <- log(ests[valid_idx])
      w <- 1 / (se[valid_idx]^2)
      log_comb <- sum(w * logs) / sum(w)
      se_comb <- sqrt(1 / sum(w))
      list(est = exp(log_comb), low = exp(log_comb - 1.96 * se_comb), high = exp(log_comb + 1.96 * se_comb), se = se_comb, ok = TRUE)
    }

    # --- Reactive Results ---
    results <- reactive({
      req(input$tipo_medida, input$cruda, input$ajustada, input$e1, input$e2)

      e1_incoh <- is_incoherent_func(input$e1, input$e1_low, input$e1_high)
      e2_incoh <- is_incoherent_func(input$e2, input$e2_low, input$e2_high)

      se1 <- calc_se(input$e1_low, input$e1_high)
      se2 <- calc_se(input$e2_low, input$e2_high)
      pval <- if (e1_incoh || e2_incoh) NA_real_ else homog_pval_func(input$e1, input$e2, se1, se2)

      mh_auto <- calc_mh_iv(c(input$e1, input$e2), c(input$e1_low, input$e2_low), c(input$e1_high, input$e2_high))

      if (input$mh_manual_chk) {
        mh_ok <- !(is.na(input$mh_est) || is.na(input$mh_low) || is.na(input$mh_high))
        list(
          tipo = if (input$tipo_medida == "or") "OR" else "RR",
          cruda = input$cruda, ajustada = input$ajustada,
          e1 = input$e1, e1_low = input$e1_low, e1_high = input$e1_high,
          e2 = input$e2, e2_low = input$e2_low, e2_high = input$e2_high,
          pval = pval,
          mh_est = if (mh_ok) input$mh_est else NA_real_,
          mh_low = if (mh_ok) input$mh_low else NA_real_,
          mh_high = if (mh_ok) input$mh_high else NA_real_,
          mh_ok = mh_ok, mh_source = "manual",
          any_incoherent = (e1_incoh || e2_incoh)
        )
      } else {
        list(
          tipo = if (input$tipo_medida == "or") "OR" else "RR",
          cruda = input$cruda, ajustada = input$ajustada,
          e1 = input$e1, e1_low = input$e1_low, e1_high = input$e1_high,
          e2 = input$e2, e2_low = input$e2_low, e2_high = input$e2_high,
          pval = pval,
          mh_est = mh_auto$est, mh_low = mh_auto$low, mh_high = mh_auto$high,
          mh_ok = mh_auto$ok, mh_source = "auto",
          any_incoherent = (e1_incoh || e2_incoh)
        )
      }
    })

    # --- Forest Plot ---
    make_forest_plot <- function(res, tr) {
      if (res$any_incoherent) {
        return(NULL)
      }
      # Reverse order for better visualization (S1 at top, MH at bottom)
      rows_names <- rev(c(tr$s1, tr$s2, "M-H"))
      df_plot <- data.frame(
        Target = factor(rows_names, levels = rows_names),
        Estimador = as.numeric(c(res$mh_est, res$e2, res$e1)),
        IC_low = as.numeric(c(res$mh_low, res$e2_low, res$e1_low)),
        IC_high = as.numeric(c(res$mh_high, res$e2_high, res$e1_high)),
        stringsAsFactors = FALSE
      )
      df_plot <- df_plot[!is.na(df_plot$Estimador), ]
      if (nrow(df_plot) == 0) {
        return(NULL)
      }

      # Numeric labels for the plot
      df_plot$label_text <- sprintf("%.2f (%.2f-%.2f)", df_plot$Estimador, df_plot$IC_low, df_plot$IC_high)
      df_plot$label_text <- gsub("NA-NA", "----", df_plot$label_text)

      ggplot(df_plot, aes(x = Target, y = Estimador)) +
        geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
        geom_errorbar(aes(ymin = IC_low, ymax = IC_high), width = 0.2, color = "#2c7fb8", size = 1.2) +
        geom_point(size = 6, color = "#2c7fb8") +
        geom_text(aes(label = label_text), vjust = -1.5, size = 4.5, fontface = "bold", color = "#2c3e50") +
        scale_y_log10(breaks = scales::breaks_log(n = 6)) +
        coord_flip() +
        labs(y = paste(res$tipo, tr$lbl_est, "(log scale)"), x = "", title = paste(tr$forest_title, res$tipo)) +
        theme_minimal(base_size = 14) +
        theme(
          panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold", margin = margin(b = 20)),
          axis.text = element_text(face = "bold"),
          panel.background = element_rect(fill = "#fdfdfd", color = NA)
        )
    }

    output$plot_forest <- renderPlot({
      req(results())
      tr <- get_translations(lang(), "confusion_interaccion_graphics")
      validate(need(!results()$any_incoherent, tr$alert_incoherent))
      p <- make_forest_plot(results(), tr)
      validate(need(!is.null(p), "No data"))
      p
    })

    output$plot_confusion <- renderPlot({
      req(results())
      res <- results()
      tr <- get_translations(lang(), "confusion_interaccion_graphics")
      dfc <- data.frame(Tipo = c(tr$lbl_crude, tr$lbl_adj), Valor = c(res$cruda, res$ajustada))
      dfc$Tipo <- factor(dfc$Tipo, levels = c(tr$lbl_crude, tr$lbl_adj))
      ggplot(dfc, aes(x = Tipo, y = Valor, fill = Tipo)) +
        geom_col(width = 0.55, show.legend = FALSE) +
        scale_fill_manual(values = c("#9ecae1", "#3182bd")) +
        labs(y = res$tipo, x = "", title = tr$confusion_plot_title) +
        theme_minimal(base_size = 13)
    })

    output$tabla_resultados <- renderDT({
      req(results())
      res <- results()
      tr <- get_translations(lang(), "confusion_interaccion_graphics")

      df <- data.frame(
        Medida = c(paste(res$tipo, tr$lbl_crude), paste(res$tipo, tr$lbl_adj), tr$s1, tr$s2, "M-H"),
        Valor = c(res$cruda, res$ajustada, res$e1, res$e2, res$mh_est),
        IC_Inf = c(NA, NA, res$e1_low, res$e2_low, res$mh_low),
        IC_Sup = c(NA, NA, res$e1_high, res$e2_high, res$mh_high)
      )
      colnames(df) <- c(tr$lbl_est, tr$lbl_est, tr$lbl_low, tr$lbl_high) # actually Measure, Est, Low, High
      colnames(df) <- c("Medida", tr$lbl_est, tr$lbl_low, tr$lbl_high)

      datatable(df, options = list(dom = "t", ordering = FALSE), rownames = FALSE) %>%
        formatRound(columns = 2:4, digits = 3)
    })

    output$interpretacion_html <- renderUI({
      req(results())
      res <- results()
      tr <- get_translations(lang(), "confusion_interaccion_graphics")

      conf_text <- if (abs((res$ajustada - res$cruda) / res$cruda) * 100 > 10) tr$conf_yes else tr$conf_no
      inter_text <- if (!is.na(res$pval)) (if (res$pval < 0.05) tr$inter_yes else tr$inter_no) else "NA"

      mh_txt <- if (res$mh_ok) paste0("M-H (", tr$mh_source, ": ", res$mh_source, ") = ", round(res$mh_est, 3)) else tr$mh_auto_failed

      HTML(paste0("<b>Confusión:</b> ", conf_text, "<br/><b>Interacción:</b> ", inter_text, "<br/><em>", mh_txt, "</em>"))
    })

    output$notaNA <- renderUI({
      tr <- get_translations(lang(), "confusion_interaccion_graphics")
      HTML(paste0("<em>", tr$note_graphics, "</em>"))
    })

    observeEvent(input$leyenda_btn, {
      tr <- get_translations(lang(), "confusion_interaccion_graphics")
      showModal(modalDialog(
        title = tr$btn_info_legend,
        tags$p(tr$modal_body),
        tags$pre(tr$modal_formula),
        tags$p(tr$modal_p),
        easyClose = TRUE, footer = NULL
      ))
    })

    # Manual PDF Download removed in favor of unified global print

    # ========= Print View Generator =========
    output$print_view <- renderUI({
      req(results())
      res <- results()
      tr <- get_translations(lang(), "confusion_interaccion_graphics")
      ns <- session$ns

      tagList(
        div(class = "printable-section", h3(tr$title)),
        div(class = "printable-section", h4(tr$h_res), DTOutput(ns("tabla_res_print"))),
        div(class = "printable-section", h4(tr$forest_title), plotOutput(ns("plot_forest_print"), height = "350px")),
        div(class = "printable-section", h4(tr$confusion_plot_title), plotOutput(ns("plot_confusion_print"), height = "350px"))
      )
    })

    output$tabla_res_print <- renderDT({
      req(results())
      res <- results()
      tr <- get_translations(lang(), "confusion_interaccion_graphics")
      df <- data.frame(M = c("Cruda", "Ajustada", "S1", "S2", "MH"), V = c(res$cruda, res$ajustada, res$e1, res$e2, res$mh_est))
      datatable(df, options = list(dom = "t"), rownames = FALSE)
    })

    output$plot_forest_print <- renderPlot({
      req(results())
      tr <- get_translations(lang(), "confusion_interaccion_graphics")
      make_forest_plot(results(), tr)
    })

    output$plot_confusion_print <- renderPlot({
      req(results())
      res <- results()
      tr <- get_translations(lang(), "confusion_interaccion_graphics")
      dfc <- data.frame(Tipo = c(tr$lbl_crude, tr$lbl_adj), Valor = c(res$cruda, res$ajustada))
      ggplot(dfc, aes(x = Tipo, y = Valor, fill = Tipo)) +
        geom_col() +
        theme_minimal()
    })

    outputOptions(output, "print_view", suspendWhenHidden = FALSE)
    outputOptions(output, "tabla_res_print", suspendWhenHidden = FALSE)
    outputOptions(output, "plot_forest_print", suspendWhenHidden = FALSE)
    outputOptions(output, "plot_confusion_print", suspendWhenHidden = FALSE)
  })
}
