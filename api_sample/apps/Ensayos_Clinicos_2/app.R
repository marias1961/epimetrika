library(shiny)

# ---------- Module UI ----------
ensayos_clinicos_2_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        h4(uiOutput(ns("h_glob"))),
        uiOutput(ns("help_glob_ui")),
        numericInput(ns("nE"), "nE", value = NA, min = 1, step = 1),
        numericInput(ns("nC"), "nC", value = NA, min = 1, step = 1),
        sliderInput(ns("alpha"), "Nivel de confianza", min = 0.80, max = 0.99, value = 0.95, step = 0.01),
        hr(),
        # CSS + JS for sidebar max-width
        tags$style(HTML(paste0("
          #", ns("nnt_method_selector"), " {
            /* anchor for JS below */
          }
        "))),
        tags$script(HTML(paste0("
          $(document).ready(function() {
            // Find the sidebar panel that contains the method selector and cap its width
            var el = document.getElementById('", ns("nnt_method_selector"), "');
            if (el) {
              var sidebar = el.closest('.col-sm-4, .col-sm-3, [class*=\"col-sm\"]');
              if (sidebar) {
                sidebar.style.maxWidth = '300px';
                sidebar.style.flex = '0 0 300px';
              }
            }
          });
        "))),
        tags$style(HTML(paste0("
          #", ns("nnt_method_selector"), " {
            border: 2px solid #2980b9;
            border-radius: 10px;
            padding: 0;
            margin-bottom: 10px;
            overflow: hidden;
            box-shadow: 0 2px 6px rgba(41, 128, 185, 0.12);
          }
          #", ns("nnt_method_selector"), " .method-selector-header {
            background: linear-gradient(135deg, #2980b9 0%, #3498db 100%);
            color: white;
            padding: 8px 12px;
            font-size: 0.9rem;
            font-weight: 600;
            display: flex;
            align-items: center;
            gap: 6px;
          }
          #", ns("nnt_method_selector"), " .method-selector-body {
            padding: 10px;
          }
          #", ns("nnt_method_selector"), " .shiny-options-group {
            display: flex;
            flex-direction: column;
            gap: 4px;
          }
          #", ns("nnt_method_selector"), " .radio label {
            margin: 0;
            padding: 0;
            width: 100%;
          }
          #", ns("nnt_method_selector"), " .radio input[type='radio'] {
            position: absolute;
            opacity: 0;
            pointer-events: none;
          }
          #", ns("nnt_method_selector"), " .radio label > span {
            display: flex;
            align-items: center;
            padding: 7px 10px;
            border: 2px solid #ecf0f1;
            border-radius: 7px;
            cursor: pointer;
            transition: all 0.2s ease;
            font-size: 0.85rem;
            background: white;
            color: #2c3e50;
          }
          #", ns("nnt_method_selector"), " .radio label:hover > span {
            border-color: #bdc3c7;
            background: #f8f9fa;
            transform: translateX(2px);
          }
          #", ns("nnt_method_selector"), " .radio input[type='radio']:checked + span {
            border-color: #2980b9;
            background: #ebf5fb;
            color: #2980b9;
            font-weight: 600;
            box-shadow: 0 2px 4px rgba(41, 128, 185, 0.15);
          }
          .nnt-method-icon {
            display: inline-flex;
            align-items: center;
            justify-content: center;
            width: 22px;
            height: 22px;
            border-radius: 50%;
            background: #2980b9;
            color: white;
            font-weight: 700;
            font-size: 0.75rem;
            margin-right: 8px;
            flex-shrink: 0;
          }
        "))),
        # Method selector card
        div(
          id = ns("nnt_method_selector"),
          div(
            class = "method-selector-header",
            tags$span(style = "font-size: 1.1rem;", "📊"),
            uiOutput(ns("method_selector_title"), inline = TRUE)
          ),
          div(
            class = "method-selector-body",
            uiOutput(ns("nnt_method_radio"))
          )
        )
      ),
      mainPanel(
        # Method A content
        conditionalPanel(
          condition = paste0("input['", ns("nnt_method"), "'] == 'A'"),
          br(),
          fluidRow(
            column(4, numericInput(ns("Re_pct"), "Riesgo en Tratamiento (Re, %)", value = 25, min = 0, max = 100, step = 0.1)),
            column(4, numericInput(ns("Rc_pct"), "Riesgo en Control (Rc, %)", value = 40, min = 0, max = 100, step = 0.1)),
            column(4, helpText("Introduce Re y Rc como porcentajes."))
          ),
          hr(),
          h5("IC del NNT desde IC del RR (opcional)"),
          fluidRow(
            column(4, numericInput(ns("RR"), "RR puntual (si lo tienes)", value = NA, min = 0, step = 0.001)),
            column(4, numericInput(ns("RR_lo"), "RR — Límite inferior", value = NA, min = 0, step = 0.001)),
            column(4, numericInput(ns("RR_hi"), "RR — Límite superior", value = NA, min = 0, step = 0.001))
          ),
          hr(),
          h4("Resultados"),
          tableOutput(ns("tab_A")),
          helpText("Notas: ARR = Rc - Re; RRR = 1 - RR. IC(NNT) se calcula invirtiendo IC(ARR) cuando es posible o, si aportas IC del RR, por propagación en extremos con Rc.")
        ),

        # Method B content
        conditionalPanel(
          condition = paste0("input['", ns("nnt_method"), "'] == 'B'"),
          br(),
          fluidRow(
            column(4, numericInput(ns("RR_B"), "RR", value = 0.7, min = 0, step = 0.001)),
            column(4, numericInput(ns("Rc_B_pct"), "Riesgo basal (Rc, %)", value = 20, min = 0, max = 100, step = 0.1)),
            column(4, helpText("Calcula Re = RR × Rc; ARR = Rc - Re; NNT = 1/|ARR|."))
          ),
          hr(),
          h5("IC(NNT) opcional desde IC(RR)"),
          fluidRow(
            column(6, numericInput(ns("RR_B_lo"), "RR — Límite inferior", value = NA, min = 0, step = 0.001)),
            column(6, numericInput(ns("RR_B_hi"), "RR — Límite superior", value = NA, min = 0, step = 0.001))
          ),
          hr(),
          h4("Resultados"),
          tableOutput(ns("tab_B"))
        ),

        # Method C content
        conditionalPanel(
          condition = paste0("input['", ns("nnt_method"), "'] == 'C'"),
          br(),
          fluidRow(
            column(4, numericInput(ns("OR_C"), "OR ajustada", value = 0.65, min = 0, step = 0.001)),
            column(4, numericInput(ns("Rc_C_pct"), "Riesgo basal (Rc, %)", value = 25, min = 0, max = 100, step = 0.1)),
            column(4, helpText("Convierte OR → RR (Zhang & Yu) y luego calcula NNT."))
          ),
          hr(),
          h4("Resultados"),
          tableOutput(ns("tab_C")),
          helpText("Corrección de Zhang & Yu: RR ≈ OR / ((1 − Rc) + Rc·OR). Útil cuando OR proviene de un modelo logístico.")
        ),

        # Method D content
        conditionalPanel(
          condition = paste0("input['", ns("nnt_method"), "'] == 'D'"),
          br(),
          fluidRow(
            column(4, numericInput(ns("RR_D"), "RR (del tratamiento)", value = 0.7, min = 0, step = 0.001)),
            column(4, numericInput(ns("R0_D_pct"), "Riesgo basal del paciente (R₀, %)", value = 15, min = 0, max = 100, step = 0.1)),
            column(4, helpText("Personaliza el NNT según el riesgo basal individual."))
          ),
          hr(),
          h5("Ajuste temporal (opcional)"),
          fluidRow(
            column(4, numericInput(ns("T_study"), "Duración del estudio (meses)", value = 12, min = 0.01, step = 0.1)),
            column(4, numericInput(ns("T_apply"), "Tiempo aplicable (meses)", value = 6, min = 0.01, step = 0.1)),
            column(4, helpText("Cuando T_aplicable < T_estudio, ARR se escala ≈ linealmente y NNT se multiplica por T_estudio/T_aplicable."))
          ),
          hr(),
          h4("Resultados"),
          tableOutput(ns("tab_D"))
        ),
        # Print Template (hidden on screen via CSS)
        div(
          class = "print-template",
          uiOutput(ns("print_view"))
        )
      )
    )
  )
}

# ---------- Module Server ----------
ensayos_clinicos_2_Server <- function(id, lang) {
  moduleServer(id, function(input, output, session) {
    # ========= Utilidades =========

    output$nnt_method_radio <- renderUI({
      ns <- session$ns
      tr <- get_translations(lang(), "ensayos_clinicos_2")
      names(tr$methods) <- c("A", "B", "C", "D")

      choices <- list(
        HTML(paste0("<span class='nnt-method-icon'>A</span> ", tr$methods["A"])),
        HTML(paste0("<span class='nnt-method-icon'>B</span> ", tr$methods["B"])),
        HTML(paste0("<span class='nnt-method-icon'>C</span> ", tr$methods["C"])),
        HTML(paste0("<span class='nnt-method-icon'>D</span> ", tr$methods["D"]))
      )

      radioButtons(ns("nnt_method"),
        label = NULL,
        choiceNames = choices,
        choiceValues = c("A", "B", "C", "D"),
        selected = isolate(input$nnt_method) %||% "A"
      )
    })

    observeEvent(lang(), {
      tr <- get_translations(lang(), "ensayos_clinicos_2")
      updateNumericInput(session, "nE", label = tr$lbl_ne)
      updateNumericInput(session, "nC", label = tr$lbl_nc)
      updateNumericInput(session, "Re_pct", label = tr$lbl_re)
      updateNumericInput(session, "Rc_pct", label = tr$lbl_rc)
      updateNumericInput(session, "RR", label = tr$lbl_rr)
      updateNumericInput(session, "OR_C", label = tr$lbl_or)
    })

    output$h_glob <- renderUI({
      tr <- get_translations(lang(), "ensayos_clinicos_2")
      tr$global_params
    })
    output$help_glob_ui <- renderUI({
      tr <- get_translations(lang(), "ensayos_clinicos_2")
      helpText(tr$help_global)
    })
    output$method_selector_title <- renderUI({
      tr <- get_translations(lang(), "ensayos_clinicos_2")
      tr$method_title
    })

    # ========= Utilidades =========
    pc <- function(x) x / 100
    p2txt <- function(x) ifelse(is.na(x), NA, sprintf("%.2f%%", 100 * x))
    num <- function(x, k = 3) ifelse(is.na(x), NA, sprintf(paste0("%.", k, "f"), x))
    nnt_label <- function(arr) ifelse(is.na(arr), "NNT/NNH", ifelse(arr > 0, "NNT (beneficio)", "NNH (daño)"))

    arr_ci_from_probs <- function(Re, Rc, nE, nC, level = 0.95) {
      if (any(is.na(Re), is.na(Rc))) {
        return(c(NA, NA))
      }
      z <- qnorm(1 - (1 - level) / 2)
      if (is.na(nE) || is.na(nC)) {
        return(c(NA, NA))
      }
      se <- sqrt(Re * (1 - Re) / nE + Rc * (1 - Rc) / nC)
      c((Rc - Re) - z * se, (Rc - Re) + z * se)
    }

    nnt_ci_from_arr_ci <- function(arr_lo, arr_hi) {
      if (any(is.na(arr_lo), is.na(arr_hi))) {
        return(c(NA, NA))
      }
      if (arr_lo <= 0 && arr_hi >= 0) {
        return(c(NA, NA))
      }
      lo <- 1 / abs(arr_hi)
      hi <- 1 / abs(arr_lo)
      c(min(lo, hi), max(lo, hi))
    }

    nnt_ci_from_rr_ci <- function(RR_lo, RR_hi, Rc) {
      if (any(is.na(RR_lo), is.na(RR_hi), is.na(Rc))) {
        return(c(NA, NA))
      }
      ARR_lo <- Rc * (1 - RR_hi)
      ARR_hi <- Rc * (1 - RR_lo)
      if (ARR_lo <= 0 && ARR_hi >= 0) {
        return(c(NA, NA))
      }
      lo <- 1 / abs(ARR_hi)
      hi <- 1 / abs(ARR_lo)
      c(min(lo, hi), max(lo, hi))
    }

    rr_from_or_zhang <- function(OR, Rc) {
      if (any(is.na(OR), is.na(Rc))) {
        return(NA_real_)
      }
      OR / ((1 - Rc) + Rc * OR)
    }

    # ========= Reactive Data Calculations =========

    data_A <- reactive({
      tr <- get_translations(lang(), "ensayos_clinicos_2")
      Re <- pc(input$Re_pct)
      Rc <- pc(input$Rc_pct)
      RR_pt <- if (!is.na(input$RR)) input$RR else if (!is.na(Re) && !is.na(Rc) && Rc > 0) Re / Rc else NA
      OR_pt <- if (!is.na(Re) && !is.na(Rc)) (Re / (1 - Re)) / (Rc / (1 - Rc)) else NA
      ARR <- if (!is.na(Re) && !is.na(Rc)) Rc - Re else NA
      RRR <- if (!is.na(RR_pt)) 1 - RR_pt else NA
      NNT <- if (!is.na(ARR) && ARR != 0) 1 / abs(ARR) else NA

      arr_ci <- arr_ci_from_probs(Re, Rc, nE = input$nE, nC = input$nC, level = input$alpha)
      nnt_ci_wald <- nnt_ci_from_arr_ci(arr_ci[1], arr_ci[2])
      nnt_ci_rr <- if (!any(is.na(c(input$RR_lo, input$RR_hi))) && !is.na(Rc)) nnt_ci_from_rr_ci(input$RR_lo, input$RR_hi, Rc) else c(NA, NA)

      df <- data.frame(
        x1 = c(tr$row_re, tr$row_rc, tr$row_rr, tr$row_or, tr$row_arr, tr$row_rrr, nnt_label(ARR), tr$row_ic_nnt_wald, tr$row_ic_nnt_rr),
        x2 = c(
          p2txt(Re), p2txt(Rc), num(RR_pt), num(OR_pt), p2txt(ARR), p2txt(RRR),
          ifelse(is.na(NNT), NA, sprintf("%.1f", NNT)),
          ifelse(any(is.na(nnt_ci_wald)), NA, sprintf("[%.1f, %.1f]", nnt_ci_wald[1], nnt_ci_wald[2])),
          ifelse(any(is.na(nnt_ci_rr)), NA, sprintf("[%.1f, %.1f]", nnt_ci_rr[1], nnt_ci_rr[2]))
        )
      )
      colnames(df) <- tr$cols
      df
    })

    data_B <- reactive({
      tr <- get_translations(lang(), "ensayos_clinicos_2")
      RR <- input$RR_B
      Rc <- pc(input$Rc_B_pct)
      Re <- if (!is.na(RR) && !is.na(Rc)) RR * Rc else NA
      ARR <- if (!is.na(Rc) && !is.na(Re)) Rc - Re else NA
      NNT <- if (!is.na(ARR) && ARR != 0) 1 / abs(ARR) else NA
      nnt_ci <- if (!any(is.na(c(input$RR_B_lo, input$RR_B_hi))) && !is.na(Rc)) nnt_ci_from_rr_ci(input$RR_B_lo, input$RR_B_hi, Rc) else c(NA, NA)

      df <- data.frame(
        x1 = c(tr$lbl_rr, tr$row_rc, tr$row_re_calc, tr$row_arr_calc, nnt_label(ARR), tr$row_ic_nnt_rr),
        x2 = c(
          num(RR), p2txt(Rc), p2txt(Re), p2txt(ARR), ifelse(is.na(NNT), NA, sprintf("%.1f", NNT)),
          ifelse(any(is.na(nnt_ci)), NA, sprintf("[%.1f, %.1f]", nnt_ci[1], nnt_ci[2]))
        )
      )
      colnames(df) <- tr$cols
      df
    })

    data_C <- reactive({
      tr <- get_translations(lang(), "ensayos_clinicos_2")
      OR <- input$OR_C
      Rc <- pc(input$Rc_C_pct)
      RR_zhang <- rr_from_or_zhang(OR, Rc)
      Re <- if (!is.na(RR_zhang) && !is.na(Rc)) RR_zhang * Rc else NA
      ARR <- if (!is.na(Re) && !is.na(Rc)) Rc - Re else NA
      NNT <- if (!is.na(ARR) && ARR != 0) 1 / abs(ARR) else NA

      df <- data.frame(
        x1 = c(
          safe_tr(tr, "row_or_adj", "Adj OR"),
          safe_tr(tr, "row_rr_zy", "RR (Z&Y)"),
          safe_tr(tr, "row_rc", "Rc"),
          safe_tr(tr, "row_re_zy", "Re (Z&Y)"),
          safe_tr(tr, "row_arr_calc", "ARR"),
          nnt_label(ARR)
        ),
        x2 = c(num(OR), num(RR_zhang), p2txt(Rc), p2txt(Re), p2txt(ARR), ifelse(is.na(NNT), NA, sprintf("%.1f", NNT)))
      )
      colnames(df) <- tr$cols
      df
    })

    data_D <- reactive({
      tr <- get_translations(lang(), "ensayos_clinicos_2")
      RR <- input$RR_D
      R0 <- pc(input$R0_D_pct)
      Re <- if (!is.na(RR) && !is.na(R0)) RR * R0 else NA
      ARR <- if (!is.na(R0) && !is.na(Re)) R0 - Re else NA
      NNT <- if (!is.na(ARR) && ARR != 0) 1 / abs(ARR) else NA
      T_est <- input$T_study
      T_app <- input$T_apply
      factor_time <- if (all(!is.na(c(T_est, T_app))) && T_app > 0) T_est / T_app else NA
      NNT_adj <- if (!is.na(NNT) && !is.na(factor_time)) NNT * factor_time else NA

      df <- data.frame(
        x1 = c(tr$row_rr, tr$row_r0, tr$row_re_r0, tr$row_arr_r0, nnt_label(ARR), tr$row_ft, tr$row_nnt_adj),
        x2 = c(
          num(RR), p2txt(R0), p2txt(Re), p2txt(ARR), ifelse(is.na(NNT), NA, sprintf("%.1f", NNT)),
          ifelse(is.na(factor_time), NA, num(factor_time, 2)), ifelse(is.na(NNT_adj), NA, sprintf("%.1f", NNT_adj))
        )
      )
      colnames(df) <- tr$cols
      df
    })

    # Output Bindings for Screen
    output$tab_A <- renderTable({
      data_A()
    })
    output$tab_B <- renderTable({
      data_B()
    })
    output$tab_C <- renderTable({
      data_C()
    })
    output$tab_D <- renderTable({
      data_D()
    })

    # Output Bindings for Print (Independent IDs)
    output$tab_A_print <- renderTable({
      req(lang())
      data_A()
    })
    output$tab_B_print <- renderTable({
      req(lang())
      data_B()
    })
    output$tab_C_print <- renderTable({
      req(lang())
      data_C()
    })
    output$tab_D_print <- renderTable({
      req(lang())
      data_D()
    })

    # ========= Print View Generator =========
    output$print_view <- renderUI({
      req(lang())
      ns <- session$ns
      # Determine which method is selected
      method <- input$nnt_method
      req(method) # Ensure method is not NULL
      tr <- get_translations(lang(), "ensayos_clinicos_2")

      # Build the content block based on method
      content <- tagList()

      if (method == "A") {
        content <- tagList(
          div(
            class = "printable-section",
            h4(tr$methods["A"]),
            p(sprintf("nE: %s, nC: %s, %s: %s%%", input$nE, input$nC, tr$lbl_alpha, input$alpha * 100))
          ),
          div(
            class = "printable-section",
            h4(tr$print_h_main),
            tableOutput(ns("tab_A_print"))
          ),
          div(
            class = "printable-section",
            h5(tr$print_h_notes),
            p(paste(tr$print_note_arr, tr$print_note_rrr))
          )
        )
      } else if (method == "B") {
        content <- tagList(
          div(
            class = "printable-section",
            h4(tr$methods["B"]),
            tableOutput(ns("tab_B_print"))
          )
        )
      } else if (method == "C") {
        content <- tagList(
          div(
            class = "printable-section",
            h4(tr$methods["C"]),
            tableOutput(ns("tab_C_print")),
            p(tr$help_zhang)
          )
        )
      } else if (method == "D") {
        content <- tagList(
          div(
            class = "printable-section",
            h4(tr$methods["D"]),
            tableOutput(ns("tab_D_print"))
          )
        )
      }

      content
    })

    # Force Rendering of Hidden Print Outputs
    outputOptions(output, "print_view", suspendWhenHidden = FALSE)
    outputOptions(output, "tab_A_print", suspendWhenHidden = FALSE)
    outputOptions(output, "tab_B_print", suspendWhenHidden = FALSE)
    outputOptions(output, "tab_C_print", suspendWhenHidden = FALSE)
    outputOptions(output, "tab_D_print", suspendWhenHidden = FALSE)
  })
}
