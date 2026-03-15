library(shiny)

# ---------- Module UI ----------
estudios_transversales_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        sliderInput(ns("alpha"), "Nivel de confianza", min = 0.80, max = 0.99, value = 0.95, step = 0.01),
        checkboxInput(ns("chi_cont"), "χ² con corrección de continuidad", TRUE),
        hr(),
        h4(id = ns("h_nomenclature"), "Nomenclatura de tabla"),
        helpText(id = ns("help_nomenclature"), "Exposición (E) por filas; Enfermedad (D) por columnas:"),
        verbatimTextOutput(ns("txt_nomenclature"))
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            title = span(id = ns("tab_A"), "A) 2×2 (transversal)"),
            br(),
            fluidRow(
              column(3, numericInput(ns("a"), "a (exp. y enfermo)", value = 50, min = 0, step = 1)),
              column(3, numericInput(ns("b"), "b (exp. y sano)", value = 150, min = 0, step = 1)),
              column(3, numericInput(ns("c"), "c (no exp. y enfermo)", value = 30, min = 0, step = 1)),
              column(3, numericInput(ns("d"), "d (no exp. y sano)", value = 170, min = 0, step = 1))
            ),
            hr(),
            h4(id = ns("h_prev_assoc"), "Prevalencias y asociaciones"),
            tableOutput(ns("tab_xsec")),
            hr(),
            h4(id = ns("h_sig_tests"), "Pruebas de significación"),
            tableOutput(ns("tab_tests"))
          ),
          tabPanel(
            title = span(id = ns("tab_B"), "B) Tamaño muestral (proporción)"),
            br(),
            fluidRow(
              column(3, numericInput(ns("p_pct"), "Prevalencia esperada p (%)", value = 20, min = 0, max = 100, step = 0.1)),
              column(3, numericInput(ns("d_pct"), "Precisión d (%)", value = 5, min = 0.01, max = 100, step = 0.1)),
              column(3, numericInput(ns("loss_pct"), "Pérdidas previstas (%)", value = 10, min = 0, max = 90, step = 0.1)),
              column(3, helpText(id = ns("help_nsize"), "Devuelve tamaño muestral mínimo (corregido por pérdidas)."))
            ),
            hr(),
            h4(id = ns("h_result"), "Resultado"),
            tableOutput(ns("tab_nsize"))
          ),
          tabPanel(
            title = span(id = ns("tab_C"), "C) RP desde OR"),
            br(),
            fluidRow(
              column(4, numericInput(ns("OR"), "OR (modelo logístico)", value = 1.80, min = 0, step = 0.001)),
              column(4, numericInput(ns("P0_pct"), "Prevalencia en no expuestos P0 (%)", value = 15, min = 0, max = 100, step = 0.1)),
              column(4, helpText(id = ns("help_rp_or"), "Convierte OR→RP (Zhang & Yu)."))
            ),
            hr(),
            tableOutput(ns("tab_rp_or"))
          ),
          tabPanel(
            title = span(id = ns("tab_D"), "D) RP desde β, SE y P0"),
            br(),
            fluidRow(
              column(3, numericInput(ns("beta"), "β (coef. logístico, log-OR)", value = log(1.8), step = 0.001)),
              column(3, numericInput(ns("se_beta"), "SE(β) (opcional, para IC)", value = 0.20, step = 0.001)),
              column(3, numericInput(ns("P0b_pct"), "P0 (%)", value = 15, min = 0, max = 100, step = 0.1)),
              column(3, helpText(id = ns("help_rp_beta"), "RP ≈ Zhang & Yu aplicado a OR=exp(β). Si hay SE, muestra IC."))
            ),
            hr(),
            tableOutput(ns("tab_rp_beta"))
          )
        )
      )
    ),
    div(
      class = "print-template",
      uiOutput(ns("print_view"))
    )
  )
}

# ---------- Module Server ----------
estudios_transversales_Server <- function(id, lang) {
  moduleServer(id, function(input, output, session) {
    # NEW: I18N Logic
    observeEvent(lang(), {
      tr <- get_translations(lang(), "estudios_transversales")

      # Standard Inputs
      updateSliderInput(session, "alpha", label = tr$lbl_alpha)
      updateCheckboxInput(session, "chi_cont", label = tr$lbl_chi_cont)

      updateNumericInput(session, "a", label = tr$lbl_a)
      updateNumericInput(session, "b", label = tr$lbl_b)
      updateNumericInput(session, "c", label = tr$lbl_c)
      updateNumericInput(session, "d", label = tr$lbl_d)

      updateNumericInput(session, "p_pct", label = tr$lbl_p_pct)
      updateNumericInput(session, "d_pct", label = tr$lbl_d_pct)
      updateNumericInput(session, "loss_pct", label = tr$lbl_loss_pct)

      updateNumericInput(session, "OR", label = tr$lbl_OR)
      updateNumericInput(session, "P0_pct", label = tr$lbl_P0_pct)

      updateNumericInput(session, "beta", label = tr$lbl_beta)
      updateNumericInput(session, "se_beta", label = tr$lbl_se_beta)
      updateNumericInput(session, "P0b_pct", label = tr$lbl_P0b_pct)

      # Static UI mappings
      ids_map <- list(
        "h_nomenclature" = "h_nomenclature",
        "help_nomenclature" = "help_nomenclature",
        "tab_A" = "tab_A",
        "h_prev_assoc" = "h_prev_assoc",
        "h_sig_tests" = "h_sig_tests",
        "tab_B" = "tab_B",
        "help_nsize" = "help_nsize",
        "h_result" = "h_result",
        "tab_C" = "tab_C",
        "help_rp_or" = "help_rp_or",
        "tab_D" = "tab_D",
        "help_rp_beta" = "help_rp_beta"
      )

      update_static_ui(session, ids_map, tr, session$ns)
    })

    output$txt_nomenclature <- renderText({
      get_translations(lang(), "estudios_transversales")$pre_diagram
    })

    # ========= Utilidades =========
    pc <- function(x) x / 100
    fmt_pct <- function(x) ifelse(is.na(x), NA, sprintf("%.2f%%", 100 * x))
    fmt_num <- function(x, k = 3) ifelse(is.na(x), NA, sprintf(paste0("%.", k, "f"), x))

    # IC Wilson para p = x/n
    wilson_ci <- function(x, n, level = 0.95) {
      if (is.na(x) || is.na(n) || n <= 0) {
        return(c(NA, NA))
      }
      z <- qnorm(1 - (1 - level) / 2)
      p <- x / n
      den <- 1 + z^2 / n
      ctr <- (p + z^2 / (2 * n)) / den
      half <- (z / den) * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))
      c(ctr - half, ctr + half)
    }

    # IC para diferencia de prevalencias (Wald)
    dp_ci <- function(px, nx, py, ny, level = 0.95) {
      if (any(nx <= 0, ny <= 0)) {
        return(c(NA, NA))
      }
      z <- qnorm(1 - (1 - level) / 2)
      se <- sqrt(px * (1 - px) / nx + py * (1 - py) / ny)
      diff <- px - py
      c(diff - z * se, diff + z * se)
    }

    # IC para razón de prevalencias RP = Pe/P0 (log-normal tipo RR/Woolf)
    rp_ci <- function(px, py, nx, ny, level = 0.95) {
      # px = prev en expuestos; py = prev en no expuestos
      if (any(is.na(c(px, py, nx, ny))) || any(nx <= 0, ny <= 0) || px %in% c(0, 1) || py %in% c(0, 1)) {
        return(c(NA, NA, NA))
      }
      # reconstruimos conteos aproximados
      ax <- round(px * nx)
      bx <- nx - ax
      ay <- round(py * ny)
      by <- ny - ay
      # Haldane-Anscombe
      ax <- ifelse(ax == 0, ax + 0.5, ax)
      bx <- ifelse(bx == 0, bx + 0.5, bx)
      ay <- ifelse(ay == 0, ay + 0.5, ay)
      by <- ifelse(by == 0, by + 0.5, by)
      Re <- ax / (ax + bx)
      R0 <- ay / (ay + by)
      RP <- Re / R0
      se <- sqrt(1 / ax - 1 / (ax + bx) + 1 / ay - 1 / (ay + by))
      z <- qnorm(1 - (1 - level) / 2)
      c(RP, exp(log(RP) - z * se), exp(log(RP) + z * se))
    }

    # OR de prevalencias (misma forma que OR de 2x2) e IC (Woolf)
    or_ci <- function(a, b, c, d, level = 0.95) {
      aa <- ifelse(a == 0, a + 0.5, a)
      bb <- ifelse(b == 0, b + 0.5, b)
      cc <- ifelse(c == 0, c + 0.5, c)
      dd <- ifelse(d == 0, d + 0.5, d)
      OR <- (aa * dd) / (bb * cc)
      se <- sqrt(1 / aa + 1 / bb + 1 / cc + 1 / dd)
      z <- qnorm(1 - (1 - level) / 2)
      c(OR, exp(log(OR) - z * se), exp(log(OR) + z * se))
    }

    # Tamaño muestral para estimar una proporción (dos colas)
    # n_bruto = z^2 * p*(1-p) / d^2 ; n_corregido = n_bruto / (1 - perdidas)
    n_size_prop <- function(p, d, level = 0.95, losses = 0) {
      z <- qnorm(1 - (1 - level) / 2)
      n0 <- (z^2) * p * (1 - p) / (d^2)
      if (!is.na(losses) && losses > 0 && losses < 1) n0 / (1 - losses) else n0
    }

    # Aproximación RP desde OR (Zhang & Yu)
    # RP ≈ OR / ((1 - P0) + P0*OR)
    rp_from_or_zhang <- function(OR, P0) {
      if (any(is.na(OR), is.na(P0))) {
        return(NA_real_)
      }
      OR / ((1 - P0) + P0 * OR)
    }

    # Aproximación RP desde beta (log-OR) y SE (modelo logístico) + P0
    # Primero OR = exp(beta); RP ≈ Zhang & Yu.
    rp_from_beta_se <- function(beta, se_beta, P0, level = 0.95) {
      if (any(is.na(beta), is.na(P0))) {
        return(c(NA, NA, NA))
      }
      OR <- exp(beta)
      RP <- rp_from_or_zhang(OR, P0)
      if (is.na(se_beta)) {
        return(c(RP, NA, NA))
      }
      z <- qnorm(1 - (1 - level) / 2)
      OR_lo <- exp(beta - z * se_beta)
      OR_hi <- exp(beta + z * se_beta)
      lo <- rp_from_or_zhang(OR_lo, P0)
      hi <- rp_from_or_zhang(OR_hi, P0)
      c(RP, min(lo, hi), max(lo, hi))
    }

    # ----- A) 2x2 -----
    output$tab_xsec <- renderTable(
      {
        tr <- get_translations(lang(), "estudios_transversales")
        req(input$a, input$b, input$c, input$d)
        a <- as.integer(input$a)
        b <- as.integer(input$b)
        c <- as.integer(input$c)
        d <- as.integer(input$d)
        nx <- a + b
        ny <- c + d
        N <- nx + ny

        # Prevalencias
        Pexp <- if (nx > 0) a / nx else NA
        P0 <- if (ny > 0) c / ny else NA
        Ptot <- if (N > 0) (a + c) / N else NA

        Pexp_ci <- wilson_ci(a, nx, level = input$alpha)
        P0_ci <- wilson_ci(c, ny, level = input$alpha)
        Ptot_ci <- wilson_ci(a + c, N, level = input$alpha)

        # Diferencia de prevalencias
        DP <- if (!any(is.na(c(Pexp, P0)))) Pexp - P0 else NA
        DP_ci <- dp_ci(Pexp, nx, P0, ny, level = input$alpha)

        # RP e IC
        RP_pack <- rp_ci(Pexp, P0, nx, ny, level = input$alpha)
        RP <- RP_pack[1]
        RP_lo <- RP_pack[2]
        RP_hi <- RP_pack[3]

        # OR de prevalencias e IC
        OR_pack <- or_ci(a, b, c, d, level = input$alpha)
        OR <- OR_pack[1]
        OR_lo <- OR_pack[2]
        OR_hi <- OR_pack[3]

        df <- data.frame(
          Measure = c(
            tr$row_pexp, tr$row_p0, tr$row_ptot,
            tr$row_dp,
            tr$row_rp, tr$row_or_prev
          ),
          Estimate = c(
            fmt_pct(Pexp), fmt_pct(P0), fmt_pct(Ptot),
            fmt_pct(DP), fmt_num(RP), fmt_num(OR)
          ),
          Lower = c(
            fmt_pct(Pexp_ci[1]), fmt_pct(P0_ci[1]), fmt_pct(Ptot_ci[1]),
            fmt_pct(DP_ci[1]), fmt_num(RP_lo), fmt_num(OR_lo)
          ),
          Upper = c(
            fmt_pct(Pexp_ci[2]), fmt_pct(P0_ci[2]), fmt_pct(Ptot_ci[2]),
            fmt_pct(DP_ci[2]), fmt_num(RP_hi), fmt_num(OR_hi)
          ),
          check.names = FALSE
        )

        names(df) <- c(tr$col_measure, tr$col_estimate, tr$col_ci_lo, tr$col_ci_hi)
        df
      },
      sanitize.text.function = function(x) x
    )

    output$tab_tests <- renderTable({
      tr <- get_translations(lang(), "estudios_transversales")
      req(input$a, input$b, input$c, input$d)
      a <- as.integer(input$a)
      b <- as.integer(input$b)
      c <- as.integer(input$c)
      d <- as.integer(input$d)
      m <- matrix(c(a, b, c, d), nrow = 2, byrow = TRUE)
      chi <- suppressWarnings(chisq.test(m, correct = isTRUE(input$chi_cont)))
      fis <- fisher.test(m)

      df <- data.frame(
        Test = c(tr$test_chi, tr$test_fisher),
        Statistic = c(ifelse(is.null(chi$statistic), NA, unname(chi$statistic)), NA),
        GL = c(ifelse(is.null(chi$parameter), NA, unname(chi$parameter)), NA),
        P_value = c(chi$p.value, fis$p.value),
        check.names = FALSE
      )

      names(df) <- c(tr$col_test, tr$col_statistic, tr$col_df, tr$col_pval)
      df
    })

    # ----- B) Tamaño muestral -----
    output$tab_nsize <- renderTable({
      tr <- get_translations(lang(), "estudios_transversales")
      p <- pc(input$p_pct)
      d <- pc(input$d_pct)
      L <- pc(input$loss_pct)
      n <- n_size_prop(p, d, level = input$alpha, losses = L)

      df <- data.frame(
        Param = c(tr$row_prev_exp, tr$row_precision, tr$row_losses, tr$row_conf_level, tr$row_n_corrected),
        Value = c(
          fmt_pct(p), fmt_pct(d), fmt_pct(L), sprintf("%.1f%%", 100 * input$alpha),
          ifelse(is.na(n), NA, sprintf("%.0f", ceiling(n)))
        ),
        check.names = FALSE
      )
      names(df) <- c(tr$col_param, tr$col_value)
      df
    })

    # ----- C) RP desde OR -----
    output$tab_rp_or <- renderTable({
      tr <- get_translations(lang(), "estudios_transversales")
      OR <- input$OR
      P0 <- pc(input$P0_pct)
      RP <- rp_from_or_zhang(OR, P0)

      df <- data.frame(
        Measure = c(tr$row_or_log, tr$row_p0_val, tr$row_rp_zhang),
        Value = c(fmt_num(OR), fmt_pct(P0), fmt_num(RP)),
        check.names = FALSE
      )
      names(df) <- c(tr$col_measure, tr$col_value)
      df
    })

    # ----- D) RP desde β, SE y P0 -----
    output$tab_rp_beta <- renderTable({
      tr <- get_translations(lang(), "estudios_transversales")
      beta <- input$beta
      se_b <- if (!is.na(input$se_beta)) input$se_beta else NA
      P0 <- pc(input$P0b_pct)
      pack <- rp_from_beta_se(beta, se_b, P0, level = input$alpha)

      df <- data.frame(
        Measure = c(
          tr$row_beta, tr$row_se_beta, tr$row_or_exp,
          tr$row_p0_d, tr$row_rp_zy, tr$row_ci_lo_rp, tr$row_ci_hi_rp
        ),
        Value = c(
          fmt_num(beta), fmt_num(se_b), fmt_num(exp(beta)),
          fmt_pct(P0), fmt_num(pack[1]), fmt_num(pack[2]), fmt_num(pack[3])
        ),
        check.names = FALSE
      )
      names(df) <- c(tr$col_measure, tr$col_value)
      df
    })

    # ===== Print Template =====
    output$tab_xsec_print <- renderTable({
      req(lang())
      tr <- get_translations(lang(), "estudios_transversales")
      req(input$a, input$b, input$c, input$d)
      a <- as.integer(input$a)
      b <- as.integer(input$b)
      c <- as.integer(input$c)
      d <- as.integer(input$d)
      nx <- a + b
      ny <- c + d
      N <- nx + ny
      Pexp <- if (nx > 0) a / nx else NA
      P0 <- if (ny > 0) c / ny else NA
      Ptot <- if (N > 0) (a + c) / N else NA
      RP_pack <- rp_ci(Pexp, P0, nx, ny, level = input$alpha)
      OR_pack <- or_ci(a, b, c, d, level = input$alpha)
      df <- data.frame(
        Medida = c(tr$row_pexp, tr$row_p0, tr$row_ptot, tr$row_rp, tr$row_or_prev),
        Valor = c(fmt_pct(Pexp), fmt_pct(P0), fmt_pct(Ptot), fmt_num(RP_pack[1]), fmt_num(OR_pack[1])), check.names = FALSE
      )
      colnames(df) <- c(tr$col_measure, tr$col_value)
      df
    })
    output$tab_tests_print <- renderTable(
      {
        req(lang())
        tr <- get_translations(lang(), "estudios_transversales")
        req(input$a, input$b, input$c, input$d)
        a <- as.integer(input$a)
        b <- as.integer(input$b)
        c <- as.integer(input$c)
        d <- as.integer(input$d)
        m <- matrix(c(a, b, c, d), nrow = 2, byrow = TRUE)
        chi <- suppressWarnings(chisq.test(m, correct = isTRUE(input$chi_cont)))
        fis <- fisher.test(m)
        df <- data.frame(Prueba = c(tr$test_chi, tr$test_fisher), p = c(chi$p.value, fis$p.value), check.names = FALSE)
        colnames(df) <- c(tr$col_test, tr$col_pval)
        df
      },
      digits = 4
    )
    output$tab_nsize_print <- renderTable({
      req(lang())
      tr <- get_translations(lang(), "estudios_transversales")
      p <- pc(input$p_pct)
      d <- pc(input$d_pct)
      L <- pc(input$loss_pct)
      n <- n_size_prop(p, d, level = input$alpha, losses = L)
      df <- data.frame(Param = c(tr$row_prev_exp, tr$row_precision, tr$row_losses, tr$row_n_corrected), Valor = c(fmt_pct(p), fmt_pct(d), fmt_pct(L), ifelse(is.na(n), NA, sprintf("%.0f", ceiling(n)))), check.names = FALSE)
      colnames(df) <- c(tr$col_param, tr$col_value)
      df
    })
    output$tab_rp_or_print <- renderTable({
      req(lang())
      tr <- get_translations(lang(), "estudios_transversales")
      OR <- input$OR
      P0 <- pc(input$P0_pct)
      RP <- rp_from_or_zhang(OR, P0)
      df <- data.frame(Medida = c(tr$row_or_log, tr$row_p0_val, tr$row_rp_zhang), Valor = c(fmt_num(OR), fmt_pct(P0), fmt_num(RP)), check.names = FALSE)
      colnames(df) <- c(tr$col_measure, tr$col_value)
      df
    })
    output$tab_rp_beta_print <- renderTable({
      req(lang())
      tr <- get_translations(lang(), "estudios_transversales")
      beta <- input$beta
      se_b <- if (!is.na(input$se_beta)) input$se_beta else NA
      P0 <- pc(input$P0b_pct)
      pack <- rp_from_beta_se(beta, se_b, P0, level = input$alpha)
      df <- data.frame(Medida = c(tr$row_beta, "OR", "RP", tr$row_ci_lo_rp, tr$row_ci_hi_rp), Valor = c(fmt_num(beta), fmt_num(exp(beta)), fmt_num(pack[1]), fmt_num(pack[2]), fmt_num(pack[3])), check.names = FALSE)
      colnames(df) <- c(tr$col_measure, tr$col_value)
      df
    })

    output$print_view <- renderUI({
      ns <- session$ns
      tr <- get_translations(lang(), "estudios_transversales")
      tagList(
        div(
          class = "printable-section",
          h3(tr$h_nomenclature),
          p(paste0("a=", input$a, ", b=", input$b, ", c=", input$c, ", d=", input$d))
        ),
        div(class = "printable-section", h4(tr$h_prev_assoc), tableOutput(ns("tab_xsec_print"))),
        div(class = "printable-section", h4(tr$h_sig_tests), tableOutput(ns("tab_tests_print"))),
        div(class = "printable-section", h4(tr$h_result), tableOutput(ns("tab_nsize_print"))),
        div(class = "printable-section", h4("RP (OR)"), tableOutput(ns("tab_rp_or_print"))),
        div(class = "printable-section", h4("RP (beta)"), tableOutput(ns("tab_rp_beta_print")))
      )
    })

    outputOptions(output, "print_view", suspendWhenHidden = FALSE)
    outputOptions(output, "tab_xsec_print", suspendWhenHidden = FALSE)
    outputOptions(output, "tab_tests_print", suspendWhenHidden = FALSE)
    outputOptions(output, "tab_nsize_print", suspendWhenHidden = FALSE)
    outputOptions(output, "tab_rp_or_print", suspendWhenHidden = FALSE)
    outputOptions(output, "tab_rp_beta_print", suspendWhenHidden = FALSE)
  })
}
