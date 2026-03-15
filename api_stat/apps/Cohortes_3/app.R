library(shiny)

# ---------- Module UI ----------
cohortes_3_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        h4(id = ns("title_data"), "Datos del estudio"),
        numericInput(ns("a"), "Casos en expuestos (a)", value = 30, min = 0, step = 1),
        numericInput(ns("pte"), "Persona-tiempo expuestos (PT_e)", value = 1200, min = 0, step = 1),
        numericInput(ns("c"), "Casos en no expuestos (c)", value = 18, min = 0, step = 1),
        numericInput(ns("pt0"), "Persona-tiempo no expuestos (PT_0)", value = 1500, min = 0, step = 1),
        hr(),
        sliderInput(ns("alpha"), "Nivel de confianza", min = 0.80, max = 0.99, value = 0.95, step = 0.01),
        numericInput(ns("base"), "Base para mostrar tasas (por ... persona-tiempo)", value = 1000, min = 1, step = 1)
      ),
      mainPanel(
        h4(id = ns("h_rates"), "Tasas de incidencia"),
        tableOutput(ns("tab_rates")),
        hr(),
        h4(id = ns("h_irr"), "Razón de tasas (RTI = IRR) y prueba de hipótesis"),
        tableOutput(ns("tab_irr")),
        hr(),
        h4(id = ns("h_diff"), "Diferencia de tasas e impacto"),
        tableOutput(ns("tab_diff")),
        hr(),
        h4(id = ns("h_frac"), "Fracciones atribuibles / prevenibles"),
        tableOutput(ns("tab_frac")),
        helpText(id = ns("help_pe"), "Pe (poblacional) se estima como PT_e / (PT_e + PT_0). Para factores protectores (RTI < 1) se muestra PPE/PPP; si se prefiere, puede interpretarse PAF negativa en valor absoluto.")
      )
    ),
    div(
      class = "print-template",
      uiOutput(ns("print_view"))
    )
  )
}

# ---------- Module Server ----------
cohortes_3_Server <- function(id, lang) {
  moduleServer(id, function(input, output, session) {
    # ========= I18N Logic =========
    observeEvent(lang(), {
      tr <- get_translations(lang(), "cohortes_3")

      # Update input labels
      updateNumericInput(session, "a", label = tr$input_a)
      updateNumericInput(session, "pte", label = tr$input_pte)
      updateNumericInput(session, "c", label = tr$input_c)
      updateNumericInput(session, "pt0", label = tr$input_pt0)
      updateSliderInput(session, "alpha", label = tr$confidence_level)
      updateNumericInput(session, "base", label = tr$input_base)

      # Update static UI elements
      ids_map <- list(
        "title_data" = "title_data",
        "h_rates"    = "h_rates",
        "h_irr"      = "h_irr",
        "h_diff"     = "h_diff",
        "h_frac"     = "h_frac",
        "help_pe"    = "help_pe"
      )
      update_static_ui(session, ids_map, tr, session$ns)
    })

    # ========= Utilidades =========
    pc <- function(x) x / 100
    fmt_num <- function(x, k = 3) ifelse(is.na(x), NA, sprintf(paste0("%.", k, "f"), x))
    fmt_rate <- function(x, base = 1000, fmt = "%.3f por %d pt") ifelse(is.na(x), NA, sprintf(fmt, x * base, base))
    fmt_pct <- function(x) ifelse(is.na(x), NA, sprintf("%.2f%%", 100 * x))
    inv_abs <- function(x) ifelse(is.na(x) || x == 0, NA_real_, 1 / abs(x))

    # Incidence rates
    rates <- function(a, pte, c, pt0) {
      Ie <- if (pte > 0) a / pte else NA
      I0 <- if (pt0 > 0) c / pt0 else NA
      It <- if ((pte + pt0) > 0) (a + c) / (pte + pt0) else NA
      list(Ie = Ie, I0 = I0, It = It)
    }

    # IRR = Ie/I0, IC por log-normal (Poisson). Haldane-Anscombe para ceros.
    irr_ci <- function(a, c, Ie, I0, level = 0.95) {
      aa <- ifelse(a == 0, a + 0.5, a)
      cc <- ifelse(c == 0, c + 0.5, c)
      irr <- if (!is.na(Ie) && !is.na(I0) && I0 > 0) Ie / I0 else NA
      z <- qnorm(1 - (1 - level) / 2)
      se <- if (!any(is.na(c(aa, cc))) && aa > 0 && cc > 0) sqrt(1 / aa + 1 / cc) else NA
      lo <- if (!is.na(irr) && !is.na(se)) exp(log(irr) - z * se) else NA
      hi <- if (!is.na(irr) && !is.na(se)) exp(log(irr) + z * se) else NA
      list(irr = irr, lo = lo, hi = hi, se = se, z = z)
    }

    # Diferencia de tasas y su IC (aprox. normal, var ≈ a/pte^2 + c/pt0^2)
    rate_diff_ci <- function(a, pte, c, pt0, Ie, I0, level = 0.95) {
      rd <- if (!is.na(Ie) && !is.na(I0)) Ie - I0 else NA
      var <- if (all(pte > 0, pt0 > 0)) (a / (pte^2) + c / (pt0^2)) else NA
      se <- if (!is.na(var)) sqrt(var) else NA
      z <- qnorm(1 - (1 - level) / 2)
      lo <- if (!is.na(rd) && !is.na(se)) rd - z * se else NA
      hi <- if (!is.na(rd) && !is.na(se)) rd + z * se else NA
      list(rd = rd, lo = lo, hi = hi, se = se)
    }

    # Fracciones atribuibles/prevenibles con Pe (proporción de persona-tiempo expuesta)
    fae_from_irr <- function(IRR) (IRR - 1) / IRR
    fap_from_irr_pe <- function(IRR, Pe) Pe * (IRR - 1) / (1 + Pe * (IRR - 1))

    # Para factores protectores (IRR<1)
    ppe_from_irr <- function(IRR) 1 - IRR # prevenible en expuestos
    ppp_from_irr_pe <- function(IRR, Pe) { # prevenible poblacional
      paf <- fap_from_irr_pe(IRR, Pe)
      if (is.na(paf)) NA_real_ else -paf
    }

    # Prueba H0: IRR = 1 (z en escala log)
    irr_test <- function(a, c, irr_list) {
      if (is.na(irr_list$irr) || is.na(irr_list$se)) {
        return(list(z = NA, p = NA, chi2 = NA))
      }
      z <- (log(irr_list$irr) - 0) / irr_list$se
      p <- 2 * pnorm(-abs(z))
      list(z = z, p = p, chi2 = z^2)
    }

    # Número necesario en persona-tiempo (1/|Δ tasa|)
    nit_from_rd <- function(rd) inv_abs(rd)

    # Cálculos básicos
    core <- reactive({
      req(input$a, input$pte, input$c, input$pt0)
      a <- as.integer(input$a)
      c <- as.integer(input$c)
      pte <- as.numeric(input$pte)
      pt0 <- as.numeric(input$pt0)
      base <- as.integer(input$base)
      r <- rates(a, pte, c, pt0)
      Pe <- if ((pte + pt0) > 0) pte / (pte + pt0) else NA
      irr <- irr_ci(a, c, r$Ie, r$I0, level = input$alpha)
      rd <- rate_diff_ci(a, pte, c, pt0, r$Ie, r$I0, level = input$alpha)
      tst <- irr_test(a, c, irr)
      list(a = a, c = c, pte = pte, pt0 = pt0, base = base, Pe = Pe, rates = r, irr = irr, rd = rd, tst = tst)
    })

    # Tasas
    output$tab_rates <- renderTable({
      k <- core()
      tr <- get_translations(lang(), "cohortes_3")
      df <- data.frame(
        col1 = c(tr$rate_ie, tr$rate_i0, tr$rate_it, tr$pe_label),
        col2 = c(
          fmt_rate(k$rates$Ie, k$base, tr$rate_format),
          fmt_rate(k$rates$I0, k$base, tr$rate_format),
          fmt_rate(k$rates$It, k$base, tr$rate_format),
          fmt_pct(k$Pe)
        ),
        check.names = FALSE
      )
      setNames(df, c(tr$col_measure, tr$col_value))
    })

    # IRR + prueba
    output$tab_irr <- renderTable({
      k <- core()
      tr <- get_translations(lang(), "cohortes_3")
      df <- data.frame(
        col1 = c(tr$rti_label, tr$ci_lo, tr$ci_hi, tr$z_h0, tr$chi_sq, tr$p_value),
        col2 = c(
          fmt_num(k$irr$irr), fmt_num(k$irr$lo), fmt_num(k$irr$hi),
          fmt_num(k$tst$z), fmt_num(k$tst$chi2), fmt_num(k$tst$p, 4)
        ),
        check.names = FALSE
      )
      setNames(df, c(tr$col_measure, tr$col_value))
    })

    # Diferencia de tasas e "N persona-tiempo"
    output$tab_diff <- renderTable({
      k <- core()
      tr <- get_translations(lang(), "cohortes_3")
      NIT <- nit_from_rd(k$rd$rd)
      df <- data.frame(
        col1 = c(
          sprintf(tr$delta_rate_fmt, k$base),
          sprintf(tr$ci_lo_rate_fmt, k$base),
          sprintf(tr$ci_hi_rate_fmt, k$base),
          tr$pt_per_event
        ),
        col2 = c(
          fmt_rate(k$rd$rd, k$base, tr$rate_format),
          fmt_rate(k$rd$lo, k$base, tr$rate_format),
          fmt_rate(k$rd$hi, k$base, tr$rate_format),
          ifelse(is.na(NIT), NA, sprintf("%.1f %s", NIT, safe_tr(tr, "unit_pt_evt", "pt/evt")))
        ),
        check.names = FALSE
      )
      setNames(df, c(tr$col_measure, tr$col_value))
    })

    # Fracciones atribuibles / prevenibles
    output$tab_frac <- renderTable({
      k <- core()
      tr <- get_translations(lang(), "cohortes_3")
      IRR <- k$irr$irr
      Pe <- k$Pe
      if (!is.na(IRR) && IRR >= 1) {
        FAE <- fae_from_irr(IRR)
        FAP <- fap_from_irr_pe(IRR, Pe)
        df <- data.frame(
          col1 = c(tr$fae_formula, tr$fap_formula),
          col2 = c(fmt_pct(FAE), fmt_pct(FAP)),
          check.names = FALSE
        )
      } else {
        PPE <- if (!is.na(IRR)) ppe_from_irr(IRR) else NA
        PPP <- if (!is.na(IRR) && !is.na(Pe)) ppp_from_irr_pe(IRR, Pe) else NA
        df <- data.frame(
          col1 = c(tr$ppe_formula, tr$ppp_formula),
          col2 = c(fmt_pct(PPE), fmt_pct(PPP)),
          check.names = FALSE
        )
      }
      setNames(df, c(tr$col_measure, tr$col_value))
    })

    # ========= Print Logic =========
    output$print_view <- renderUI({
      tr <- get_translations(lang(), "cohortes_3")
      ns <- session$ns
      tagList(
        div(class = "printable-section", h3(tr$h_rates), tableOutput(ns("tab_rates"))),
        div(class = "printable-section", h3(tr$h_irr), tableOutput(ns("tab_irr"))),
        div(class = "printable-section", h3(tr$h_diff), tableOutput(ns("tab_diff"))),
        div(class = "printable-section", h3(tr$h_frac), tableOutput(ns("tab_frac")))
      )
    })
    outputOptions(output, "print_view", suspendWhenHidden = FALSE)
  })
}
