library(shiny)

# ---------- Module UI ----------
estudios_ecologicos_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        h4(id = ns("h_data_agg"), "Datos agregados por exposición"),
        numericInput(ns("a"), "Casos en expuestos (a)", value = 130, min = 0, step = 1),
        numericInput(ns("pte"), "Persona-tiempo expuestos (PT_e)", value = 42000, min = 0, step = 1),
        numericInput(ns("c"), "Casos en no expuestos (c)", value = 90, min = 0, step = 1),
        numericInput(ns("pt0"), "Persona-tiempo no expuestos (PT_0)", value = 48000, min = 0, step = 1),
        hr(),
        sliderInput(ns("alpha"), "Nivel de confianza", min = 0.80, max = 0.99, value = 0.95, step = 0.01),
        numericInput(ns("base"), "Base de tasas (por ... pt)", value = 100000, min = 1, step = 1),
        hr(),
        h4(id = ns("h_pop_params"), "Parámetros poblacionales"),
        helpText(id = ns("help_pop"), "Si no indicas Pe, se usa la proporción de PT expuesta."),
        numericInput(ns("Pe_pct"), "Pe (%, proporción expuesta en la población)", value = NA, min = 0, max = 100, step = 0.1)
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            title = span(id = ns("tab_A"), "A) Tasas y asociaciones"),
            br(),
            h4(id = ns("h_rates"), "Tasas de incidencia"),
            tableOutput(ns("tab_rates")),
            hr(),
            h4(id = ns("h_assoc"), "RTI (IRR) y Δ de tasas"),
            tableOutput(ns("tab_assoc"))
          ),
          tabPanel(
            title = span(id = ns("tab_B"), "B) Modelo aditivo vs multiplicativo"),
            br(),
            h4(id = ns("h_pred_pop"), "Predicción poblacional y fracciones atribuibles"),
            tableOutput(ns("tab_models")),
            hr(),
            h4(id = ns("h_cases_attr"), "Casos atribuibles esperados"),
            tableOutput(ns("tab_cases"))
          ),
          tabPanel(
            title = span(id = ns("tab_C"), "C) Extensión Poisson — SMR"),
            br(),
            helpText(id = ns("help_smr"), "SMR = O/E. E = I0 * PT_total (basal no expuesto)."),
            fluidRow(
              column(4, numericInput(ns("O"), "O (casos observados totales)", value = 220, min = 0, step = 1)),
              column(4, numericInput(ns("E_override"), "E (esperados, opcional)", value = NA, min = 0, step = 1)),
              column(4, helpText(id = ns("help_E"), "Si no das E, se calcula con I0 estimado y PT_total."))
            ),
            hr(),
            tableOutput(ns("tab_smr"))
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
estudios_ecologicos_Server <- function(id, lang) {
  moduleServer(id, function(input, output, session) {
    observeEvent(lang(), {
      tr <- get_translations(lang(), "estudios_ecologicos")

      # UI Updates
      updateNumericInput(session, "a", label = tr$lbl_a)
      updateNumericInput(session, "pte", label = tr$lbl_pte)
      updateNumericInput(session, "c", label = tr$lbl_c)
      updateNumericInput(session, "pt0", label = tr$lbl_pt0)

      updateSliderInput(session, "alpha", label = tr$lbl_alpha)
      updateNumericInput(session, "base", label = tr$lbl_base)
      updateNumericInput(session, "Pe_pct", label = tr$lbl_pe)

      updateNumericInput(session, "O", label = tr$lbl_O)
      updateNumericInput(session, "E_override", label = tr$lbl_E)

      # Static UI
      ids_map <- list(
        "h_data_agg" = "h_data_agg",
        "h_pop_params" = "h_pop_params",
        "help_pop" = "help_pop",
        "tab_A" = "tab_A",
        "h_rates" = "h_rates",
        "h_assoc" = "h_assoc",
        "tab_B" = "tab_B",
        "h_pred_pop" = "h_pred_pop",
        "h_cases_attr" = "h_cases_attr",
        "tab_C" = "tab_C",
        "help_smr" = "help_smr",
        "help_E" = "help_E"
      )
      update_static_ui(session, ids_map, tr, session$ns)
    })

    # ========= Utilidades =========
    fmt_num <- function(x, k = 3) ifelse(is.na(x), NA, sprintf(paste0("%.", k, "f"), x))
    fmt_pct <- function(x) ifelse(is.na(x), NA, sprintf("%.2f%%", 100 * x))
    fmt_rate <- function(x, base = 1000) ifelse(is.na(x), NA, sprintf("%.3f", x * base)) # Value only, simpler for table cells

    inv_abs <- function(x) ifelse(is.na(x) || x == 0, NA_real_, 1 / abs(x))

    rates <- function(a, pte, c, pt0) {
      Ie <- if (pte > 0) a / pte else NA
      I0 <- if (pt0 > 0) c / pt0 else NA
      It <- if ((pte + pt0) > 0) (a + c) / (pte + pt0) else NA
      list(Ie = Ie, I0 = I0, It = It)
    }

    # IRR = Ie/I0 con IC log-normal (Poisson); Haldane-Anscombe si hay ceros
    irr_ci <- function(a, c, Ie, I0, level = 0.95) {
      aa <- ifelse(a == 0, a + 0.5, a)
      cc <- ifelse(c == 0, c + 0.5, c)
      irr <- if (!is.na(Ie) && !is.na(I0) && I0 > 0) Ie / I0 else NA
      z <- qnorm(1 - (1 - level) / 2)
      se <- if (!any(is.na(c(aa, cc))) && aa > 0 && cc > 0) sqrt(1 / aa + 1 / cc) else NA
      lo <- if (!is.na(irr) && !is.na(se)) exp(log(irr) - z * se) else NA
      hi <- if (!is.na(irr) && !is.na(se)) exp(log(irr) + z * se) else NA
      list(irr = irr, lo = lo, hi = hi)
    }

    # Diferencia de tasas y su IC (aprox. normal: Var ≈ a/pte^2 + c/pt0^2)
    rate_diff_ci <- function(a, pte, c, pt0, Ie, I0, level = 0.95) {
      rd <- if (!is.na(Ie) && !is.na(I0)) Ie - I0 else NA
      var <- if (all(pte > 0, pt0 > 0)) (as.numeric(a) / (pte^2) + as.numeric(c) / (pt0^2)) else NA
      se <- if (!is.na(var)) sqrt(var) else NA
      z <- qnorm(1 - (1 - level) / 2)
      lo <- if (!is.na(rd) && !is.na(se)) rd - z * se else NA
      hi <- if (!is.na(rd) && !is.na(se)) rd + z * se else NA
      list(rd = rd, lo = lo, hi = hi)
    }

    # FAP bajo modelos aditivo y multiplicativo
    fap_additive <- function(Pe, RD, It) { # It = I0 + Pe*RD
      if (any(is.na(c(Pe, RD, It))) || It <= 0) {
        return(NA_real_)
      }
      (Pe * RD) / It
    }
    fap_multiplicative <- function(Pe, IRR) {
      if (any(is.na(c(Pe, IRR)))) {
        return(NA_real_)
      }
      Pe * (IRR - 1) / (1 + Pe * (IRR - 1))
    }

    # SMR (O/E) con IC de Byar (aprox. exacta de Poisson)
    smr_ci <- function(O, E, level = 0.95) {
      if (any(is.na(c(O, E))) || E <= 0 || O < 0) {
        return(c(NA, NA, NA))
      }
      SMR <- O / E
      if (O == 0) {
        lo <- 0
        hi <- -log(1 - level) / E # límite superior exacto cuando O=0
        return(c(SMR, lo, hi))
      }
      z <- qnorm(1 - (1 - level) / 2)
      lo <- SMR * exp(-z / sqrt(O))
      hi <- SMR * exp(z / sqrt(O))
      c(SMR, lo, hi)
    }

    core <- reactive({
      a <- as.integer(input$a)
      c <- as.integer(input$c)
      pte <- as.numeric(input$pte)
      pt0 <- as.numeric(input$pt0)
      base <- as.integer(input$base)
      r <- rates(a, pte, c, pt0)
      PTtot <- pte + pt0
      Pe_in <- if (!is.na(input$Pe_pct)) input$Pe_pct / 100 else NA
      Pe <- if (!is.na(Pe_in)) Pe_in else if (PTtot > 0) pte / PTtot else NA

      irr <- irr_ci(a, c, r$Ie, r$I0, level = input$alpha)
      rd <- rate_diff_ci(a, pte, c, pt0, r$Ie, r$I0, level = input$alpha)

      list(a = a, c = c, pte = pte, pt0 = pt0, PTtot = PTtot, base = base, Pe = Pe, r = r, irr = irr, rd = rd)
    })

    # A) Tasas
    output$tab_rates <- renderTable({
      tr <- get_translations(lang(), "estudios_ecologicos")
      k <- core()
      df <- data.frame(
        item = c(tr$row_rate_e, tr$row_rate_0, tr$row_rate_t, tr$row_pe),
        val = c(
          fmt_rate(k$r$Ie, k$base),
          fmt_rate(k$r$I0, k$base),
          fmt_rate(k$r$It, k$base),
          fmt_pct(k$Pe)
        ),
        check.names = FALSE
      )
      names(df) <- c(tr$col_measure, tr$col_value)
      df
    })

    # A) Asociaciones
    output$tab_assoc <- renderTable({
      tr <- get_translations(lang(), "estudios_ecologicos")
      k <- core()
      df <- data.frame(
        item = c(
          tr$row_irr, tr$row_irr_lo, tr$row_irr_hi,
          tr$row_rd, # "Δ tasa = Ie − I0"
          tr$row_rd_lo,
          tr$row_rd_hi
        ),
        val = c(
          fmt_num(k$irr$irr), fmt_num(k$irr$lo), fmt_num(k$irr$hi),
          fmt_rate(k$rd$rd, k$base),
          fmt_rate(k$rd$lo, k$base),
          fmt_rate(k$rd$hi, k$base)
        ),
        check.names = FALSE
      )
      names(df) <- c(tr$col_measure, tr$col_value)
      df
    })

    # B) Modelos poblacionales
    output$tab_models <- renderTable({
      tr <- get_translations(lang(), "estudios_ecologicos")
      k <- core()
      Ie <- k$r$Ie
      I0 <- k$r$I0
      It_obs <- k$r$It
      Pe <- k$Pe
      IRR <- k$irr$irr
      RD <- k$rd$rd
      PTtot <- k$PTtot

      # Predicción poblacional bajo cada modelo
      It_add <- if (!any(is.na(c(I0, Pe, RD)))) I0 + Pe * RD else NA
      It_mul <- if (!any(is.na(c(I0, Pe, IRR)))) I0 * ((1 - Pe) + Pe * IRR) else NA

      # FAP en cada modelo (magnitud de exceso respecto a basal)
      FAP_add <- fap_additive(Pe, RD, It_add)
      FAP_mul <- fap_multiplicative(Pe, IRR)

      df <- data.frame(
        mod = c(tr$mod_add_desc, tr$mod_mul_desc, tr$mod_obs),
        rate = c(fmt_rate(It_add, k$base), fmt_rate(It_mul, k$base), fmt_rate(It_obs, k$base)),
        fap = c(fmt_pct(FAP_add), fmt_pct(FAP_mul), NA),
        check.names = FALSE
      )
      names(df) <- c(tr$col_model, tr$col_rate_tot, tr$col_fap)
      df
    })

    output$tab_cases <- renderTable({
      tr <- get_translations(lang(), "estudios_ecologicos")
      k <- core()
      Ie <- k$r$Ie
      I0 <- k$r$I0
      Pe <- k$Pe
      IRR <- k$irr$irr
      RD <- k$rd$rd
      PTtot <- k$PTtot

      It_add <- if (!any(is.na(c(I0, Pe, RD)))) I0 + Pe * RD else NA
      It_mul <- if (!any(is.na(c(I0, Pe, IRR)))) I0 * ((1 - Pe) + Pe * IRR) else NA

      # Casos totales esperados
      E_add <- if (!is.na(It_add) && PTtot > 0) It_add * PTtot else NA
      E_mul <- if (!is.na(It_mul) && PTtot > 0) It_mul * PTtot else NA
      E_obs <- if (PTtot > 0) (k$a + k$c) else NA

      # Casos atribuibles (exceso) vs basal en cada modelo
      A_add <- if (!is.na(E_add) && PTtot > 0) (It_add - I0) * PTtot else NA
      A_mul <- if (!is.na(E_mul) && PTtot > 0) (It_mul - I0) * PTtot else NA

      df <- data.frame(
        mod = c(tr$mod_add, tr$mod_mul, tr$mod_obs),
        exp = c(fmt_num(E_add, 1), fmt_num(E_mul, 1), fmt_num(E_obs, 1)),
        att = c(fmt_num(A_add, 1), fmt_num(A_mul, 1), NA),
        check.names = FALSE
      )
      names(df) <- c(tr$col_model, tr$col_cases_exp, tr$col_cases_attr)
      df
    })

    # C) SMR
    output$tab_smr <- renderTable({
      tr <- get_translations(lang(), "estudios_ecologicos")
      k <- core()
      I0 <- k$r$I0
      PTtot <- k$PTtot
      O <- as.numeric(input$O)
      E_user <- if (!is.na(input$E_override)) input$E_override else NA
      E_calc <- if (!is.na(I0) && PTtot > 0) I0 * PTtot else NA
      E <- if (!is.na(E_user)) E_user else E_calc

      pack <- smr_ci(O, E, level = input$alpha)
      SMR <- pack[1]
      lo <- pack[2]
      hi <- pack[3]

      df <- data.frame(
        item = c(tr$row_O, tr$row_E, tr$row_SMR, tr$row_smr_lo, tr$row_smr_hi),
        val = c(fmt_num(O, 0), fmt_num(E, 0), fmt_num(SMR), fmt_num(lo), fmt_num(hi)),
        check.names = FALSE
      )
      names(df) <- c(tr$col_measure, tr$col_value)
      df
    })

    # ===== Print Template =====
    output$tab_rates_print <- renderTable({
      req(lang())
      k <- core()
      data.frame(Medida = c("Ie", "I0", "It", "Pe"), Valor = c(fmt_rate(k$r$Ie, k$base), fmt_rate(k$r$I0, k$base), fmt_rate(k$r$It, k$base), fmt_pct(k$Pe)), check.names = FALSE)
    })
    output$tab_assoc_print <- renderTable({
      req(lang())
      k <- core()
      data.frame(
        Medida = c("IRR", "IC inf", "IC sup", "RD", "RD IC inf", "RD IC sup"),
        Valor = c(fmt_num(k$irr$irr), fmt_num(k$irr$lo), fmt_num(k$irr$hi), fmt_rate(k$rd$rd, k$base), fmt_rate(k$rd$lo, k$base), fmt_rate(k$rd$hi, k$base)), check.names = FALSE
      )
    })
    output$tab_models_print <- renderTable({
      req(lang())
      k <- core()
      Ie <- k$r$Ie
      I0 <- k$r$I0
      Pe <- k$Pe
      IRR <- k$irr$irr
      RD <- k$rd$rd
      It_add <- if (!any(is.na(c(I0, Pe, RD)))) I0 + Pe * RD else NA
      It_mul <- if (!any(is.na(c(I0, Pe, IRR)))) I0 * ((1 - Pe) + Pe * IRR) else NA
      FAP_add <- fap_additive(Pe, RD, It_add)
      FAP_mul <- fap_multiplicative(Pe, IRR)
      data.frame(Modelo = c("Aditivo", "Multiplicativo"), Tasa = c(fmt_rate(It_add, k$base), fmt_rate(It_mul, k$base)), FAP = c(fmt_pct(FAP_add), fmt_pct(FAP_mul)), check.names = FALSE)
    })
    output$tab_cases_print <- renderTable({
      req(lang())
      k <- core()
      I0 <- k$r$I0
      Pe <- k$Pe
      IRR <- k$irr$irr
      RD <- k$rd$rd
      PTtot <- k$PTtot
      It_add <- if (!any(is.na(c(I0, Pe, RD)))) I0 + Pe * RD else NA
      It_mul <- if (!any(is.na(c(I0, Pe, IRR)))) I0 * ((1 - Pe) + Pe * IRR) else NA
      E_add <- if (!is.na(It_add) && PTtot > 0) It_add * PTtot else NA
      E_mul <- if (!is.na(It_mul) && PTtot > 0) It_mul * PTtot else NA
      data.frame(Modelo = c("Aditivo", "Multiplicativo"), Esperados = c(fmt_num(E_add, 1), fmt_num(E_mul, 1)), check.names = FALSE)
    })
    output$tab_smr_print <- renderTable({
      req(lang())
      k <- core()
      I0 <- k$r$I0
      PTtot <- k$PTtot
      O <- as.numeric(input$O)
      E_user <- if (!is.na(input$E_override)) input$E_override else NA
      E_calc <- if (!is.na(I0) && PTtot > 0) I0 * PTtot else NA
      E <- if (!is.na(E_user)) E_user else E_calc
      pack <- smr_ci(O, E, level = input$alpha)
      data.frame(Medida = c("O", "E", "SMR", "IC inf", "IC sup"), Valor = c(fmt_num(O, 0), fmt_num(E, 0), fmt_num(pack[1]), fmt_num(pack[2]), fmt_num(pack[3])), check.names = FALSE)
    })

    output$print_view <- renderUI({
      req(lang())
      ns <- session$ns
      tr <- get_translations(lang(), "estudios_ecologicos")
      tagList(
        div(
          class = "printable-section",
          h3(tr$h_data_agg),
          p(paste0("a=", input$a, ", PT_e=", input$pte, ", c=", input$c, ", PT_0=", input$pt0))
        ),
        div(class = "printable-section", h4(tr$h_rates), tableOutput(ns("tab_rates_print"))),
        div(class = "printable-section", h4(tr$h_assoc), tableOutput(ns("tab_assoc_print"))),
        div(class = "printable-section", h4(tr$h_pred_pop), tableOutput(ns("tab_models_print"))),
        div(class = "printable-section", h4(tr$h_cases_attr), tableOutput(ns("tab_cases_print"))),
        div(class = "printable-section", h4("SMR"), tableOutput(ns("tab_smr_print")))
      )
    })

    outputOptions(output, "print_view", suspendWhenHidden = FALSE)
    outputOptions(output, "tab_rates_print", suspendWhenHidden = FALSE)
    outputOptions(output, "tab_assoc_print", suspendWhenHidden = FALSE)
    outputOptions(output, "tab_models_print", suspendWhenHidden = FALSE)
    outputOptions(output, "tab_cases_print", suspendWhenHidden = FALSE)
    outputOptions(output, "tab_smr_print", suspendWhenHidden = FALSE)
  })
}
