library(shiny)

# ---------- Module UI ----------
casos_controles_1_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        h4(id = ns("title_obs"), "Datos observados"),
        tagList(
          # HTML Table structure hard to translate with just ID replace if we don't wrap text.
          # But for now, let's leave the pre block or use a dynamic UI if strictly needed.
          # The user asked for "revisa". The pre block is hard-coded text.
          # I will skip translating the pre-formatted table for now as it's just labels 'Casos/Controles'.
          # Actually, I should probably replace the pre block with a small tableOutput or just accept it's "universal".
          # Better: I will use 'renderUI' for it if I want to be perfect, but let's stick to the Headers first.
        ),
        div(style = "overflow-x: auto;", tableOutput(ns("guide_table"))),
        numericInput(ns("a"), "Casos — Expuestos (a)", value = 60, min = 0, step = 1),
        numericInput(ns("b"), "Casos — No expuestos (b)", value = 40, min = 0, step = 1),
        numericInput(ns("c"), "Controles — Expuestos (c)", value = 30, min = 0, step = 1),
        numericInput(ns("d"), "Controles — No expuestos (d)", value = 70, min = 0, step = 1),
        hr(),
        sliderInput(ns("alpha"), "Nivel de confianza", min = 0.80, max = 0.99, value = 0.95, step = 0.01),
        checkboxInput(ns("continuity"), "Usar corrección de continuidad en χ²", TRUE),
        hr(),
        h4(id = ns("title_opt"), "Datos poblacionales opcionales"),
        numericInput(ns("Pe_pct"), "Pe (%, proporción expuestos en población)", value = NA, min = 0, max = 100, step = 0.1),
        numericInput(ns("Pce_pct"), "Pce (%, proporción de casos expuestos)", value = NA, min = 0, max = 100, step = 0.1),
        numericInput(ns("P0_pct"), "P0 (%, riesgo en no expuestos)", value = NA, min = 0, max = 100, step = 0.1)
      ),
      mainPanel(
        h4(id = ns("res_2x2"), "Tabla 2×2"),
        tableOutput(ns("tab2x2")),
        hr(),
        h4(id = ns("res_tests"), "Pruebas de significación"),
        tableOutput(ns("tests")),
        hr(),
        h4(id = ns("res_or"), "Odds ratio (OR) y medidas derivadas"),
        tableOutput(ns("tab_or")),
        hr(),
        h4(id = ns("res_fap"), "Fracciones atribuibles y números de impacto"),
        tableOutput(ns("tab_fap"))
      )
    ),
    div(
      class = "print-template",
      uiOutput(ns("print_view"))
    )
  )
}

# ---------- Module Server ----------
casos_controles_1_Server <- function(id, lang) {
  moduleServer(id, function(input, output, session) {
    # NEW: I18N Logic
    observeEvent(lang(), {
      tr <- get_translations(lang(), "casos_controles_1")

      # Inputs
      updateNumericInput(session, "a", label = tr$inp_cases_exp)
      updateNumericInput(session, "b", label = tr$inp_cases_unexp)
      updateNumericInput(session, "c", label = tr$inp_ctrl_exp)
      updateNumericInput(session, "d", label = tr$inp_ctrl_unexp)
      updateSliderInput(session, "alpha", label = tr$confidence_level)
      updateCheckboxInput(session, "continuity", label = tr$chk_continuity)
      updateNumericInput(session, "Pe_pct", label = tr$inp_pe)
      updateNumericInput(session, "Pce_pct", label = tr$inp_pce)
      updateNumericInput(session, "P0_pct", label = tr$inp_p0)

      # Static UI mappings
      ids_map <- list(
        "title_obs" = "title_obs",
        "title_opt" = "title_opt",
        "res_2x2" = "table_2x2",
        "res_tests" = "significance_tests",
        "res_or" = "title_or",
        "res_fap" = "title_fap"
      )

      update_static_ui(session, ids_map, tr, session$ns)
    })

    output$guide_table <- renderTable({
      tr <- get_translations(lang(), "casos_controles_1")
      data.frame(
        v1 = c(tr$cases, tr$controls),
        v2 = c("a", "c"),
        v3 = c("b", "d"),
        check.names = FALSE
      ) |> setNames(c(tr$group, tr$exposed, tr$unexposed))
    })

    # ========= Utilidades =========
    fmt_num <- function(x, k = 3) ifelse(is.na(x), NA, sprintf(paste0("%.", k, "f"), x))
    fmt_pct <- function(x) ifelse(is.na(x), NA, sprintf("%.2f%%", 100 * x))
    inv_abs <- function(x) ifelse(is.na(x) || x == 0, NA_real_, 1 / abs(x))

    # Log-OR con IC (Woolf, log-normal)
    log_or_ci <- function(a, b, c, d, level = 0.95) {
      aa <- ifelse(a == 0, a + 0.5, a)
      bb <- ifelse(b == 0, b + 0.5, b)
      cc <- ifelse(c == 0, c + 0.5, c)
      dd <- ifelse(d == 0, d + 0.5, d)
      OR <- (aa * dd) / (bb * cc)
      se <- sqrt(1 / aa + 1 / bb + 1 / cc + 1 / dd)
      z <- qnorm(1 - (1 - level) / 2)
      lo <- exp(log(OR) - z * se)
      hi <- exp(log(OR) + z * se)
      c(OR, lo, hi)
    }

    # RR aproximado desde OR y P0 (riesgo basal)
    rr_from_or_zhang <- function(OR, P0) {
      if (any(is.na(OR), is.na(P0))) {
        return(NA_real_)
      }
      OR / ((1 - P0) + P0 * OR)
    }

    # Fracciones atribuibles
    fae_from_rr <- function(RR) (RR - 1) / RR
    fap_from_rr_pe <- function(RR, Pe) Pe * (RR - 1) / (1 + Pe * (RR - 1))
    fap_from_pce_rr <- function(Pce, RR) Pce * (RR - 1) / RR

    data_reactive <- reactive({
      req(input$a, input$b, input$c, input$d)
      a <- as.integer(input$a)
      b <- as.integer(input$b)
      c <- as.integer(input$c)
      d <- as.integer(input$d)
      list(a = a, b = b, c = c, d = d)
    })

    output$tab2x2 <- renderTable(
      {
        tr <- get_translations(lang(), "casos_controles_1")
        with(data_reactive(), {
          df <- matrix(c(a, b, c, d),
            nrow = 2, byrow = TRUE,
            dimnames = list(
              Grupo = c(tr$cases, tr$controls),
              Exposicion = c(tr$exposed, tr$unexposed)
            )
          )
          as.data.frame.matrix(df)
        })
      },
      rownames = TRUE
    )

    output$tests <- renderTable({
      tr <- get_translations(lang(), "casos_controles_1")
      with(data_reactive(), {
        m <- matrix(c(a, b, c, d), nrow = 2, byrow = TRUE)
        chi <- suppressWarnings(chisq.test(m, correct = isTRUE(input$continuity)))
        fis <- fisher.test(m)

        df <- data.frame(
          p = c(tr$chi_sq, tr$fisher_ext),
          est = c(unname(chi$statistic), NA),
          gl = c(unname(chi$parameter), NA),
          pval = c(chi$p.value, fis$p.value),
          check.names = FALSE
        )
        colnames(df) <- c(tr$test, tr$statistic, tr$df, tr$p_val)
        df
      })
    })

    output$tab_or <- renderTable({
      tr <- get_translations(lang(), "casos_controles_1")
      with(data_reactive(), {
        pack <- log_or_ci(a, b, c, d, input$alpha)
        OR <- pack[1]
        lo <- pack[2]
        hi <- pack[3]

        Pe <- if (!is.na(input$Pe_pct)) input$Pe_pct / 100 else NA
        Pce <- if (!is.na(input$Pce_pct)) input$Pce_pct / 100 else NA
        P0 <- if (!is.na(input$P0_pct)) input$P0_pct / 100 else NA

        RR <- if (!is.na(P0)) rr_from_or_zhang(OR, P0) else NA

        df <- data.frame(
          m = c(
            "Odds ratio (OR)", tr$ci_inf, tr$ci_sup,
            if (!is.na(P0)) tr$rr_zhang else NULL
          ),
          v = c(
            fmt_num(OR), fmt_num(lo), fmt_num(hi),
            if (!is.na(P0)) fmt_num(RR) else NULL
          ),
          check.names = FALSE
        )
        colnames(df) <- c(tr$measure, tr$value)
        df
      })
    })

    output$tab_fap <- renderTable({
      tr <- get_translations(lang(), "casos_controles_1")
      with(data_reactive(), {
        pack <- log_or_ci(a, b, c, d, input$alpha)
        OR <- pack[1]
        Pe <- if (!is.na(input$Pe_pct)) input$Pe_pct / 100 else NA
        Pce <- if (!is.na(input$Pce_pct)) input$Pce_pct / 100 else NA
        P0 <- if (!is.na(input$P0_pct)) input$P0_pct / 100 else NA

        RR <- if (!is.na(P0)) rr_from_or_zhang(OR, P0) else NA
        RR_eff <- if (!is.na(RR)) RR else OR # fallback a OR si no hay P0

        FAE <- fae_from_rr(RR_eff)
        FAP_pe <- if (!is.na(Pe)) fap_from_rr_pe(RR_eff, Pe) else NA
        FAP_pce <- if (!is.na(Pce)) fap_from_pce_rr(Pce, RR_eff) else NA

        NIE <- if (!is.na(FAE)) inv_abs(FAE) else NA
        NIC <- if (!is.na(FAP_pce)) inv_abs(FAP_pce) else NA

        df <- data.frame(
          m = c(
            tr$fae_formula,
            tr$fap_pe_formula,
            tr$fap_pce_formula,
            tr$nie_formula,
            tr$nic_formula
          ),
          v = c(
            fmt_pct(FAE),
            fmt_pct(FAP_pe),
            fmt_pct(FAP_pce),
            ifelse(is.na(NIE), NA, sprintf("%.1f", NIE)),
            ifelse(is.na(NIC), NA, sprintf("%.1f", NIC))
          ),
          check.names = FALSE
        )
        colnames(df) <- c(tr$measure, tr$value)
        df
      })
    })

    # ===== Print Template =====
    output$tab2x2_print <- renderTable(
      {
        req(lang())
        tr <- get_translations(lang(), "casos_controles_1")
        d <- data_reactive()
        req(d)
        as.data.frame.matrix(matrix(c(d$a, d$b, d$c, d$d),
          nrow = 2, byrow = TRUE,
          dimnames = list(Grupo = c(tr$cases, tr$controls), Exposicion = c(tr$exposed, tr$unexposed))
        ))
      },
      rownames = TRUE
    )

    output$tests_print <- renderTable(
      {
        req(lang())
        tr <- get_translations(lang(), "casos_controles_1")
        d <- data_reactive()
        req(d)
        m <- matrix(c(d$a, d$b, d$c, d$d), nrow = 2, byrow = TRUE)
        chi <- suppressWarnings(chisq.test(m, correct = isTRUE(input$continuity)))
        fis <- fisher.test(m)
        data.frame(
          Pruebas = c(tr$chi_sq, tr$fisher_ext), Estadistico = c(unname(chi$statistic), NA),
          gl = c(unname(chi$parameter), NA), p = c(chi$p.value, fis$p.value), check.names = FALSE
        )
      },
      digits = 4
    )

    output$tab_or_print <- renderTable({
      req(lang())
      tr <- get_translations(lang(), "casos_controles_1")
      with(data_reactive(), {
        pack <- log_or_ci(a, b, c, d, input$alpha)
        OR <- pack[1]
        lo <- pack[2]
        hi <- pack[3]
        P0 <- if (!is.na(input$P0_pct)) input$P0_pct / 100 else NA
        RR <- if (!is.na(P0)) rr_from_or_zhang(OR, P0) else NA
        rows <- data.frame(Medida = c("OR", tr$ci_inf, tr$ci_sup), Valor = c(fmt_num(OR), fmt_num(lo), fmt_num(hi)), check.names = FALSE)
        if (!is.na(P0)) rows <- rbind(rows, data.frame(Medida = tr$rr_zhang, Valor = fmt_num(RR), check.names = FALSE))
        colnames(rows) <- c(tr$measure, tr$value)
        rows
      })
    })

    output$tab_fap_print <- renderTable({
      req(lang())
      tr <- get_translations(lang(), "casos_controles_1")
      with(data_reactive(), {
        pack <- log_or_ci(a, b, c, d, input$alpha)
        OR <- pack[1]
        Pe <- if (!is.na(input$Pe_pct)) input$Pe_pct / 100 else NA
        Pce <- if (!is.na(input$Pce_pct)) input$Pce_pct / 100 else NA
        P0 <- if (!is.na(input$P0_pct)) input$P0_pct / 100 else NA
        RR <- if (!is.na(P0)) rr_from_or_zhang(OR, P0) else NA
        RR_eff <- if (!is.na(RR)) RR else OR
        FAE <- fae_from_rr(RR_eff)
        FAP_pe <- if (!is.na(Pe)) fap_from_rr_pe(RR_eff, Pe) else NA
        FAP_pce <- if (!is.na(Pce)) fap_from_pce_rr(Pce, RR_eff) else NA
        df <- data.frame(
          Medida = c("FAE", "FAP(Pe)", "FAP(Pce)"),
          Valor = c(fmt_pct(FAE), fmt_pct(FAP_pe), fmt_pct(FAP_pce)), check.names = FALSE
        )
        colnames(df) <- c(tr$measure, tr$value)
        df
      })
    })

    output$print_view <- renderUI({
      ns <- session$ns
      tr <- get_translations(lang(), "casos_controles_1")
      tagList(
        div(
          class = "printable-section",
          h3(tr$title_obs),
          p(paste0(
            "a=", input$a, ", b=", input$b, ", c=", input$c, ", d=", input$d,
            " | ", tr$confidence_level, ": ", input$alpha * 100, "%"
          ))
        ),
        div(class = "printable-section", h4(tr$table_2x2), tableOutput(ns("tab2x2_print"))),
        div(class = "printable-section", h4(tr$significance_tests), tableOutput(ns("tests_print"))),
        div(class = "printable-section", h4(tr$title_or), tableOutput(ns("tab_or_print"))),
        div(class = "printable-section", h4(tr$title_fap), tableOutput(ns("tab_fap_print")))
      )
    })

    outputOptions(output, "print_view", suspendWhenHidden = FALSE)
    outputOptions(output, "tab2x2_print", suspendWhenHidden = FALSE)
    outputOptions(output, "tests_print", suspendWhenHidden = FALSE)
    outputOptions(output, "tab_or_print", suspendWhenHidden = FALSE)
    outputOptions(output, "tab_fap_print", suspendWhenHidden = FALSE)
  })
}
