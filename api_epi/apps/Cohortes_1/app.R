library(shiny)


# ---------- Module UI ----------
# ---------- Module UI ----------
cohortes_1_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        h4(id = ns("title_data"), "Datos (conteos)"),
        helpText(id = ns("help_def"), "Definición:"),
        tags$pre("                 Enfermo   No enfermo
Expuestos (E=1)        a          b
No expuestos (E=0)     c          d"),
        numericInput(ns("a"), "Expuestos — Enfermo (a)", value = 60, min = 0, step = 1),
        numericInput(ns("b"), "Expuestos — No enfermo (b)", value = 140, min = 0, step = 1),
        numericInput(ns("c"), "No expuestos — Enfermo (c)", value = 40, min = 0, step = 1),
        numericInput(ns("d"), "No expuestos — No enfermo (d)", value = 160, min = 0, step = 1),
        hr(),
        sliderInput(ns("alpha"), "Nivel de confianza", min = 0.80, max = 0.99, value = 0.95, step = 0.01),
        checkboxInput(ns("chi_cont"), "χ² con corrección de continuidad", TRUE),
        hr(),
        h4(id = ns("title_opt"), "Opcional para FAP/RAP"),
        helpText(id = ns("help_pe"), "Si quieres, fija Pe (proporción expuestos en la población). Por defecto se usa la de la muestra."),
        numericInput(ns("Pe_override_pct"), "Pe (%, opcional)", value = NA, min = 0, max = 100, step = 0.1)
      ),
      mainPanel(
        h4(id = ns("res_2x2"), "Tabla 2×2"),
        tableOutput(ns("tab2x2")),
        hr(),
        h4(id = ns("res_tests"), "Pruebas de significación"),
        tableOutput(ns("tests")),
        hr(),
        h4(id = ns("title_risk"), "Medidas de riesgo"),
        tableOutput(ns("risk")),
        hr(),
        h4(id = ns("title_impact"), "Medidas de impacto"),
        tableOutput(ns("impact")),
        hr(),
        h4(id = ns("title_impact_num"), "Números de impacto"),
        tableOutput(ns("impact_numbers")),
        helpText(id = ns("help_nav"), "Convenciones: RAE = Re − R0 (atribuible en expuestos); RAP = riesgo atribuible poblacional; FAE y FAP son fracciones atribuibles.")
      )
    ),
    div(
      class = "print-template",
      uiOutput(ns("print_view"))
    )
  )
}

# ---------- Module Server ----------
cohortes_1_Server <- function(id, lang) {
  moduleServer(id, function(input, output, session) {
    # NEW: I18N Logic
    observeEvent(lang(), {
      tr <- get_translations(lang(), "cohortes_1")

      # Standard Inputs
      updateNumericInput(session, "a", label = tr$input_a)
      updateNumericInput(session, "b", label = tr$input_b)
      updateNumericInput(session, "c", label = tr$input_c)
      updateNumericInput(session, "d", label = tr$input_d)
      updateSliderInput(session, "alpha", label = tr$confidence_level)
      updateCheckboxInput(session, "chi_cont", label = tr$chk_continuity)
      updateNumericInput(session, "Pe_override_pct", label = tr$optional)

      # Static UI mappings
      ids_map <- list(
        "title_data" = "title_data",
        "help_def" = "help_def",
        "title_opt" = "title_opt",
        "help_pe" = "help_pe",
        "title_risk" = "title_risk",
        "title_impact" = "title_impact",
        "title_impact_num" = "title_impact_num",
        "help_nav" = "help_nav",
        "res_2x2" = "table_2x2",
        "res_tests" = "significance_tests"
      )

      update_static_ui(session, ids_map, tr, session$ns)
    })

    # ===== Utilidades =====
    fmt_pct <- function(x) ifelse(is.na(x), NA, sprintf("%.2f%%", 100 * x))
    fmt_num <- function(x, k = 3) ifelse(is.na(x), NA, sprintf(paste0("%.", k, "f"), x))

    prop_ci_wald <- function(x, n, level = 0.95) {
      if (n == 0) {
        return(c(NA, NA))
      }
      p <- x / n
      z <- qnorm(1 - (1 - level) / 2)
      se <- sqrt(p * (1 - p) / n)
      c(p - z * se, p + z * se)
    }

    # IC (Wald) para RD = p1 - p0
    rd_ci <- function(a, b, c, d, level = 0.95) {
      n1 <- a + b
      n0 <- c + d
      if (n1 == 0 || n0 == 0) {
        return(c(NA, NA))
      }
      p1 <- a / n1
      p0 <- c / n0
      z <- qnorm(1 - (1 - level) / 2)
      se <- sqrt(p1 * (1 - p1) / n1 + p0 * (1 - p0) / n0)
      c((p1 - p0) - z * se, (p1 - p0) + z * se)
    }

    # RR y OR con IC (Woolf, log-normal) + Haldane-Anscombe si hay ceros
    log_rr_ci <- function(a, b, c, d, level = 0.95) {
      aa <- ifelse(a == 0, a + 0.5, a)
      bb <- ifelse(b == 0, b + 0.5, b)
      cc <- ifelse(c == 0, c + 0.5, c)
      dd <- ifelse(d == 0, d + 0.5, d)
      p1 <- aa / (aa + bb)
      p0 <- cc / (cc + dd)
      rr <- p1 / p0
      se <- sqrt(1 / aa - 1 / (aa + bb) + 1 / cc - 1 / (cc + dd))
      z <- qnorm(1 - (1 - level) / 2)
      c(rr, exp(log(rr) - z * se), exp(log(rr) + z * se))
    }
    log_or_ci <- function(a, b, c, d, level = 0.95) {
      aa <- ifelse(a == 0, a + 0.5, a)
      bb <- ifelse(b == 0, b + 0.5, b)
      cc <- ifelse(c == 0, c + 0.5, c)
      dd <- ifelse(d == 0, d + 0.5, d)
      or <- (aa * dd) / (bb * cc)
      se <- sqrt(1 / aa + 1 / bb + 1 / cc + 1 / dd)
      z <- qnorm(1 - (1 - level) / 2)
      c(or, exp(log(or) - z * se), exp(log(or) + z * se))
    }

    # A partir de RR y Pe:
    # FAE = (RR-1)/RR -> IC por transformación de límites del RR
    fae_from_rr <- function(RR) (RR - 1) / RR
    fae_ci_from_rr_ci <- function(RR_lo, RR_hi) {
      if (any(is.na(RR_lo), is.na(RR_hi))) {
        return(c(NA, NA))
      }
      c(fae_from_rr(RR_lo), fae_from_rr(RR_hi))
    }
    # FAP = Pe*(RR-1)/(1 + Pe*(RR-1)) -> IC por extremos de RR con Pe fijo
    fap_from_rr_pe <- function(RR, Pe) Pe * (RR - 1) / (1 + Pe * (RR - 1))
    fap_ci_from_rr_ci <- function(RR_lo, RR_hi, Pe) {
      if (any(is.na(RR_lo), is.na(RR_hi), is.na(Pe))) {
        return(c(NA, NA))
      }
      lo <- fap_from_rr_pe(RR_lo, Pe)
      hi <- fap_from_rr_pe(RR_hi, Pe)
      c(min(lo, hi), max(lo, hi))
    }

    # Números de impacto (inversos, en valor absoluto)
    inv_abs <- function(x) ifelse(is.na(x) || x == 0, NA_real_, 1 / abs(x))

    dat <- reactive({
      req(input$a, input$b, input$c, input$d)
      a <- as.integer(input$a)
      b <- as.integer(input$b)
      c <- as.integer(input$c)
      d <- as.integer(input$d)
      list(a = a, b = b, c = c, d = d)
    })

    output$tab2x2 <- renderTable(
      {
        with(dat(), {
          as.data.frame.matrix(matrix(c(a, b, c, d),
            nrow = 2, byrow = TRUE,
            dimnames = list(
              Exposicion = c(tr$lbl_exposed, tr$lbl_unexposed),
              Estado = c(tr$lbl_sick, tr$lbl_healthy)
            )
          ))
        })
      },
      rownames = TRUE
    )

    output$tests <- renderTable(
      {
        with(dat(), {
          m <- matrix(c(a, b, c, d), nrow = 2, byrow = TRUE)
          chi <- suppressWarnings(chisq.test(m, correct = isTRUE(input$chi_cont)))
          fis <- fisher.test(m)
          data.frame(
            Pruebas = c(tr$chi_sq, tr$fisher_ext),
            Estadistico = c(unname(chi$statistic), NA),
            gl = c(unname(chi$parameter), NA),
            pval = c(chi$p.value, fis$p.value),
            check.names = FALSE
          )
        })
      },
      digits = 4
    )

    output$risk <- renderTable({
      tr <- get_translations(lang(), "cohortes_1")
      with(dat(), {
        n1 <- a + b
        n0 <- c + d
        N <- n1 + n0
        Re <- if (n1 > 0) a / n1 else NA
        R0 <- if (n0 > 0) c / n0 else NA
        Rt <- if (N > 0) (a + c) / N else NA

        Re_ci <- prop_ci_wald(a, n1, input$alpha)
        R0_ci <- prop_ci_wald(c, n0, input$alpha)

        rr_pack <- log_rr_ci(a, b, c, d, input$alpha)
        or_pack <- log_or_ci(a, b, c, d, input$alpha)
        RR <- rr_pack[1]
        RR_lo <- rr_pack[2]
        RR_hi <- rr_pack[3]
        OR <- or_pack[1]
        OR_lo <- or_pack[2]
        OR_hi <- or_pack[3]

        RD <- if (!is.na(Re) & !is.na(R0)) Re - R0 else NA
        RD_ci <- rd_ci(a, b, c, d, input$alpha)

        data.frame(
          Medida = c(
            tr$row_re, tr$row_r0,
            tr$row_rt, tr$row_rd,
            tr$row_rr, tr$row_or
          ),
          Estimacion = c(
            fmt_pct(Re), fmt_pct(R0),
            fmt_pct(Rt), fmt_pct(RD),
            fmt_num(RR), fmt_num(OR)
          ),
          `IC inferior` = c(
            fmt_pct(Re_ci[1]), fmt_pct(R0_ci[1]),
            NA, fmt_pct(RD_ci[1]),
            fmt_num(RR_lo), fmt_num(OR_lo)
          ),
          `IC superior` = c(
            fmt_pct(Re_ci[2]), fmt_pct(R0_ci[2]),
            NA, fmt_pct(RD_ci[2]),
            fmt_num(RR_hi), fmt_num(OR_hi)
          ),
          check.names = FALSE
        )
        names(df) <- c(tr$th_measure, tr$th_est, tr$th_lower, tr$th_upper)
        df
      })
    })

    output$impact <- renderTable({
      tr <- get_translations(lang(), "cohortes_1")
      with(dat(), {
        n1 <- a + b
        n0 <- c + d
        N <- n1 + n0
        Re <- if (n1 > 0) a / n1 else NA
        R0 <- if (n0 > 0) c / n0 else NA
        Rt <- if (N > 0) (a + c) / N else NA
        Pe_muestra <- if (N > 0) n1 / N else NA
        Pe <- if (!is.na(input$Pe_override_pct)) input$Pe_override_pct / 100 else Pe_muestra

        rr_pack <- log_rr_ci(a, b, c, d, input$alpha)
        RR <- rr_pack[1]
        RR_lo <- rr_pack[2]
        RR_hi <- rr_pack[3]

        # Medidas de impacto
        RAE <- if (!is.na(Re) & !is.na(R0)) Re - R0 else NA # en expuestos
        RAE_ci <- rd_ci(a, b, c, d, input$alpha) # mismo que RD

        RAP <- if (!is.na(Pe) & !is.na(RAE)) Pe * RAE else NA # poblacional (en riesgo absoluto)
        RAP_ci <- if (!any(is.na(c(Pe, RAE_ci)))) Pe * RAE_ci else c(NA, NA) # escala por Pe

        FAE <- if (!is.na(RR)) fae_from_rr(RR) else NA
        FAE_ci <- fae_ci_from_rr_ci(RR_lo, RR_hi)

        FAP <- if (!any(is.na(c(Pe, RR)))) fap_from_rr_pe(RR, Pe) else NA
        FAP_ci <- fap_ci_from_rr_ci(RR_lo, RR_hi, Pe)

        data.frame(
          Medida = c(
            tr$row_rae,
            tr$row_rap,
            tr$row_fae,
            tr$row_fap
          ),
          Estimacion = c(fmt_pct(RAE), fmt_pct(RAP), fmt_pct(FAE), fmt_pct(FAP)),
          `IC inferior` = c(
            fmt_pct(RAE_ci[1]), fmt_pct(RAP_ci[1]),
            fmt_pct(FAE_ci[1]), fmt_pct(FAP_ci[1])
          ),
          `IC superior` = c(
            fmt_pct(RAE_ci[2]), fmt_pct(RAP_ci[2]),
            fmt_pct(FAE_ci[2]), fmt_pct(FAP_ci[2])
          ),
          check.names = FALSE
        )
        names(df) <- c(tr$th_measure, tr$th_est, tr$th_lower, tr$th_upper)
        df
      })
    })

    output$impact_numbers <- renderTable({
      tr <- get_translations(lang(), "cohortes_1")
      with(dat(), {
        n1 <- a + b
        n0 <- c + d
        N <- n1 + n0
        Re <- if (n1 > 0) a / n1 else NA
        R0 <- if (n0 > 0) c / n0 else NA
        Pe <- if (N > 0) n1 / N else NA

        rr_pack <- log_rr_ci(a, b, c, d, input$alpha)
        RR <- rr_pack[1]

        RAE <- if (!is.na(Re) & !is.na(R0)) Re - R0 else NA
        RAP <- if (!is.na(Pe) & !is.na(RAE)) Pe * RAE else NA
        FAE <- if (!is.na(RR)) fae_from_rr(RR) else NA
        FAP <- if (!any(is.na(c(Pe, RR)))) fap_from_rr_pe(RR, Pe) else NA

        # NIE (para daño, RR>1) y NIEP (para prevención, RR<1), ambos como inverso de |RAE|
        NIE <- if (!is.na(RAE)) inv_abs(RAE) else NA
        NIP <- if (!is.na(RAP)) inv_abs(RAP) else NA
        NIC <- if (!is.na(FAP)) inv_abs(FAP) else NA
        NICE <- if (!is.na(FAE)) inv_abs(FAE) else NA

        tipo_NIE <- if (is.na(RR)) tr$row_nie else if (RR > 1) tr$row_nie_harm else tr$row_nie_prev

        data.frame(
          Medida = c(
            tipo_NIE, tr$row_nip,
            tr$row_nic, tr$row_nice
          ),
          Valor = c(
            ifelse(is.na(NIE), NA, sprintf("%.1f", NIE)),
            ifelse(is.na(NIP), NA, sprintf("%.1f", NIP)),
            ifelse(is.na(NIC), NA, sprintf("%.1f", NIC)),
            ifelse(is.na(NICE), NA, sprintf("%.1f", NICE))
          ),
          Interpretacion = c(
            tr$interp_nie,
            tr$interp_nip,
            tr$interp_nic,
            tr$interp_nice
          ),
          check.names = FALSE
        )
        names(df) <- c(tr$th_measure, tr$th_val, tr$th_interp)
        df
      })
    })

    # ===== Print Template Outputs =====
    output$tab2x2_print <- renderTable(
      {
        req(lang())
        tr <- get_translations(lang(), "cohortes_1")
        with(dat(), {
          as.data.frame.matrix(matrix(c(a, b, c, d),
            nrow = 2, byrow = TRUE,
            dimnames = list(
              Exposicion = c(tr$lbl_exposed, tr$lbl_unexposed),
              Estado = c(tr$lbl_sick, tr$lbl_healthy)
            )
          ))
        })
      },
      rownames = TRUE
    )

    output$tests_print <- renderTable(
      {
        req(lang())
        tr <- get_translations(lang(), "cohortes_1")
        with(dat(), {
          m <- matrix(c(a, b, c, d), nrow = 2, byrow = TRUE)
          chi <- suppressWarnings(chisq.test(m, correct = isTRUE(input$chi_cont)))
          fis <- fisher.test(m)
          data.frame(
            Prueba = c(tr$chi_sq, tr$fisher_ext),
            Estadistico = c(unname(chi$statistic), NA),
            gl = c(unname(chi$parameter), NA),
            p = c(chi$p.value, fis$p.value),
            check.names = FALSE
          )
        })
      },
      digits = 4
    )

    output$risk_print <- renderTable({
      req(lang())
      tr <- get_translations(lang(), "cohortes_1")
      with(dat(), {
        n1 <- a + b
        n0 <- c + d
        Re <- if (n1 > 0) a / n1 else NA
        R0 <- if (n0 > 0) c / n0 else NA
        rr_pack <- log_rr_ci(a, b, c, d, input$alpha)
        or_pack <- log_or_ci(a, b, c, d, input$alpha)
        RD <- if (!is.na(Re) & !is.na(R0)) Re - R0 else NA
        df <- data.frame(
          Medida = c("Re", "R0", "RD", "RR", "OR"),
          Valor = c(fmt_pct(Re), fmt_pct(R0), fmt_pct(RD), fmt_num(rr_pack[1]), fmt_num(or_pack[1])),
          check.names = FALSE
        )
        colnames(df) <- c(tr$th_measure, tr$th_val)
        df
      })
    })

    output$impact_print <- renderTable({
      req(lang())
      tr <- get_translations(lang(), "cohortes_1")
      with(dat(), {
        n1 <- a + b
        n0 <- c + d
        N <- n1 + n0
        Re <- if (n1 > 0) a / n1 else NA
        R0 <- if (n0 > 0) c / n0 else NA
        Pe <- if (N > 0) n1 / N else NA
        rr_pack <- log_rr_ci(a, b, c, d, input$alpha)
        RR <- rr_pack[1]
        RAE <- if (!is.na(Re) & !is.na(R0)) Re - R0 else NA
        FAE <- if (!is.na(RR)) fae_from_rr(RR) else NA
        FAP <- if (!any(is.na(c(Pe, RR)))) fap_from_rr_pe(RR, Pe) else NA
        df <- data.frame(
          Medida = c("RAE", "FAE", "FAP"),
          Valor = c(fmt_pct(RAE), fmt_pct(FAE), fmt_pct(FAP)),
          check.names = FALSE
        )
        colnames(df) <- c(tr$th_measure, tr$th_val)
        df
      })
    })

    output$impact_numbers_print <- renderTable({
      req(lang())
      tr <- get_translations(lang(), "cohortes_1")
      with(dat(), {
        n1 <- a + b
        n0 <- c + d
        N <- n1 + n0
        Re <- if (n1 > 0) a / n1 else NA
        R0 <- if (n0 > 0) c / n0 else NA
        Pe <- if (N > 0) n1 / N else NA
        rr_pack <- log_rr_ci(a, b, c, d, input$alpha)
        RR <- rr_pack[1]
        RAE <- if (!is.na(Re) & !is.na(R0)) Re - R0 else NA
        FAE <- if (!is.na(RR)) fae_from_rr(RR) else NA
        FAP <- if (!any(is.na(c(Pe, RR)))) fap_from_rr_pe(RR, Pe) else NA
        NIE <- if (!is.na(RAE)) inv_abs(RAE) else NA
        NIC <- if (!is.na(FAP)) inv_abs(FAP) else NA
        df <- data.frame(
          Medida = c("NIE", "NIC"),
          Valor = c(ifelse(is.na(NIE), NA, sprintf("%.1f", NIE)), ifelse(is.na(NIC), NA, sprintf("%.1f", NIC))),
          check.names = FALSE
        )
        colnames(df) <- c(tr$th_measure, tr$th_val)
        df
      })
    })

    output$print_view <- renderUI({
      ns <- session$ns
      tr <- get_translations(lang(), "cohortes_1")
      tagList(
        div(
          class = "printable-section",
          h3(tr$title_data),
          p(paste0(
            "a = ", input$a, ", b = ", input$b, ", c = ", input$c, ", d = ", input$d,
            " | ", tr$confidence_level, ": ", input$alpha * 100, "%"
          ))
        ),
        div(class = "printable-section", h4(tr$table_2x2), tableOutput(ns("tab2x2_print"))),
        div(class = "printable-section", h4(tr$significance_tests), tableOutput(ns("tests_print"))),
        div(class = "printable-section", h4(tr$title_risk), tableOutput(ns("risk_print"))),
        div(class = "printable-section", h4(tr$title_impact), tableOutput(ns("impact_print"))),
        div(class = "printable-section", h4(tr$title_impact_num), tableOutput(ns("impact_numbers_print")))
      )
    })

    outputOptions(output, "print_view", suspendWhenHidden = FALSE)
    outputOptions(output, "tab2x2_print", suspendWhenHidden = FALSE)
    outputOptions(output, "tests_print", suspendWhenHidden = FALSE)
    outputOptions(output, "risk_print", suspendWhenHidden = FALSE)
    outputOptions(output, "impact_print", suspendWhenHidden = FALSE)
    outputOptions(output, "impact_numbers_print", suspendWhenHidden = FALSE)
  })
}
