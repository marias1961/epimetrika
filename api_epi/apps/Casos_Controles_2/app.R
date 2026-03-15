library(shiny)

# ---------- Module UI ----------
casos_controles_2_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        h4(id = ns("title_effect"), "Efecto (obligatorio)"),
        numericInput(ns("OR"), "OR puntual (≈ RR si rara)", value = 2.10, min = 0, step = 0.001),
        numericInput(ns("OR_lo"), "OR — IC95 inferior", value = 1.40, min = 0, step = 0.001),
        numericInput(ns("OR_hi"), "OR — IC95 superior", value = 3.10, min = 0, step = 0.001),
        hr(),
        h4(id = ns("title_props"), "Proporciones agregadas"),
        numericInput(ns("Pce_pct"), "Pce (%, proporción de CASOS expuestos)", value = 60, min = 0, max = 100, step = 0.1),
        numericInput(ns("Pcte_pct"), "Pcte (%, proporción de CONTROLES expuestos)", value = 30, min = 0, max = 100, step = 0.1),
        numericInput(ns("Pe_pct"), "Pe (%, proporción expuestos en la población)", value = NA, min = 0, max = 100, step = 0.1),
        hr(),
        h4(id = ns("title_inc"), "Incidencia poblacional (opcional)"),
        numericInput(ns("Ipop_pct"), "Iₚ (%, incidencia del evento en la población)", value = NA, min = 0, max = 100, step = 0.001)
      ),
      mainPanel(
        h4(id = ns("res_effect"), "Resumen del efecto"),
        tableOutput(ns("tab_effect")),
        hr(),
        h4(id = ns("res_fap"), "Fracciones atribuibles (con IC95 desde IC del OR/RR)"),
        tableOutput(ns("tab_fap")),
        hr(),
        h4(id = ns("res_numbers"), "Números de impacto"),
        tableOutput(ns("tab_numbers")),
        hr(),
        h4(id = ns("res_inc"), "Incidencias estimadas (si aportas Iₚ y Pe)"),
        tableOutput(ns("tab_inc"))
      )
    ),
    div(
      class = "print-template",
      uiOutput(ns("print_view"))
    )
  )
}

# ---------- Module Server ----------
casos_controles_2_Server <- function(id, lang) {
  moduleServer(id, function(input, output, session) {
    # NEW: I18N Logic
    observeEvent(lang(), {
      tr <- get_translations(lang(), "casos_controles_2")

      # Inputs
      updateNumericInput(session, "OR", label = tr$inp_or)
      updateNumericInput(session, "OR_lo", label = tr$inp_or_lo)
      updateNumericInput(session, "OR_hi", label = tr$inp_or_hi)
      updateNumericInput(session, "Pce_pct", label = tr$inp_pce)
      updateNumericInput(session, "Pcte_pct", label = tr$inp_pcte)
      updateNumericInput(session, "Pe_pct", label = tr$inp_pe)
      updateNumericInput(session, "Ipop_pct", label = tr$inp_ipop)

      # Static UI mappings
      ids_map <- list(
        "title_effect" = "title_effect",
        "title_props" = "title_props",
        "title_inc" = "title_inc",
        "res_effect" = "res_effect",
        "res_fap" = "res_fap",
        "res_numbers" = "res_numbers",
        "res_inc" = "res_inc"
      )

      update_static_ui(session, ids_map, tr, session$ns)
    })

    # ========= Utilidades =========
    fmt_num <- function(x, k = 3) ifelse(is.na(x), NA, sprintf(paste0("%.", k, "f"), x))
    fmt_pct <- function(x) ifelse(is.na(x), NA, sprintf("%.2f%%", 100 * x))
    pc <- function(x) x / 100
    inv_abs <- function(x) ifelse(is.na(x) || x == 0, NA_real_, 1 / abs(x))

    # FAE y FAP
    fae_from_rr <- function(RR) (RR - 1) / RR
    fap_from_rr_pe <- function(RR, Pe) Pe * (RR - 1) / (1 + Pe * (RR - 1))
    fap_from_pce_rr <- function(Pce, RR) Pce * (RR - 1) / RR

    # IC por extremos desde IC(RR) (o IC(OR) si RR≈OR)
    fae_ci_from_rr_ci <- function(RR_lo, RR_hi) {
      if (any(is.na(RR_lo), is.na(RR_hi))) {
        return(c(NA, NA))
      }
      lo <- fae_from_rr(RR_lo)
      hi <- fae_from_rr(RR_hi)
      c(min(lo, hi), max(lo, hi))
    }
    fap_pe_ci_from_rr_ci <- function(RR_lo, RR_hi, Pe) {
      if (any(is.na(RR_lo), is.na(RR_hi), is.na(Pe))) {
        return(c(NA, NA))
      }
      lo <- fap_from_rr_pe(RR_lo, Pe)
      hi <- fap_from_rr_pe(RR_hi, Pe)
      c(min(lo, hi), max(lo, hi))
    }
    fap_pce_ci_from_rr_ci <- function(RR_lo, RR_hi, Pce) {
      if (any(is.na(RR_lo), is.na(RR_hi), is.na(Pce))) {
        return(c(NA, NA))
      }
      lo <- fap_from_pce_rr(Pce, RR_lo)
      hi <- fap_from_pce_rr(Pce, RR_hi)
      c(min(lo, hi), max(lo, hi))
    }

    # Si hay I_p (incidencia poblacional), Pe y RR, se puede recuperar I0 y Re
    i0_re_from_ipop <- function(Ipop, Pe, RR) {
      if (any(is.na(Ipop), is.na(Pe), is.na(RR))) {
        return(c(NA, NA))
      }
      denom <- (1 - Pe) + Pe * RR
      if (denom <= 0) {
        return(c(NA, NA))
      }
      I0 <- Ipop / denom
      Re <- RR * I0
      c(I0, Re)
    }

    rr_pack <- reactive({
      list(RR = input$OR, lo = input$OR_lo, hi = input$OR_hi) # RR≈OR por defecto
    })

    output$tab_effect <- renderTable({
      tr <- get_translations(lang(), "casos_controles_2")
      rp <- rr_pack()
      data.frame(
        m = c(tr$or_label, tr$ci_lo, tr$ci_hi),
        v = c(fmt_num(rp$RR), fmt_num(rp$lo), fmt_num(rp$hi)),
        check.names = FALSE
      ) |> setNames(c(tr$measure, tr$value))
    })

    output$tab_fap <- renderTable({
      tr <- get_translations(lang(), "casos_controles_2")
      rp <- rr_pack()
      RR <- rp$RR
      RR_lo <- rp$lo
      RR_hi <- rp$hi

      Pce <- if (!is.na(input$Pce_pct)) pc(input$Pce_pct) else NA
      Pcte <- if (!is.na(input$Pcte_pct)) pc(input$Pcte_pct) else NA
      Pe <- if (!is.na(input$Pe_pct)) pc(input$Pe_pct) else NA
      if (is.na(Pe) && !is.na(Pcte)) Pe <- Pcte # aproximación clásica si controles representativos

      FAE <- fae_from_rr(RR)
      FAE_ci <- fae_ci_from_rr_ci(RR_lo, RR_hi)
      FAP_Pe <- if (!is.na(Pe)) fap_from_rr_pe(RR, Pe) else NA
      FAP_Pe_ci <- if (!is.na(Pe)) fap_pe_ci_from_rr_ci(RR_lo, RR_hi, Pe) else c(NA, NA)
      FAP_Pce <- if (!is.na(Pce)) fap_from_pce_rr(Pce, RR) else NA
      FAP_Pce_ci <- if (!is.na(Pce)) fap_pce_ci_from_rr_ci(RR_lo, RR_hi, Pce) else c(NA, NA)
      FAP_Pcte <- if (!is.na(Pcte)) fap_from_rr_pe(RR, Pcte) else NA
      FAP_Pcte_ci <- if (!is.na(Pcte)) fap_pe_ci_from_rr_ci(RR_lo, RR_hi, Pcte) else c(NA, NA)

      data.frame(
        v = c(tr$fae_label, tr$fap_pe_label, tr$fap_pce_label, tr$fap_pcte_label),
        est = c(fmt_pct(FAE), fmt_pct(FAP_Pe), fmt_pct(FAP_Pce), fmt_pct(FAP_Pcte)),
        icl = c(fmt_pct(FAE_ci[1]), fmt_pct(FAP_Pe_ci[1]), fmt_pct(FAP_Pce_ci[1]), fmt_pct(FAP_Pcte_ci[1])),
        ich = c(fmt_pct(FAE_ci[2]), fmt_pct(FAP_Pe_ci[2]), fmt_pct(FAP_Pce_ci[2]), fmt_pct(FAP_Pcte_ci[2])),
        check.names = FALSE
      ) |> setNames(c(tr$variant, tr$estimate, tr$ci_lo, tr$ci_hi))
    })

    output$tab_numbers <- renderTable({
      tr <- get_translations(lang(), "casos_controles_2")
      rp <- rr_pack()
      RR <- rp$RR
      Pce <- if (!is.na(input$Pce_pct)) pc(input$Pce_pct) else NA
      Pcte <- if (!is.na(input$Pcte_pct)) pc(input$Pcte_pct) else NA
      Pe <- if (!is.na(input$Pe_pct)) pc(input$Pe_pct) else NA
      if (is.na(Pe) && !is.na(Pcte)) Pe <- Pcte

      FAE <- fae_from_rr(RR)
      FAP <- if (!is.na(Pe)) fap_from_rr_pe(RR, Pe) else if (!is.na(Pce)) fap_from_pce_rr(Pce, RR) else NA

      NIE <- inv_abs(FAE)
      NIC <- inv_abs(FAP)
      tipo <- if (is.na(RR)) "NNT(E)/NNT(Pop)" else if (RR > 1) tr$nie_damage else tr$niep_prevention

      data.frame(
        m = c(tipo, tr$nic_label),
        v = c(
          ifelse(is.na(NIE), NA, sprintf("%.1f", NIE)),
          ifelse(is.na(NIC), NA, sprintf("%.1f", NIC))
        ),
        check.names = FALSE
      ) |> setNames(c(tr$measure, tr$value))
    })

    output$tab_inc <- renderTable({
      tr <- get_translations(lang(), "casos_controles_2")
      rp <- rr_pack()
      RR <- rp$RR
      Pe <- if (!is.na(input$Pe_pct)) pc(input$Pe_pct) else if (!is.na(input$Pcte_pct)) pc(input$Pcte_pct) else NA
      Ipop <- if (!is.na(input$Ipop_pct)) pc(input$Ipop_pct) else NA

      rec <- i0_re_from_ipop(Ipop, Pe, RR)
      I0 <- rec[1]
      Re <- rec[2]
      ARR_pop <- if (!any(is.na(c(Ipop, I0)))) Ipop - I0 else NA

      data.frame(
        m = c(tr$inc_pop, tr$pe_used, tr$inc_0_rec, tr$re_rec, tr$arr_pop),
        v = c(fmt_pct(Ipop), fmt_pct(Pe), fmt_pct(I0), fmt_pct(Re), fmt_pct(ARR_pop)),
        check.names = FALSE
      ) |> setNames(c(tr$measure, tr$value))
    })

    # ===== Print Template =====
    output$tab_effect_print <- renderTable({
      req(lang())
      tr <- get_translations(lang(), "casos_controles_2")
      rp <- rr_pack()
      req(rp)
      data.frame(Medida = c(tr$or_label, tr$ci_lo, tr$ci_hi), Valor = c(fmt_num(rp$RR), fmt_num(rp$lo), fmt_num(rp$hi)), check.names = FALSE)
    })
    output$tab_fap_print <- renderTable({
      req(lang())
      tr <- get_translations(lang(), "casos_controles_2")
      rp <- rr_pack()
      RR <- rp$RR
      RR_lo <- rp$lo
      RR_hi <- rp$hi
      Pce <- if (!is.na(input$Pce_pct)) pc(input$Pce_pct) else NA
      Pe <- if (!is.na(input$Pe_pct)) pc(input$Pe_pct) else if (!is.na(input$Pcte_pct)) pc(input$Pcte_pct) else NA
      FAE <- fae_from_rr(RR)
      FAP_Pe <- if (!is.na(Pe)) fap_from_rr_pe(RR, Pe) else NA
      FAP_Pce <- if (!is.na(Pce)) fap_from_pce_rr(Pce, RR) else NA
      data.frame(Medida = c("FAE", "FAP(Pe)", "FAP(Pce)"), Valor = c(fmt_pct(FAE), fmt_pct(FAP_Pe), fmt_pct(FAP_Pce)), check.names = FALSE)
    })
    output$tab_numbers_print <- renderTable({
      req(lang())
      tr <- get_translations(lang(), "casos_controles_2")
      rp <- rr_pack()
      RR <- rp$RR
      FAE <- fae_from_rr(RR)
      NIE <- inv_abs(FAE)
      data.frame(Medida = c("NNT(E)"), Valor = c(ifelse(is.na(NIE), NA, sprintf("%.1f", NIE))), check.names = FALSE)
    })
    output$tab_inc_print <- renderTable({
      req(lang())
      tr <- get_translations(lang(), "casos_controles_2")
      rp <- rr_pack()
      RR <- rp$RR
      Pe <- if (!is.na(input$Pe_pct)) pc(input$Pe_pct) else if (!is.na(input$Pcte_pct)) pc(input$Pcte_pct) else NA
      Ipop <- if (!is.na(input$Ipop_pct)) pc(input$Ipop_pct) else NA
      rec <- i0_re_from_ipop(Ipop, Pe, RR)
      data.frame(Medida = c("Ip", "I0", "Re"), Valor = c(fmt_pct(Ipop), fmt_pct(rec[1]), fmt_pct(rec[2])), check.names = FALSE)
    })

    output$print_view <- renderUI({
      ns <- session$ns
      tr <- get_translations(lang(), "casos_controles_2")
      tagList(
        div(
          class = "printable-section",
          h3(tr$title_effect),
          p(paste0("OR=", input$OR, " [", input$OR_lo, ", ", input$OR_hi, "]"))
        ),
        div(class = "printable-section", h4(tr$res_effect), tableOutput(ns("tab_effect_print"))),
        div(class = "printable-section", h4(tr$res_fap), tableOutput(ns("tab_fap_print"))),
        div(class = "printable-section", h4(tr$res_numbers), tableOutput(ns("tab_numbers_print"))),
        div(class = "printable-section", h4(tr$res_inc), tableOutput(ns("tab_inc_print")))
      )
    })

    outputOptions(output, "print_view", suspendWhenHidden = FALSE)
    outputOptions(output, "tab_effect_print", suspendWhenHidden = FALSE)
    outputOptions(output, "tab_fap_print", suspendWhenHidden = FALSE)
    outputOptions(output, "tab_numbers_print", suspendWhenHidden = FALSE)
    outputOptions(output, "tab_inc_print", suspendWhenHidden = FALSE)
  })
}
