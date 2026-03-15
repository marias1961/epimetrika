library(shiny)

# ---------- Module UI ----------
cohortes_2_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        h4(id = ns("h_glob"), "Parámetros globales (opcionales)"),
        numericInput(ns("nE"), "nE", value = NA, min = 1, step = 1),
        numericInput(ns("n0"), "n0", value = NA, min = 1, step = 1),
        sliderInput(ns("alpha"), "Alpha", min = 0.80, max = 0.99, value = 0.95, step = 0.01),
        hr(),
        uiOutput(ns("help_tabs_ui"))
      ),
      mainPanel(
        uiOutput(ns("tabs_ui"))
      )
    ),
    div(
      class = "print-template",
      uiOutput(ns("print_view"))
    )
  )
}

# ---------- Module Server ----------
cohortes_2_Server <- function(id, lang) {
  moduleServer(id, function(input, output, session) {
    # ========= Utilidades =========
    fmt_pct <- function(x) ifelse(is.na(x), NA, sprintf("%.2f%%", 100 * x))
    fmt_num <- function(x, k = 3) ifelse(is.na(x), NA, sprintf(paste0("%.", k, "f"), x))
    pc <- function(x) x / 100
    inv_abs <- function(x) ifelse(is.na(x) || x == 0, NA_real_, 1 / abs(x))

    # Wald para p y para diferencia de riesgos (RD)
    prop_ci_wald <- function(x, n, level = 0.95) {
      if (is.na(x) || is.na(n) || n <= 0) {
        return(c(NA, NA))
      }
      p <- x / n
      z <- qnorm(1 - (1 - level) / 2)
      se <- sqrt(p * (1 - p) / n)
      c(p - z * se, p + z * se)
    }
    rd_ci_from_probs <- function(Re, R0, nE, n0, level = 0.95) {
      if (any(is.na(c(Re, R0, nE, n0))) || min(nE, n0) <= 0) {
        return(c(NA, NA))
      }
      z <- qnorm(1 - (1 - level) / 2)
      se <- sqrt(Re * (1 - Re) / nE + R0 * (1 - R0) / n0)
      c((Re - R0) - z * se, (Re - R0) + z * se)
    }

    # Woolf (log-normal) para RR con conteos aproximados (si dispones de nE,n0)
    rr_ci_from_probs <- function(Re, R0, nE, n0, level = 0.95) {
      if (any(is.na(c(Re, R0, nE, n0))) || min(nE, n0) <= 0 || Re %in% c(0, 1) || R0 %in% c(0, 1)) {
        return(c(NA, NA, NA))
      }
      a <- Re * nE
      b <- (1 - Re) * nE
      c <- R0 * n0
      d <- (1 - R0) * n0
      aa <- ifelse(a == 0, a + 0.5, a)
      bb <- ifelse(b == 0, b + 0.5, b)
      cc <- ifelse(c == 0, c + 0.5, c)
      dd <- ifelse(d == 0, d + 0.5, d)
      p1 <- aa / (aa + bb)
      p0 <- cc / (cc + dd)
      RR <- p1 / p0
      se <- sqrt(1 / aa - 1 / (aa + bb) + 1 / cc - 1 / (cc + dd))
      z <- qnorm(1 - (1 - level) / 2)
      c(RR, exp(log(RR) - z * se), exp(log(RR) + z * se))
    }

    # FAE y FAP
    fae_from_rr <- function(RR) (RR - 1) / RR
    fap_from_rr_pe <- function(RR, Pe) Pe * (RR - 1) / (1 + Pe * (RR - 1))

    # FAP desde Pce (proporción de casos expuestos): FAP = Pce*(RR-1)/RR
    fap_from_pce_rr <- function(Pce, RR) {
      if (any(is.na(Pce), is.na(RR))) {
        return(NA_real_)
      }
      Pce * (RR - 1) / RR
    }

    # IC(NNT) por inversión de IC(ARR)
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

    # Zhang & Yu: convertir OR -> RR dado R0
    rr_from_or_zhang <- function(OR, R0) {
      if (any(is.na(OR), is.na(R0))) {
        return(NA_real_)
      }
      OR / ((1 - R0) + R0 * OR)
    }

    # Recuperar RR desde Pcne y Pe (sin R0, sin Rt):
    # Pcne = (1-Pe) / ( (1-Pe) + Pe*RR )  =>  RR = ((1-Pe)*(1-Pcne))/(Pcne*Pe)
    rr_from_pcne_pe <- function(Pcne, Pe) {
      if (any(is.na(Pcne), is.na(Pe)) || Pcne <= 0 || Pe <= 0 || Pcne >= 1 || Pe >= 1) {
        return(NA_real_)
      }
      ((1 - Pe) * (1 - Pcne)) / (Pcne * Pe)
    }

    # Si tenemos Rt (prevalencia total), Pe y RR, podemos recuperar R0:
    # Rt = R0 * ((1-Pe) + Pe*RR)  =>  R0 = Rt / ((1-Pe) + Pe*RR)
    r0_from_rt_pe_rr <- function(Rt, Pe, RR) {
      if (any(is.na(Rt), is.na(Pe), is.na(RR))) {
        return(NA_real_)
      }
      Rt / ((1 - Pe) + Pe * RR)
    }

    observeEvent(lang(), {
      tr <- get_translations(lang(), "cohortes_2")
      ids_map <- list(
        "h_glob" = "title_sidebar",
        "h4_basic" = "h_measures_ci",
        "help_fap" = "help_fap",
        "h4_res_b" = "h_results",
        "h4_res_c" = "h_results",
        "h5_d1" = "h5_opt1",
        "h5_d2" = "h5_opt2",
        "help_d2" = "help_nnt2",
        "h4_res_d" = "h_results",
        "h4_res_e" = "h_results",
        "help_f" = "help_pap",
        "h4_res_f" = "h_results"
      )
      update_static_ui(session, ids_map, tr, session$ns)
      # Inputs
      updateNumericInput(session, "Re_pct", label = safe_tr(tr, "input_re", "Re (%)"))
      updateNumericInput(session, "R0_pct", label = safe_tr(tr, "input_r0", "R0 (%)"))
      updateNumericInput(session, "Pe_pct", label = safe_tr(tr, "input_pe", "Pe (%)"))
      updateNumericInput(session, "Rt_pct", label = safe_tr(tr, "input_rt", "Rt (%)"))
      updateNumericInput(session, "RR_FAP", label = safe_tr(tr, "input_rr_fap", "RR"))
      updateNumericInput(session, "Pe_FAP_pct", label = safe_tr(tr, "input_pe_fap", "Pe (%)"))
      updateNumericInput(session, "Pce_pct", label = safe_tr(tr, "input_pce", "Pce (%)"))
      updateNumericInput(session, "OR_adj", label = safe_tr(tr, "input_or_adj", "Adj OR"))
      updateNumericInput(session, "R0_or_pct", label = safe_tr(tr, "input_r0_or", "R0 (%)"))
      updateNumericInput(session, "OR_nnt", label = safe_tr(tr, "input_or_nnt", "Adj OR"))
      updateNumericInput(session, "R0_nnt_pct", label = safe_tr(tr, "input_r0_nnt", "R0 (%)"))
      updateNumericInput(session, "Pcne_pct", label = safe_tr(tr, "input_pcne", "Pcne (%)"))
      updateNumericInput(session, "Pe_pcne_pct", label = safe_tr(tr, "input_pe_pcne", "Pe (%)"))
      updateNumericInput(session, "Rt_pcne_pct", label = safe_tr(tr, "input_rt_pcne", "Rt (%)"))
      updateNumericInput(session, "R0_pcne_pct", label = safe_tr(tr, "input_r0_pcne", "R0 (%)"))
      updateNumericInput(session, "OR_nie", label = safe_tr(tr, "input_or_nie", "Adj OR"))
      updateNumericInput(session, "R0_nie_pct", label = safe_tr(tr, "input_r0_nie", "R0 (%)"))
      updateNumericInput(session, "OR_pap", label = safe_tr(tr, "input_or_pap", "Adj OR"))
      updateNumericInput(session, "R0_pap_pct", label = safe_tr(tr, "input_r0_pap", "R0 (%)"))
      updateNumericInput(session, "Pe_pap_pct", label = safe_tr(tr, "input_pe_pap", "Pe (%)"))
    })

    # ----- A) Básico -----
    output$tab_basic <- renderTable({
      tr <- get_translations(lang(), "cohortes_2")
      Re <- pc(input$Re_pct)
      R0 <- pc(input$R0_pct)
      Pe <- pc(input$Pe_pct)
      Rt <- if (!is.na(input$Rt_pct)) pc(input$Rt_pct) else ((1 - Pe) * R0 + Pe * Re)
      RR <- if (!any(is.na(c(Re, R0))) && R0 > 0) Re / R0 else NA
      RAE <- if (!any(is.na(c(Re, R0)))) Re - R0 else NA
      RAP <- if (!any(is.na(c(Pe, RAE)))) Pe * RAE else NA
      FAE <- if (!is.na(RR)) (RR - 1) / RR else NA
      FAP <- if (!any(is.na(c(Pe, RR)))) fap_from_rr_pe(RR, Pe) else NA

      # IC si tenemos tamaños
      rr_ci <- rr_ci_from_probs(Re, R0, input$nE, input$n0, input$alpha)
      rd_ci <- rd_ci_from_probs(Re, R0, input$nE, input$n0, input$alpha)
      nnt <- inv_abs(RAE)
      nnt_ci <- nnt_ci_from_arr_ci(rd_ci[1], rd_ci[2])

      df <- data.frame(
        x1 = c(
          "Re", "R0", "Pe", "Rt",
          "RR = Re/R0",
          tr$row_rae,
          tr$row_rap,
          tr$row_fae,
          tr$row_fap,
          ifelse(is.na(RAE) || RAE >= 0, tr$row_nie_d, tr$row_nie_p)
        ),
        x2 = c(
          fmt_pct(Re), fmt_pct(R0), fmt_pct(Pe), fmt_pct(Rt),
          fmt_num(RR),
          fmt_pct(RAE),
          fmt_pct(RAP),
          fmt_pct(FAE),
          fmt_pct(FAP),
          ifelse(is.na(nnt), NA, sprintf("%.1f", nnt))
        ),
        x3 = c(
          NA, NA, NA, NA,
          fmt_num(rr_ci[2]),
          fmt_pct(rd_ci[1]),
          ifelse(is.na(Pe) || is.na(rd_ci[1]), NA, fmt_pct(Pe * rd_ci[1])),
          NA, NA,
          ifelse(any(is.na(nnt_ci)), NA, sprintf("%.1f", nnt_ci[1]))
        ),
        x4 = c(
          NA, NA, NA, NA,
          fmt_num(rr_ci[3]),
          fmt_pct(rd_ci[2]),
          ifelse(is.na(Pe) || is.na(rd_ci[2]), NA, fmt_pct(Pe * rd_ci[2])),
          NA, NA,
          ifelse(any(is.na(nnt_ci)), NA, sprintf("%.1f", nnt_ci[2]))
        )
      )
      colnames(df) <- c(safe_tr(tr, "meas", "Measure"), safe_tr(tr, "est", "Estimate"), safe_tr(tr, "ic_lo", "Lower CI"), safe_tr(tr, "ic_hi", "Upper CI"))
      df
    })

    # ----- B) FAP por Pe o por Pce -----
    output$tab_fap <- renderTable({
      tr <- get_translations(lang(), "cohortes_2")
      RR_in <- input$RR_FAP
      Pe_in <- if (!is.na(input$Pe_FAP_pct)) pc(input$Pe_FAP_pct) else NA
      Pce <- if (!is.na(input$Pce_pct)) pc(input$Pce_pct) else NA

      # RR de respaldo: si no lo dieron, intenta construirlo desde la pestaña A
      ReA <- pc(input$Re_pct)
      R0A <- pc(input$R0_pct)
      RR_fallback <- if (!any(is.na(c(ReA, R0A))) && R0A > 0) ReA / R0A else NA
      RR <- if (!is.na(RR_in)) RR_in else RR_fallback

      FAP_Pe <- if (!any(is.na(c(Pe_in, RR)))) fap_from_rr_pe(RR, Pe_in) else NA
      FAP_Pce <- if (!any(is.na(c(Pce, RR)))) fap_from_pce_rr(Pce, RR) else NA

      df <- data.frame(
        x1 = c(tr$row_fap_pe, tr$row_fap_pce),
        x2 = c("Pe(RR-1)/(1+Pe(RR-1))", "Pce(RR-1)/RR"),
        x3 = c(fmt_pct(FAP_Pe), fmt_pct(FAP_Pce))
      )
      colnames(df) <- c(tr$variant, "Formula", safe_tr(tr, "est", "Estimate"))
      df
    })

    # ----- C) OR ajustada -> RR (Zhang & Yu) -----
    output$tab_or2rr <- renderTable({
      tr <- get_translations(lang(), "cohortes_2")
      OR <- input$OR_adj
      R0 <- pc(input$R0_or_pct)
      RR <- rr_from_or_zhang(OR, R0)
      df <- data.frame(
        x1 = c(safe_tr(tr, "row_or_adj", "Adj OR"), safe_tr(tr, "lbl_r0", "R0"), safe_tr(tr, "row_rr_zy", "RR (Z&Y)")),
        x2 = c(fmt_num(OR), fmt_pct(R0), fmt_num(RR))
      )
      colnames(df) <- c(safe_tr(tr, "meas", "Measure"), safe_tr(tr, "val", "Value"))
      df
    })

    # ----- D) NNT desde OR ajustada o Pcne -----
    output$tab_nnt <- renderTable({
      tr <- get_translations(lang(), "cohortes_2")
      # Opción 1: OR + R0
      OR1 <- input$OR_nnt
      R01 <- pc(input$R0_nnt_pct)
      RR1 <- rr_from_or_zhang(OR1, R01)
      ARR1 <- if (!is.na(RR1) && !is.na(R01)) R01 * (1 - RR1) else NA
      NNT1 <- inv_abs(ARR1)

      # Opción 2: Pcne + Pe (+ Rt o R0)
      Pcne <- if (!is.na(input$Pcne_pct)) pc(input$Pcne_pct) else NA
      Pe <- if (!is.na(input$Pe_pcne_pct)) pc(input$Pe_pcne_pct) else NA
      Rt2 <- if (!is.na(input$Rt_pcne_pct)) pc(input$Rt_pcne_pct) else NA
      R02 <- if (!is.na(input$R0_pcne_pct)) pc(input$R0_pcne_pct) else NA

      RR2 <- rr_from_pcne_pe(Pcne, Pe)
      R0_rec <- if (!is.na(R02)) R02 else if (!any(is.na(c(Rt2, Pe, RR2)))) r0_from_rt_pe_rr(Rt2, Pe, RR2) else NA
      ARR2 <- if (!any(is.na(c(R0_rec, RR2)))) R0_rec * (1 - RR2) else NA
      NNT2 <- inv_abs(ARR2)

      df <- data.frame(
        x1 = c(safe_tr(tr, "row_or_r0", "OR + R0"), safe_tr(tr, "row_pcne_pe", "Pcne + Pe")),
        x2 = c(fmt_num(RR1), fmt_num(RR2)),
        x3 = c(fmt_pct(ARR1), fmt_pct(ARR2)),
        x4 = c(
          ifelse(is.na(NNT1), NA, sprintf("%.1f", NNT1)),
          ifelse(is.na(NNT2), NA, sprintf("%.1f", NNT2))
        ),
        x5 = c(
          "RR Zhang & Yu; ARR = R0(1-RR)",
          "RR via Pcne/Pe"
        )
      )
      colnames(df) <- c(tr$variant, "RR est", "ARR est", "NNT/NNH", "Nota")
      df
    })

    # ----- E) NIE/NIEP desde OR ajustada -----
    output$tab_nie <- renderTable({
      tr <- get_translations(lang(), "cohortes_2")
      OR <- input$OR_nie
      R0 <- pc(input$R0_nie_pct)
      RR <- rr_from_or_zhang(OR, R0)
      Re <- if (!any(is.na(c(RR, R0)))) RR * R0 else NA
      RD <- if (!any(is.na(c(Re, R0)))) Re - R0 else NA
      NIE <- inv_abs(RD)
      tipo <- if (is.na(RR)) "NIE/NIEP" else if (RR > 1) tr$row_nie_d else tr$row_nie_p
      df <- data.frame(
        x1 = c(tr$row_or_adj, tr$row_rr_zy, tr$lbl_r0, "Re ~= RR*R0", "RD = Re - R0", tipo),
        x2 = c(
          fmt_num(OR), fmt_num(RR), fmt_pct(R0), fmt_pct(Re), fmt_pct(RD),
          ifelse(is.na(NIE), NA, sprintf("%.1f", NIE))
        )
      )
      colnames(df) <- c(safe_tr(tr, "meas", "Measure"), safe_tr(tr, "val", "Value"))
      df
    })

    # ----- F) PAP/FAP con confusión (OR→RR) -----
    output$tab_pap <- renderTable({
      tr <- get_translations(lang(), "cohortes_2")
      OR <- input$OR_pap
      R0 <- pc(input$R0_pap_pct)
      Pe <- pc(input$Pe_pap_pct)
      RR <- rr_from_or_zhang(OR, R0)
      FAP <- if (!any(is.na(c(Pe, RR)))) fap_from_rr_pe(RR, Pe) else NA
      df <- data.frame(
        x1 = c(tr$row_or_adj, tr$row_rr_zy, tr$lbl_pe, tr$row_fap),
        x2 = c(fmt_num(OR), fmt_num(RR), fmt_pct(Pe), fmt_pct(FAP))
      )
      colnames(df) <- c(tr$meas, tr$val)
      df
    })
    output$tabs_ui <- renderUI({
      tr <- get_translations(lang(), "cohortes_2")
      ns <- session$ns
      tabsetPanel(
        id = ns("tabs"),
        tabPanel(
          tr$tab_a,
          br(),
          fluidRow(
            column(3, numericInput(ns("Re_pct"), safe_tr(tr, "input_re", "Re (%)"), value = 30, min = 0, max = 100, step = 0.1)),
            column(3, numericInput(ns("R0_pct"), safe_tr(tr, "input_r0", "R0 (%)"), value = 20, min = 0, max = 100, step = 0.1)),
            column(3, numericInput(ns("Pe_pct"), safe_tr(tr, "input_pe", "Pe (%)"), value = 50, min = 0, max = 100, step = 0.1)),
            column(3, numericInput(ns("Rt_pct"), safe_tr(tr, "input_rt", "Rt (%)"), value = NA, min = 0, max = 100, step = 0.1))
          ),
          hr(),
          h4(id = ns("h4_basic"), tr$h4_basic),
          tableOutput(ns("tab_basic"))
        ),
        tabPanel(
          tr$tab_b,
          br(),
          fluidRow(
            column(4, numericInput(ns("RR_FAP"), safe_tr(tr, "input_rr_fap", "RR"), value = NA, min = 0, step = 0.001)),
            column(4, numericInput(ns("Pe_FAP_pct"), safe_tr(tr, "input_pe_fap", "Pe (%)"), value = NA, min = 0, max = 100, step = 0.1)),
            column(4, numericInput(ns("Pce_pct"), safe_tr(tr, "input_pce", "Pce (%)"), value = NA, min = 0, max = 100, step = 0.1))
          ),
          helpText(id = ns("help_fap"), tr$help_fap),
          hr(),
          h4(id = ns("h4_res_b"), tr$h4_res),
          tableOutput(ns("tab_fap"))
        ),
        tabPanel(
          tr$tab_c,
          br(),
          fluidRow(
            column(4, numericInput(ns("OR_adj"), safe_tr(tr, "input_or_adj", "OR adjusted"), value = 0.65, min = 0, step = 0.001)),
            column(4, numericInput(ns("R0_or_pct"), safe_tr(tr, "input_r0_or", "R0 (%)"), value = 20, min = 0, max = 100, step = 0.1)),
            column(4, helpText(tr$help_zhang))
          ),
          hr(),
          h4(id = ns("h4_res_c"), tr$h4_res),
          tableOutput(ns("tab_or2rr"))
        ),
        tabPanel(
          tr$tab_d,
          br(),
          h5(id = ns("h5_d1"), tr$h5_d1),
          fluidRow(
            column(4, numericInput(ns("OR_nnt"), safe_tr(tr, "input_or_nnt", "OR adjusted"), value = 0.65, min = 0, step = 0.001)),
            column(4, numericInput(ns("R0_nnt_pct"), safe_tr(tr, "input_r0_nnt", "R0 (%)"), value = 20, min = 0, max = 100, step = 0.1)),
            column(4, helpText(tr$help_d1))
          ),
          hr(),
          h5(id = ns("h5_d2"), tr$h5_d2),
          fluidRow(
            column(3, numericInput(ns("Pcne_pct"), safe_tr(tr, "input_pcne", "Pcne (%)"), value = NA, min = 0, max = 100, step = 0.1)),
            column(3, numericInput(ns("Pe_pcne_pct"), safe_tr(tr, "input_pe_pcne", "Pe (%)"), value = NA, min = 0, max = 100, step = 0.1)),
            column(3, numericInput(ns("Rt_pcne_pct"), safe_tr(tr, "input_rt_pcne", "Rt (%)"), value = NA, min = 0, max = 100, step = 0.1)),
            column(3, numericInput(ns("R0_pcne_pct"), safe_tr(tr, "input_r0_pcne", "R0 (%)"), value = NA, min = 0, max = 100, step = 0.1))
          ),
          helpText(id = ns("help_d2"), tr$help_d2),
          hr(),
          h4(id = ns("h4_res_d"), tr$h4_res),
          tableOutput(ns("tab_nnt"))
        ),
        tabPanel(
          tr$tab_e,
          br(),
          fluidRow(
            column(4, numericInput(ns("OR_nie"), safe_tr(tr, "input_or_nie", "OR adjusted"), value = 1.40, min = 0, step = 0.001)),
            column(4, numericInput(ns("R0_nie_pct"), safe_tr(tr, "input_r0_nie", "R0 (%)"), value = 15, min = 0, max = 100, step = 0.1)),
            column(4, helpText(tr$help_e))
          ),
          hr(),
          h4(id = ns("h4_res_e"), tr$h4_res),
          tableOutput(ns("tab_nie"))
        ),
        tabPanel(
          tr$tab_f,
          br(),
          fluidRow(
            column(4, numericInput(ns("OR_pap"), safe_tr(tr, "input_or_pap", "OR adjusted"), value = 1.50, min = 0, step = 0.001)),
            column(4, numericInput(ns("R0_pap_pct"), safe_tr(tr, "input_r0_pap", "R0 (%)"), value = 10, min = 0, max = 100, step = 0.1)),
            column(4, numericInput(ns("Pe_pap_pct"), safe_tr(tr, "input_pe_pap", "Pe (%)"), value = 30, min = 0, max = 100, step = 0.1))
          ),
          helpText(id = ns("help_f"), tr$help_f),
          hr(),
          h4(id = ns("h4_res_f"), tr$h4_res),
          tableOutput(ns("tab_pap"))
        )
      )
    })

    # ===== Print Template =====
    output$tab_basic_print <- renderTable({
      req(lang())
      tr <- get_translations(lang(), "cohortes_2")
      Re <- pc(input$Re_pct)
      R0 <- pc(input$R0_pct)
      Pe <- pc(input$Pe_pct)
      # Robust check for NULL/length 0
      if (length(Re) < 1 || length(R0) < 1 || is.na(Re[1]) || is.na(R0[1]) || isTRUE(R0[1] <= 0)) {
        RR <- NA
      } else {
        RR <- Re / R0
      }
      data.frame(Medida = c("Re", "R0", "Pe", "RR"), Valor = c(fmt_pct(Re), fmt_pct(R0), fmt_pct(Pe), fmt_num(RR)), check.names = FALSE)
    })
    output$tab_fap_print <- renderTable({
      req(lang())
      tr <- get_translations(lang(), "cohortes_2")
      RR <- input$RR_FAP
      Pe <- pc(input$Pe_FAP_pct)
      Pce <- pc(input$Pce_pct)
      # Ensure inputs are not length 0
      if (length(RR) == 0) RR <- NA
      if (length(Pe) == 0) Pe <- NA
      if (length(Pce) == 0) Pce <- NA

      FAP_Pe <- if (!any(is.na(c(Pe, RR)))) fap_from_rr_pe(RR, Pe) else NA
      FAP_Pce <- if (!any(is.na(c(Pce, RR)))) fap_from_pce_rr(Pce, RR) else NA
      data.frame(Medida = c("FAP(Pe)", "FAP(Pce)"), Valor = c(fmt_pct(FAP_Pe), fmt_pct(FAP_Pce)), check.names = FALSE)
    })

    output$print_view <- renderUI({
      ns <- session$ns
      tr <- get_translations(lang(), "cohortes_2")
      tagList(
        div(class = "printable-section", h3(tr$tab_a), tableOutput(ns("tab_basic_print"))),
        div(class = "printable-section", h3(tr$tab_b), tableOutput(ns("tab_fap_print")))
      )
    })

    outputOptions(output, "print_view", suspendWhenHidden = FALSE)
    outputOptions(output, "tab_basic_print", suspendWhenHidden = FALSE)
    outputOptions(output, "tab_fap_print", suspendWhenHidden = FALSE)
  })
}
