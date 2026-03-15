library(shiny)

# ---------- Module UI ----------
# ---------- Module UI ----------
otros_calculos_nnt_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        sliderInput(ns("alpha"), "Nivel de confianza", min = 0.80, max = 0.99, value = 0.95, step = 0.01),
        helpText(id = ns("help_note"), "Notas: NNT>0 indica beneficio (ARR>0); NNH se reporta cuando ARR<0 (daño).")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            title = span(id = ns("tab_A_title"), "A) NNT/NNH desde riesgos o RR"),
            br(),
            h5(id = ns("h5_A1"), "Opción 1 — Desde riesgos absolutos"),
            fluidRow(
              column(4, numericInput(ns("Re_pct_A"), "Re (%)", value = 18, min = 0, max = 100, step = 0.1)),
              column(4, numericInput(ns("Rc_pct_A"), "Rc (%)", value = 30, min = 0, max = 100, step = 0.1)),
              column(4, helpText(id = ns("help_A1"), "ARR = Rc − Re; NNT=1/|ARR|."))
            ),
            h5(id = ns("h5_A_IC"), "IC (opcional) si conoces tamaños muestrales"),
            fluidRow(
              column(3, numericInput(ns("nE_A"), "nE", value = NA, min = 1, step = 1)),
              column(3, numericInput(ns("nC_A"), "nC", value = NA, min = 1, step = 1))
            ),
            hr(),
            h5(id = ns("h5_A2"), "Opción 2 — Desde RR y riesgo basal"),
            fluidRow(
              column(4, numericInput(ns("RR_A"), "RR", value = 0.65, min = 0, step = 0.001)),
              column(4, numericInput(ns("RcRR_pct_A"), "Rc (%)", value = 25, min = 0, max = 100, step = 0.1)),
              column(4, helpText(id = ns("help_A2"), "Re = RR·Rc; ARR = Rc − Re."))
            ),
            hr(),
            h4(id = ns("res_A"), "Resultados"),
            tableOutput(ns("tab_A"))
          ),
          tabPanel(
            title = span(id = ns("tab_B_title"), "B) Ajuste temporal y por riesgo basal"),
            br(),
            fluidRow(
              column(3, numericInput(ns("RR_B"), "RR del tratamiento", value = 0.70, min = 0, step = 0.001)),
              column(3, numericInput(ns("R0_B_pct"), "Riesgo basal (R₀, %)", value = 12, min = 0, max = 100, step = 0.1)),
              column(3, numericInput(ns("T_est"), "Duración del estudio (meses)", value = 24, min = 0.01, step = 0.1)),
              column(3, numericInput(ns("T_apl"), "Tiempo aplicable (meses)", value = 6, min = 0.01, step = 0.1))
            ),
            hr(),
            h4(id = ns("res_B"), "Resultados"),
            tableOutput(ns("tab_B")),
            helpText(id = ns("help_B"), "Ajuste temporal aproximado: NNT_aj ≈ NNT × (T_estudio / T_aplicable).")
          ),
          tabPanel(
            title = span(id = ns("tab_C_title"), "C) NNV (vacunas)"),
            br(),
            fluidRow(
              column(3, numericInput(ns("VE_pct"), "Eficacia vacunal VE (%)", value = 60, min = 0, max = 100, step = 0.1)),
              column(3, numericInput(ns("Inc_pct"), "Incidencia (o riesgo) sin vacunar (%)", value = 10, min = 0, max = 100, step = 0.1)),
              column(3, numericInput(ns("T_ve"), "Horizonte temporal (meses)", value = 12, min = 0.01, step = 0.1)),
              column(3, numericInput(ns("T_obj"), "Periodo de interés (meses)", value = 12, min = 0.01, step = 0.1))
            ),
            hr(),
            h4(id = ns("res_C"), "Resultados"),
            tableOutput(ns("tab_C")),
            helpText(id = ns("help_C"), "NNV = 1 / (VE·Inc). Si el periodo objetivo es menor, NNV_aj ≈ NNV × (T_horizonte / T_obj).")
          ),
          tabPanel(
            title = span(id = ns("tab_D_title"), "D) NNT en persona-tiempo (tasas)"),
            br(),
            fluidRow(
              column(3, numericInput(ns("Ie_D"), "Tasa Ie (por 1 pt)", value = 0.020, min = 0, step = 0.0001)),
              column(3, numericInput(ns("I0_D"), "Tasa I0 (por 1 pt)", value = 0.030, min = 0, step = 0.0001)),
              column(3, numericInput(ns("base_D"), "Base de presentación (por ... pt)", value = 1000, min = 1, step = 1)),
              column(3, helpText(id = ns("help_D"), "Δ tasa = Ie − I0; NNT_pt = 1/|Δ tasa| (pt por evento)."))
            ),
            hr(),
            h4(id = ns("res_D"), "Resultados"),
            tableOutput(ns("tab_D"))
          ),
          tabPanel(
            title = span(id = ns("tab_E_title"), "E) Subgrupos por riesgo basal"),
            br(),
            fluidRow(
              column(3, numericInput(ns("RR_E"), "RR común", value = 0.70, min = 0, step = 0.001)),
              column(3, textInput(ns("Rc_grid"), "Rc (%) separados por comas", value = "5,10,15,20,25,30")),
              column(3, numericInput(ns("T_scale"), "Escala temporal (multiplica NNT)", value = 1, min = 0.01, step = 0.01)),
              column(3, helpText(id = ns("help_E"), "Ej.: 0.5 para 6 m cuando el estudio duró 12 m."))
            ),
            hr(),
            h4(id = ns("res_E"), "Resultados"),
            tableOutput(ns("tab_E"))
          ),
          tabPanel(
            title = span(id = ns("tab_F_title"), "F) Conversión NNT ↔ ARR"),
            br(),
            fluidRow(
              column(4, numericInput(ns("NNT_F"), "NNT (positivo = beneficio)", value = 10, min = 0.01, step = 0.01)),
              column(4, helpText(id = ns("help_F"), "ARR = 1/NNT; si el efecto es dañino, NNH y ARR negativa.")),
              column(4, numericInput(ns("signo_F"), "Signo de ARR (+1 beneficio, −1 daño)", value = 1, min = -1, max = 1, step = 1))
            ),
            hr(),
            h4(id = ns("res_F"), "Conversión"),
            tableOutput(ns("tab_F")),
            hr(),
            h5(id = ns("h5_F_IC"), "ARR ↔ NNT con IC (opcional)"),
            fluidRow(
              column(3, numericInput(ns("Re_ic_pct"), "Re (%)", value = NA, min = 0, max = 100, step = 0.1)),
              column(3, numericInput(ns("Rc_ic_pct"), "Rc (%)", value = NA, min = 0, max = 100, step = 0.1)),
              column(3, numericInput(ns("nE_ic"), "nE", value = NA, min = 1, step = 1)),
              column(3, numericInput(ns("nC_ic"), "nC", value = NA, min = 1, step = 1))
            ),
            tableOutput(ns("tab_F_ic")),
            helpText(id = ns("help_F_IC"), "Si aportas Re, Rc, nE y nC, se muestra IC(ARR) por Wald e IC(NNT) por inversión.")
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
otros_calculos_nnt_Server <- function(id, lang) {
  moduleServer(id, function(input, output, session) {
    # NEW: I18N Logic
    observeEvent(lang(), {
      # Get translations
      tr <- get_translations(lang(), "otros_calculos_nnt")

      # Standard Inputs
      updateSliderInput(session, "alpha", label = tr$confidence_level)
      updateNumericInput(session, "Re_pct_A", label = tr$lbl_Re)
      updateNumericInput(session, "Rc_pct_A", label = tr$lbl_Rc)
      updateNumericInput(session, "RR_A", label = tr$lbl_RR)
      updateNumericInput(session, "RcRR_pct_A", label = tr$lbl_Rc)
      updateNumericInput(session, "nE_A", label = tr$lbl_nE)
      updateNumericInput(session, "nC_A", label = tr$lbl_nC)

      updateNumericInput(session, "RR_B", label = tr$lbl_RR_B)
      updateNumericInput(session, "R0_B_pct", label = tr$lbl_R0_B)
      updateNumericInput(session, "T_est", label = tr$lbl_T_est)
      updateNumericInput(session, "T_apl", label = tr$lbl_T_apl)

      updateNumericInput(session, "VE_pct", label = tr$lbl_VE)
      updateNumericInput(session, "Inc_pct", label = tr$lbl_Inc)
      updateNumericInput(session, "T_ve", label = tr$lbl_T_ve)
      updateNumericInput(session, "T_obj", label = tr$lbl_T_obj)

      updateNumericInput(session, "Ie_D", label = tr$lbl_Ie_D)
      updateNumericInput(session, "I0_D", label = tr$lbl_I0_D)
      updateNumericInput(session, "base_D", label = tr$lbl_base_D)

      updateNumericInput(session, "RR_E", label = tr$lbl_RR_E)
      updateTextInput(session, "Rc_grid", label = tr$lbl_Rc_grid)
      updateNumericInput(session, "T_scale", label = tr$lbl_T_scale)

      updateNumericInput(session, "NNT_F", label = tr$lbl_NNT_F)
      updateNumericInput(session, "signo_F", label = tr$lbl_signo_F)
      updateNumericInput(session, "Re_ic_pct", label = tr$lbl_Re)
      updateNumericInput(session, "Rc_ic_pct", label = tr$lbl_Rc)
      updateNumericInput(session, "nE_ic", label = tr$lbl_nE)
      updateNumericInput(session, "nC_ic", label = tr$lbl_nC)

      # Update Static UI elements with IDs (we map UI ID -> Dictionary Key)
      ids_map <- list(
        "help_note" = "help_note",
        "tab_A_title" = "tab_A",
        "h5_A1" = "h5_A1",
        "help_A1" = "help_A1",
        "h5_A_IC" = "h5_A_IC",
        "h5_A2" = "h5_A2",
        "help_A2" = "help_A2",
        "res_A" = "results",
        "tab_B_title" = "tab_B",
        "res_B" = "results",
        "help_B" = "help_B",
        "tab_C_title" = "tab_C",
        "res_C" = "results",
        "help_C" = "help_C",
        "tab_D_title" = "tab_D",
        "res_D" = "results",
        "help_D" = "help_D",
        "tab_E_title" = "tab_E",
        "res_E" = "results",
        "help_E" = "help_E",
        "tab_F_title" = "tab_F",
        "res_F" = "results", # Or "Convert" if I made a key, but UI says "Conversión" originally
        # let's leave it as "results" or make a dictionary entry "conversion"?
        # Dictionary has standard 'results' -> 'Results'
        # Original UI for F had h4("Conversión").
        # Let's map res_F to 'conversion' and add it to dict, or use standard 'results' if acceptable?
        # I'll stick to what I have in dict. Dictionary has 'tab_F'="F) Conversión...",
        # but no key for the h4 "Conversión". I'll add "conversion" to common or module dict.
        # For now, let's just use 'results' for consistency or leave it if not in dict.
        # Wait, the dictionary in i18n_utils.R DOES NOT have "conversion".
        # I will use "results" for now to fix the Spanish issue, as "Results" is generic and fine.
        # Or I can quickly update the dict.
        "h5_F_IC" = "h5_F_IC",
        "help_F_IC" = "help_F_IC"
      )

      update_static_ui(session, ids_map, tr, session$ns)
    })

    # ========= Utilidades =========
    pc <- function(x) x / 100
    fmt_pct <- function(x) ifelse(is.na(x), NA, sprintf("%.2f%%", 100 * x))
    fmt_num <- function(x, k = 3) ifelse(is.na(x), NA, sprintf(paste0("%.", k, "f"), x))
    inv_abs <- function(x) ifelse(is.na(x) || x == 0, NA_real_, 1 / abs(x))

    # IC (Wald) para diferencia de riesgos ARR = Rc - Re
    arr_ci_wald <- function(Re, Rc, nE = NA, nC = NA, level = 0.95) {
      if (any(is.na(Re), is.na(Rc)) || any(is.na(nE), is.na(nC)) || min(nE, nC) <= 0) {
        return(c(NA, NA))
      }
      z <- qnorm(1 - (1 - level) / 2)
      se <- sqrt(Re * (1 - Re) / nE + Rc * (1 - Rc) / nC)
      lo <- (Rc - Re) - z * se
      hi <- (Rc - Re) + z * se
      c(lo, hi)
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

    # Zhang & Yu (OR -> RR) por si se usa en alguna pestaña
    rr_from_or_zhang <- function(OR, R0) {
      if (any(is.na(OR), is.na(R0))) {
        return(NA_real_)
      }
      OR / ((1 - R0) + R0 * OR)
    }

    # ----- A) NNT desde riesgos o RR -----
    output$tab_A <- renderTable({
      tr <- get_translations(lang(), "otros_calculos_nnt")

      Re1 <- pc(input$Re_pct_A)
      Rc1 <- pc(input$Rc_pct_A)
      ARR1 <- if (!any(is.na(c(Re1, Rc1)))) Rc1 - Re1 else NA
      NNT1 <- if (!is.na(ARR1) && ARR1 != 0) 1 / abs(ARR1) else NA

      lbl1 <- if (is.na(ARR1)) tr$nnt_nnh_label else if (ARR1 > 0) tr$nnt_benefit else tr$nnh_harm

      # IC (si hay tamaños)
      arr_ci1 <- arr_ci_wald(Re1, Rc1, nE = input$nE_A, nC = input$nC_A, level = input$alpha)
      nnt_ci1 <- nnt_ci_from_arr_ci(arr_ci1[1], arr_ci1[2])

      RR2 <- input$RR_A
      Rc2 <- pc(input$RcRR_pct_A)
      Re2 <- if (!any(is.na(c(RR2, Rc2)))) RR2 * Rc2 else NA
      ARR2 <- if (!any(is.na(c(Rc2, Re2)))) Rc2 - Re2 else NA
      NNT2 <- if (!is.na(ARR2) && ARR2 != 0) 1 / abs(ARR2) else NA
      lbl2 <- if (is.na(ARR2)) tr$nnt_nnh_label else if (ARR2 > 0) tr$nnt_benefit else tr$nnh_harm

      df <- data.frame(
        v1 = c(tr$from_re_rc, tr$from_rr_rc),
        v2 = c(fmt_pct(ARR1), fmt_pct(ARR2)),
        v3 = c(
          ifelse(is.na(NNT1), NA, sprintf("%.1f", NNT1)),
          ifelse(is.na(NNT2), NA, sprintf("%.1f", NNT2))
        ),
        v4 = c(
          ifelse(any(is.na(nnt_ci1)), NA, sprintf("[%.1f, %.1f]", nnt_ci1[1], nnt_ci1[2])),
          NA
        ),
        v5 = c(lbl1, lbl2),
        check.names = FALSE
      )

      colnames(df) <- c(tr$variant, "ARR (Rc - Re)", tr$nnt_nnh_label, tr$ic_nnt_label, tr$label)
      df
    })

    # ----- B) Ajuste temporal y por riesgo -----
    output$tab_B <- renderTable({
      tr <- get_translations(lang(), "otros_calculos_nnt")

      RR <- input$RR_B
      R0 <- pc(input$R0_B_pct)
      Re <- if (!any(is.na(c(RR, R0)))) RR * R0 else NA
      ARR <- if (!any(is.na(c(R0, Re)))) R0 - Re else NA
      NNT <- if (!is.na(ARR) && ARR != 0) 1 / abs(ARR) else NA

      fac_t <- if (!any(is.na(c(input$T_est, input$T_apl)))) input$T_est / input$T_apl else NA
      NNT_adj <- if (!any(is.na(c(NNT, fac_t)))) NNT * fac_t else NA
      lbl <- if (is.na(ARR)) tr$nnt_nnh_label else if (ARR > 0) tr$nnt_benefit else tr$nnh_harm

      df <- data.frame(
        m = c("R0", "Re = RR·R0", "ARR = R0 − Re", lbl, tr$time_factor, tr$nnt_adjusted),
        v = c(
          fmt_pct(R0), fmt_pct(Re), fmt_pct(ARR),
          ifelse(is.na(NNT), NA, sprintf("%.1f", NNT)),
          fmt_num(fac_t, 2),
          ifelse(is.na(NNT_adj), NA, sprintf("%.1f", NNT_adj))
        ),
        check.names = FALSE
      )
      colnames(df) <- c(tr$measure, tr$value)
      df
    })

    # ----- C) NNV (vacunas) -----
    output$tab_C <- renderTable({
      tr <- get_translations(lang(), "otros_calculos_nnt")

      VE <- pc(input$VE_pct)
      Inc <- pc(input$Inc_pct)
      NNV <- if (!any(is.na(c(VE, Inc))) && VE > 0 && Inc > 0) 1 / (VE * Inc) else NA

      fac_t <- if (!any(is.na(c(input$T_ve, input$T_obj)))) input$T_ve / input$T_obj else NA
      NNV_adj <- if (!any(is.na(c(NNV, fac_t)))) NNV * fac_t else NA

      df <- data.frame(
        m = c("VE", tr$inc_no_vac, "NNV = 1/(VE·Inc)", tr$time_factor, tr$nnt_adjusted),
        v = c(
          fmt_pct(VE), fmt_pct(Inc),
          ifelse(is.na(NNV), NA, sprintf("%.1f", NNV)),
          fmt_num(fac_t, 2),
          ifelse(is.na(NNV_adj), NA, sprintf("%.1f", NNV_adj))
        ),
        check.names = FALSE
      )
      colnames(df) <- c(tr$measure, tr$value)
      df
    })

    # ----- D) NNT en persona-tiempo -----
    output$tab_D <- renderTable({
      tr <- get_translations(lang(), "otros_calculos_nnt")

      Ie <- input$Ie_D
      I0 <- input$I0_D
      base <- input$base_D
      dI <- if (!any(is.na(c(Ie, I0)))) Ie - I0 else NA
      NIT <- if (!is.na(dI) && dI != 0) 1 / abs(dI) else NA

      # For dynamic strings with partial translation (like "por 1000 pt") we might keep structure but translate parts
      # Or just use the dictionary string and append numbers.
      # tr$delta_rate = "Delta rate"
      # tr$person_time_event = "Person-time per event"

      df <- data.frame(
        m = c(paste0(tr$delta_rate, " = Ie − I0 (", round(base), " pt)"), paste0(tr$person_time_event, " (1/|", tr$delta_rate, "|)")),
        v = c(
          sprintf("%.3f per %d pt", dI * base, base),
          ifelse(is.na(NIT), NA, sprintf("%.1f pt/evt", NIT))
        ),
        check.names = FALSE
      )
      colnames(df) <- c(tr$measure, tr$value)
      df
    })

    # ----- E) Subgrupos por Rc -----
    output$tab_E <- renderTable(
      {
        tr <- get_translations(lang(), "otros_calculos_nnt")

        RR <- input$RR_E
        Rc_vals <- as.numeric(trimws(unlist(strsplit(input$Rc_grid, ","))))
        Rc_vals <- Rc_vals[!is.na(Rc_vals)]
        if (length(Rc_vals) == 0) {
          return(NULL)
        }
        Rc <- Rc_vals / 100
        Re <- RR * Rc
        ARR <- Rc - Re
        NNT <- sapply(ARR, function(x) if (!is.na(x) && x != 0) 1 / abs(x) else NA)
        NNT_scaled <- NNT * input$T_scale

        df <- data.frame(
          c1 = sprintf("%.1f", 100 * Rc),
          c2 = sprintf("%.1f", 100 * Re),
          c3 = sprintf("%.2f", 100 * ARR),
          c4 = ifelse(is.na(NNT), NA, sprintf("%.1f", NNT)),
          c5 = ifelse(is.na(NNT_scaled), NA, sprintf("%.1f", NNT_scaled)),
          check.names = FALSE
        )
        colnames(df) <- c(tr$lbl_Rc, tr$lbl_Re, tr$arr_label, tr$nnt_nnh_label, paste0(tr$nnt_nnh_label, " x ", input$T_scale))
        df
      },
      sanitize.text.function = function(x) x
    )

    # ----- F) Conversión NNT <-> ARR + IC opcional -----
    output$tab_F <- renderTable({
      tr <- get_translations(lang(), "otros_calculos_nnt")

      sgn <- ifelse(input$signo_F >= 0, 1, -1)
      ARR <- sgn * (1 / input$NNT_F)
      lbl <- if (ARR > 0) tr$nnt_benefit else tr$nnh_harm

      df <- data.frame(
        m = c(tr$arr_label, lbl),
        v = c(fmt_pct(ARR), sprintf("%.1f", abs(input$NNT_F))),
        check.names = FALSE
      )
      colnames(df) <- c(tr$measure, tr$value)
      df
    })

    output$tab_F_ic <- renderTable({
      tr <- get_translations(lang(), "otros_calculos_nnt")

      Re <- pc(input$Re_ic_pct)
      Rc <- pc(input$Rc_ic_pct)
      nE <- input$nE_ic
      nC <- input$nC_ic
      if (any(is.na(c(Re, Rc, nE, nC)))) {
        return(NULL)
      }
      arr_ci <- arr_ci_wald(Re, Rc, nE, nC, level = input$alpha)
      nnt_ci <- nnt_ci_from_arr_ci(arr_ci[1], arr_ci[2])
      ARR <- Rc - Re
      NNT <- if (!is.na(ARR) && ARR != 0) 1 / abs(ARR) else NA

      df <- data.frame(
        m = c(
          tr$arr_punt, tr$ic_inf_arr, tr$ic_sup_arr,
          paste0(tr$nnt_nnh_label, " puntual"), tr$ic_inf_nnt, tr$ic_sup_nnt
        ),
        v = c(
          fmt_pct(ARR), fmt_pct(arr_ci[1]), fmt_pct(arr_ci[2]),
          ifelse(is.na(NNT), NA, sprintf("%.1f", NNT)),
          ifelse(any(is.na(nnt_ci)), NA, sprintf("%.1f", nnt_ci[1])),
          ifelse(any(is.na(nnt_ci)), NA, sprintf("%.1f", nnt_ci[2]))
        ),
        check.names = FALSE
      )
      colnames(df) <- c(tr$measure, tr$value)
      df
    })

    # ===== Print Template =====
    output$tab_A_print <- renderTable({
      req(lang())
      tr <- get_translations(lang(), "otros_calculos_nnt")
      Re1 <- pc(input$Re_pct_A)
      Rc1 <- pc(input$Rc_pct_A)
      ARR1 <- if (!any(is.na(c(Re1, Rc1)))) Rc1 - Re1 else NA
      NNT1 <- if (!is.na(ARR1) && ARR1 != 0) 1 / abs(ARR1) else NA
      RR2 <- input$RR_A
      Rc2 <- pc(input$RcRR_pct_A)
      Re2 <- if (!any(is.na(c(RR2, Rc2)))) RR2 * Rc2 else NA
      ARR2 <- if (!any(is.na(c(Rc2, Re2)))) Rc2 - Re2 else NA
      NNT2 <- if (!is.na(ARR2) && ARR2 != 0) 1 / abs(ARR2) else NA
      df <- data.frame(
        Via = c("Re/Rc", "RR/Rc"), ARR = c(fmt_pct(ARR1), fmt_pct(ARR2)),
        NNT = c(ifelse(is.na(NNT1), NA, sprintf("%.1f", NNT1)), ifelse(is.na(NNT2), NA, sprintf("%.1f", NNT2))), check.names = FALSE
      )
      colnames(df) <- c(tr$variant, "ARR", tr$nnt_nnh_label)
      df
    })
    output$tab_B_print <- renderTable({
      req(lang())
      tr <- get_translations(lang(), "otros_calculos_nnt")
      RR <- input$RR_B
      R0 <- pc(input$R0_B_pct)
      Re <- if (!any(is.na(c(RR, R0)))) RR * R0 else NA
      ARR <- if (!any(is.na(c(R0, Re)))) R0 - Re else NA
      NNT <- if (!is.na(ARR) && ARR != 0) 1 / abs(ARR) else NA
      fac_t <- if (!any(is.na(c(input$T_est, input$T_apl)))) input$T_est / input$T_apl else NA
      NNT_adj <- if (!any(is.na(c(NNT, fac_t)))) NNT * fac_t else NA
      df <- data.frame(M = c("ARR", "NNT", tr$nnt_adjusted), V = c(fmt_pct(ARR), ifelse(is.na(NNT), NA, sprintf("%.1f", NNT)), ifelse(is.na(NNT_adj), NA, sprintf("%.1f", NNT_adj))), check.names = FALSE)
      colnames(df) <- c(tr$measure, tr$value)
      df
    })
    output$tab_C_print <- renderTable({
      req(lang())
      tr <- get_translations(lang(), "otros_calculos_nnt")
      VE <- pc(input$VE_pct)
      Inc <- pc(input$Inc_pct)
      NNV <- if (!any(is.na(c(VE, Inc))) && VE > 0 && Inc > 0) 1 / (VE * Inc) else NA
      df <- data.frame(M = c("VE", tr$inc_no_vac, "NNV"), V = c(fmt_pct(VE), fmt_pct(Inc), ifelse(is.na(NNV), NA, sprintf("%.1f", NNV))), check.names = FALSE)
      colnames(df) <- c(tr$measure, tr$value)
      df
    })
    output$tab_D_print <- renderTable({
      req(lang())
      tr <- get_translations(lang(), "otros_calculos_nnt")
      Ie <- input$Ie_D
      I0 <- input$I0_D
      dI <- if (!any(is.na(c(Ie, I0)))) Ie - I0 else NA
      NIT <- if (!is.na(dI) && dI != 0) 1 / abs(dI) else NA
      df <- data.frame(M = c(tr$delta_rate, tr$person_time_event), V = c(ifelse(is.na(dI), NA, sprintf("%.4f", dI)), ifelse(is.na(NIT), NA, sprintf("%.1f", NIT))), check.names = FALSE)
      colnames(df) <- c(tr$measure, tr$value)
      df
    })
    output$tab_E_print <- renderTable({
      req(lang())
      tr <- get_translations(lang(), "otros_calculos_nnt")
      RR <- input$RR_E
      Rc_vals <- as.numeric(trimws(unlist(strsplit(input$Rc_grid, ","))))
      Rc_vals <- Rc_vals[!is.na(Rc_vals)]
      if (length(Rc_vals) == 0) {
        return(NULL)
      }
      Rc <- Rc_vals / 100
      Re <- RR * Rc
      ARR <- Rc - Re
      NNT <- sapply(ARR, function(x) if (!is.na(x) && x != 0) 1 / abs(x) else NA)
      df <- data.frame(Rc = sprintf("%.1f", 100 * Rc), ARR = sprintf("%.2f", 100 * ARR), NNT = ifelse(is.na(NNT), NA, sprintf("%.1f", NNT)), check.names = FALSE)
      colnames(df) <- c(tr$lbl_Rc, "ARR", tr$nnt_nnh_label)
      df
    })
    output$tab_F_print <- renderTable({
      req(lang())
      tr <- get_translations(lang(), "otros_calculos_nnt")
      sgn <- ifelse(input$signo_F >= 0, 1, -1)
      ARR <- sgn * (1 / input$NNT_F)
      df <- data.frame(M = c("ARR", tr$nnt_nnh_label), V = c(fmt_pct(ARR), sprintf("%.1f", abs(input$NNT_F))), check.names = FALSE)
      colnames(df) <- c(tr$measure, tr$value)
      df
    })
    output$tab_F_ic_print <- renderTable({
      req(lang())
      tr <- get_translations(lang(), "otros_calculos_nnt")
      Re <- pc(input$Re_ic_pct)
      Rc <- pc(input$Rc_ic_pct)
      nE <- input$nE_ic
      nC <- input$nC_ic
      if (any(is.na(c(Re, Rc, nE, nC)))) {
        return(NULL)
      }
      arr_ci <- arr_ci_wald(Re, Rc, nE, nC, level = input$alpha)
      ARR <- Rc - Re
      NNT <- if (!is.na(ARR) && ARR != 0) 1 / abs(ARR) else NA
      df <- data.frame(M = c("ARR", tr$nnt_nnh_label), V = c(fmt_pct(ARR), ifelse(is.na(NNT), NA, sprintf("%.1f", NNT))), check.names = FALSE)
      colnames(df) <- c(tr$measure, tr$value)
      df
    })

    output$print_view <- renderUI({
      ns <- session$ns
      tr <- get_translations(lang(), "otros_calculos_nnt")
      tagList(
        div(class = "printable-section", h3("NNT / NNH")),
        div(class = "printable-section", h4(tr$tab_A), tableOutput(ns("tab_A_print"))),
        div(class = "printable-section", h4(tr$tab_B), tableOutput(ns("tab_B_print"))),
        div(class = "printable-section", h4(tr$tab_C), tableOutput(ns("tab_C_print"))),
        div(class = "printable-section", h4(tr$tab_D), tableOutput(ns("tab_D_print"))),
        div(class = "printable-section", h4(tr$tab_E), tableOutput(ns("tab_E_print"))),
        div(class = "printable-section", h4(tr$tab_F), tableOutput(ns("tab_F_print"))),
        div(class = "printable-section", tableOutput(ns("tab_F_ic_print")))
      )
    })

    outputOptions(output, "print_view", suspendWhenHidden = FALSE)
    outputOptions(output, "tab_A_print", suspendWhenHidden = FALSE)
    outputOptions(output, "tab_B_print", suspendWhenHidden = FALSE)
    outputOptions(output, "tab_C_print", suspendWhenHidden = FALSE)
    outputOptions(output, "tab_D_print", suspendWhenHidden = FALSE)
    outputOptions(output, "tab_E_print", suspendWhenHidden = FALSE)
    outputOptions(output, "tab_F_print", suspendWhenHidden = FALSE)
    outputOptions(output, "tab_F_ic_print", suspendWhenHidden = FALSE)
  })
}
