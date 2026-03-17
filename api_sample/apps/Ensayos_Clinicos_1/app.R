library(shiny)

# =========================
# Helpers
# =========================
pc3 <- function(p) { # proporción -> "xx.xxx %"
  ifelse(is.na(p) | !is.finite(p), "", sprintf("%.3f%%", 100 * p))
}
num3 <- function(x) {
  ifelse(is.na(x) | !is.finite(x), "", sprintf("%.3f", x))
}
pval <- function(x) {
  ifelse(is.na(x) | !is.finite(x), "", formatC(x, digits = 6, format = "f"))
}
int_ceiling_abs <- function(x) {
  ifelse(is.na(x) | !is.finite(x), NA_real_, ceiling(abs(x)))
}
fmt_int_or_inf <- function(x) {
  if (is.na(x)) {
    return("")
  }
  if (is.infinite(x)) {
    return("\u221E")
  } # ∞
  as.character(as.integer(ceiling(abs(x))))
}

# =========================
# Statistics Engine
# =========================
wald_prop_ci <- function(p, n, z) {
  if (is.na(p) | is.na(n) | n <= 0) {
    return(c(NA_real_, NA_real_))
  }
  se <- sqrt(p * (1 - p) / n)
  c(max(0, p - z * se), min(1, p + z * se))
}

nnt_from_rar <- function(rar) {
  if (is.na(rar) | !is.finite(rar) | rar == 0) {
    return(NA_real_)
  }
  1 / rar
}

nnt_ci_from_rar_ci <- function(rar_ci, rar_point) {
  if (any(is.na(rar_ci)) | any(!is.finite(rar_ci))) {
    return(c(NA_real_, NA_real_))
  }
  lo <- rar_ci[1]
  hi <- rar_ci[2]
  if (lo > hi) {
    tmp <- lo
    lo <- hi
    hi <- tmp
  }

  crosses0 <- (lo <= 0 & hi >= 0)
  if (crosses0) {
    if (!is.na(rar_point) && rar_point > 0) {
      pos_hi <- max(lo, hi)
      if (pos_hi <= 0) {
        return(c(Inf, Inf))
      }
      return(c(ceiling(1 / pos_hi), Inf))
    } else if (!is.na(rar_point) && rar_point < 0) {
      neg_lo <- min(lo, hi)
      if (neg_lo >= 0) {
        return(c(Inf, Inf))
      }
      return(c(ceiling(1 / abs(neg_lo)), Inf))
    } else {
      return(c(NA_real_, NA_real_))
    }
  }

  inv <- sort(abs(1 / c(lo, hi)))
  c(ceiling(inv[1]), ceiling(inv[2]))
}

# =========================
# Nomogram Plot
# =========================
nomogram_plot <- function(cer_p, rrr_p) {
  cer_min <- 0.005
  cer_max <- 0.90
  rrr_min <- 0.001
  rrr_max <- 1.00
  nnt_min <- 1
  nnt_max <- 1000

  map_log <- function(x, xmin, xmax) {
    x <- max(min(x, xmax), xmin)
    (log10(x) - log10(xmin)) / (log10(xmax) - log10(xmin))
  }

  plot.new()
  plot.window(xlim = c(0.5, 3.5), ylim = c(0, 1))
  x_cer <- 1
  x_rrr <- 2
  x_nnt <- 3
  segments(x_cer, 0, x_cer, 1)
  segments(x_rrr, 0, x_rrr, 1)
  segments(x_nnt, 0, x_nnt, 1)

  cer_ticks <- c(0.006, 0.008, 0.01, 0.015, 0.02, 0.025, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.10, 0.12, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90)
  rrr_ticks <- c(0.001, 0.002, 0.003, 0.005, 0.01, 0.02, 0.03, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 1.0)
  nnt_ticks <- c(1, 2, 3, 4, 5, 7, 8, 10, 12, 15, 20, 25, 30, 40, 50, 60, 80, 100, 120, 150, 200, 250, 300, 350, 400, 500, 600, 700, 800, 1000)

  for (t in cer_ticks) {
    y <- map_log(t, cer_min, cer_max)
    segments(x_cer - 0.03, y, x_cer + 0.03, y)
    if (t %in% c(0.01, 0.02, 0.03, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9)) {
      text(x_cer - 0.08, y, labels = sprintf("%.1f", 100 * t), adj = 1, cex = 0.8)
    }
  }
  for (t in rrr_ticks) {
    y <- map_log(t, rrr_min, rrr_max)
    segments(x_rrr - 0.03, y, x_rrr + 0.03, y)
    if (t %in% c(0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 0.5, 1.0)) {
      lab <- ifelse(t < 1, sprintf("%.1f", 100 * t), "100")
      text(x_rrr + 0.08, y, labels = lab, adj = 0, cex = 0.8)
    }
  }
  for (t in nnt_ticks) {
    y <- map_log(t, nnt_min, nnt_max)
    segments(x_nnt - 0.03, y, x_nnt + 0.03, y)
    if (t %in% c(1, 2, 3, 5, 10, 20, 50, 100, 200, 500, 1000)) {
      text(x_nnt + 0.10, y, labels = as.character(t), adj = 0, cex = 0.8)
    }
  }

  text(x_cer, -0.06, "Riesgo absoluto\nsin tratamiento (CER %)", cex = 0.9)
  text(x_rrr, -0.06, "Reducción\nrelativa riesgo (RRR %)", cex = 0.9)
  text(x_nnt, -0.06, "NNT", cex = 0.9)

  if (!is.na(cer_p) & !is.na(rrr_p) & is.finite(cer_p) & is.finite(rrr_p) &
    cer_p > 0 & rrr_p > 0) {
    cer_p <- max(min(cer_p, cer_max), cer_min)
    rrr_p <- max(min(rrr_p, rrr_max), rrr_min)
    nnt <- 1 / (cer_p * rrr_p)
    nnt <- max(min(nnt, nnt_max), nnt_min)
    y1 <- map_log(cer_p, cer_min, cer_max)
    y2 <- map_log(rrr_p, rrr_min, rrr_max)
    y3 <- map_log(nnt, nnt_min, nnt_max)
    lines(c(x_cer, x_rrr, x_nnt), c(y1, y2, y3), lwd = 2, col = "red")
    points(c(x_cer, x_rrr, x_nnt), c(y1, y2, y3), pch = 16, col = "red")
  }
}

calc_ec1 <- function(a, b, c, d, yates_chi2 = FALSE) {
  conf.level <- 0.95
  z <- qnorm(1 - (1 - conf.level) / 2)
  nt <- a + b
  nc <- c + d
  n <- nt + nc
  tab <- matrix(c(a, b, c, d), nrow = 2, byrow = TRUE)
  p_chi2 <- suppressWarnings(chisq.test(tab, correct = yates_chi2)$p.value)
  p_fisher_1s <- suppressWarnings(fisher.test(tab, alternative = "less")$p.value)
  p_binom_point <- {
    tot_evt <- a + c
    p0 <- if (n == 0) NA_real_ else nt / n
    if (tot_evt == 0 | is.na(p0)) NA_real_ else dbinom(a, size = tot_evt, prob = p0)
  }
  risk_t <- if (nt == 0) NA_real_ else a / nt
  risk_c <- if (nc == 0) NA_real_ else c / nc
  ci_risk_t <- wald_prop_ci(risk_t, nt, z)
  ci_risk_c <- wald_prop_ci(risk_c, nc, z)
  rar <- risk_c - risk_t
  se_rar <- if (nt <= 0 | nc <= 0) NA_real_ else sqrt(risk_t * (1 - risk_t) / nt + risk_c * (1 - risk_c) / nc)
  ci_rar <- if (is.na(se_rar)) c(NA_real_, NA_real_) else c(rar - z * se_rar, rar + z * se_rar)
  risk_t_c <- (a + 1) / (nt + 2)
  risk_c_c <- (c + 1) / (nc + 2)
  ci_risk_t_c <- wald_prop_ci(risk_t_c, nt + 2, z)
  ci_risk_c_c <- wald_prop_ci(risk_c_c, nc + 2, z)
  rar_c <- risk_c_c - risk_t_c
  se_rar_c <- sqrt(risk_t_c * (1 - risk_t_c) / (nt + 2) + risk_c_c * (1 - risk_c_c) / (nc + 2))
  ci_rar_c <- c(rar_c - z * se_rar_c, rar_c + z * se_rar_c)
  rr <- risk_t / risk_c
  if (a > 0 & c > 0) {
    se_log_rr <- sqrt((1 / a) - (1 / nt) + (1 / c) - (1 / nc))
    ci_rr <- exp(log(rr) + c(-1, 1) * z * se_log_rr)
  } else {
    aa <- a + 0.5
    bb <- b + 0.5
    cc <- c + 0.5
    dd <- d + 0.5
    rr <- (aa / (aa + bb)) / (cc / (cc + dd))
    se_log_rr <- sqrt((1 / aa) - (1 / (aa + bb)) + (1 / cc) - (1 / (cc + dd)))
    ci_rr <- exp(log(rr) + c(-1, 1) * z * se_log_rr)
  }
  rrr <- if (is.na(rr)) NA_real_ else 1 - rr
  rrr_ci <- if (any(is.na(ci_rr))) c(NA_real_, NA_real_) else sort(1 - rev(ci_rr))
  aa <- ifelse(a == 0, a + 0.5, a)
  bb <- ifelse(b == 0, b + 0.5, b)
  cc <- ifelse(c == 0, c + 0.5, c)
  dd <- ifelse(d == 0, d + 0.5, d)
  or <- (aa * dd) / (bb * cc)
  se_log_or <- sqrt(1 / aa + 1 / bb + 1 / cc + 1 / dd)
  ci_or <- exp(log(or) + c(-1, 1) * z * se_log_or)
  nnt_w <- nnt_from_rar(rar)
  nnt_w_ci <- nnt_ci_from_rar_ci(ci_rar, rar)
  nnt_wc <- nnt_from_rar(rar_c)
  nnt_wc_ci <- nnt_ci_from_rar_ci(ci_rar_c, rar_c)
  nrr <- if (is.na(rr) || !is.finite(rr) || rr == 1 || rr > 1) NA_real_ else ceiling(rr / (1 - rr))
  nrr_ci <- c(NA_real_, NA_real_)
  eer <- if (is.na(risk_c) | is.na(nnt_w) | !is.finite(nnt_w) | nnt_w == 0) NA_real_ else (risk_c - (1 / nnt_w))
  eer_ci <- ci_risk_t
  list(
    tests = list(chi2 = p_chi2, fisher1s = p_fisher_1s, binom = p_binom_point),
    risk = list(
      rt = risk_t, rt_ci = ci_risk_t, rc = risk_c, rc_ci = ci_risk_c, rar = rar, rar_ci = ci_rar,
      p1c = risk_c_c, p1c_ci = ci_risk_c_c, p2c = risk_t_c, p2c_ci = ci_risk_t_c, rarc = rar_c, rarc_ci = ci_rar_c,
      rr = rr, rr_ci = ci_rr, rrr = rrr, rrr_ci = rrr_ci, or = or, or_ci = ci_or
    ),
    impact = list(nnt_w = nnt_w, nnt_w_ci = nnt_w_ci, nnt_wc = nnt_wc, nnt_wc_ci = nnt_wc_ci, nrr = nrr, nrr_ci = nrr_ci, eer = eer, eer_ci = eer_ci)
  )
}

calc_applicability <- function(rb, rr, rr_ci) {
  if (is.na(rb) | rb <= 0 | rb >= 1 | is.na(rr) | !is.finite(rr)) {
    return(list(nntc = NA_real_, nntc_ci = c(NA_real_, NA_real_)))
  }
  rrr <- 1 - rr
  arr_ind <- rb * rrr
  nntc <- if (arr_ind == 0) Inf else 1 / arr_ind
  nntc_ci <- c(NA_real_, NA_real_)
  if (!any(is.na(rr_ci)) & all(is.finite(rr_ci))) {
    rrr1 <- 1 - rr_ci[1]
    rrr2 <- 1 - rr_ci[2]
    arr1 <- rb * rrr1
    arr2 <- rb * rrr2
    if ((arr1 <= 0 & arr2 >= 0) | (arr2 <= 0 & arr1 >= 0) | arr1 == 0 | arr2 == 0) {
      vals <- c(arr1, arr2)
      vals <- vals[vals != 0]
      if (length(vals) == 0) nntc_ci <- c(Inf, Inf) else nntc_ci <- c(ceiling(1 / max(abs(vals))), Inf)
    } else {
      inv <- sort(abs(1 / c(arr1, arr2)))
      nntc_ci <- c(ceiling(inv[1]), ceiling(inv[2]))
    }
  }
  list(nntc = nntc, nntc_ci = nntc_ci)
}

calc_nnt_veces_rb <- function(rar, rar_ci, veces) {
  if (is.na(veces) | veces <= 0 | is.na(rar) | !is.finite(rar) | rar == 0) {
    return(list(val = NA_real_, ci = c(NA_real_, NA_real_)))
  }
  val <- (1 / abs(rar)) / veces
  ci <- c(NA_real_, NA_real_)
  if (!any(is.na(rar_ci)) & all(is.finite(rar_ci))) {
    lo <- rar_ci[1]
    hi <- rar_ci[2]
    if (lo > hi) {
      tmp <- lo
      lo <- hi
      hi <- tmp
    }
    if (lo <= 0 & hi >= 0) {
      vals <- c(lo, hi)
      vals <- vals[vals != 0]
      if (length(vals) == 0) ci <- c(Inf, Inf) else ci <- c(ceiling((1 / max(abs(vals))) / veces), Inf)
    } else {
      inv <- sort((1 / abs(c(lo, hi))) / veces)
      ci <- c(ceiling(inv[1]), ceiling(inv[2]))
    }
  }
  list(val = val, ci = ci)
}

calc_nntct <- function(nnt_user_int, te, tp) {
  if (is.na(nnt_user_int) | is.na(te) | is.na(tp) | te <= 0 | tp <= 0) {
    return(NA_real_)
  }
  nnt_user_int * te / tp
}

# =========================
# Module UI
# =========================
ensayos_clinicos_1_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(paste0("
      #", ns("main_container"), " .lbl { font-size: 15px; padding-top: 9px; }
      #", ns("main_container"), " .hdr { font-size: 20px; font-weight: 700; margin: 8px 0 12px; }
      #", ns("main_container"), " .subhdr { font-weight: 700; text-align:center; margin-bottom:6px; }
      #", ns("main_container"), " .tblhead { font-weight:700; text-align:center; }
      #", ns("main_container"), " .cell { padding: 6px 4px; }
      #", ns("main_container"), " .out-box { width: 100%; border: 2px solid #1558b0; background: #d2e3fc; border-radius: 4px; padding: 10px; font-size: 15px; margin-bottom: 5px; }
      #", ns("main_container"), " .out-box-small { width: 100%; border: 2px solid #1558b0; background: #d2e3fc; border-radius: 4px; padding: 6px 8px; font-size: 13px; margin-bottom: 5px; }
      table.t22-mod { border-collapse: collapse; width: 100%; max-width: 520px; }
      table.t22-mod th, table.t22-mod td { border: 1px solid #b6c7e6; padding: 8px 10px; text-align: center; }
      table.t22-mod th { background: #d2e3fc; font-weight: 700; }
      table.t22-mod td.lbl { text-align: left; font-weight: 700; }
    "))),
    div(
      id = ns("main_container"),
      sidebarLayout(
        sidebarPanel(
          h4(uiOutput(ns("header_data_ui"))),
          fluidRow(column(4, ""), column(4, div(class = "tblhead", uiOutput(ns("lbl_event_ui")))), column(4, div(class = "tblhead", uiOutput(ns("lbl_noevent_ui"))))),
          fluidRow(column(4, div(class = "tblhead", uiOutput(ns("lbl_treat_ui")))), column(4, numericInput(ns("a"), NULL, 10, min = 0, step = 1), class = "cell"), column(4, numericInput(ns("b"), NULL, 90, min = 0, step = 1), class = "cell")),
          fluidRow(column(4, div(class = "tblhead", uiOutput(ns("lbl_ctrl_ui")))), column(4, numericInput(ns("c"), NULL, 20, min = 0, step = 1), class = "cell"), column(4, numericInput(ns("d"), NULL, 80, min = 0, step = 1), class = "cell")),
          checkboxInput(ns("yates"), "Corrección Yates", value = FALSE),
          hr(),
          h4(uiOutput(ns("header_app_ui"))),
          numericInput(ns("rb"), "Riesgo basal (%)", value = NA, min = 0, max = 100, step = 0.1),
          numericInput(ns("veces_rb"), "NNTveces RB", value = NA, min = 1, step = 1),
          hr(),
          h4(uiOutput(ns("header_time_ui"))),
          numericInput(ns("nnt_user"), "NNT estudio", value = NA, min = 1, step = 1),
          numericInput(ns("te"), "te", value = NA, min = 0, step = 0.1),
          numericInput(ns("tp"), "tp", value = NA, min = 0, step = 0.1)
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(uiOutput(ns("tab_t22_ui"), inline = TRUE), div(class = "hdr", uiOutput(ns("header_tab_ui"))), uiOutput(ns("t22_ui_out"))),
            tabPanel(
              uiOutput(ns("tab_sig_ui"), inline = TRUE), div(class = "hdr", uiOutput(ns("header_sig_ui"))),
              fluidRow(column(5, div(class = "lbl", uiOutput(ns("test_chi_ui")))), column(7, uiOutput(ns("p_chi2_out")))),
              fluidRow(column(5, div(class = "lbl", uiOutput(ns("test_fisher_ui")))), column(7, uiOutput(ns("p_fisher_out")))),
              fluidRow(column(5, div(class = "lbl", uiOutput(ns("test_binom_ui")))), column(7, uiOutput(ns("p_binom_out"))))
            ),
            tabPanel(uiOutput(ns("tab_risk_ui"), inline = TRUE), div(class = "hdr", uiOutput(ns("header_risk_ui"))), uiOutput(ns("risk_table_ui"))),
            tabPanel(uiOutput(ns("tab_imp_ui"), inline = TRUE), div(class = "hdr", uiOutput(ns("header_imp_ui"))), uiOutput(ns("impact_table_ui"))),
            tabPanel(uiOutput(ns("tab_app_ui"), inline = TRUE), div(class = "hdr", uiOutput(ns("header_app_tab_ui"))), uiOutput(ns("app_table_ui"))),
            tabPanel(
              uiOutput(ns("tab_time_ui"), inline = TRUE), div(class = "hdr", uiOutput(ns("header_time_tab_ui"))),
              fluidRow(column(5, div(class = "lbl", uiOutput(ns("lbl_nnt_user_ui")))), column(7, uiOutput(ns("nnt_user_out")))),
              fluidRow(column(5, div(class = "lbl", uiOutput(ns("lbl_nnt_ct_ui")))), column(7, uiOutput(ns("nnt_ct_out"))))
            ),
            tabPanel(uiOutput(ns("tab_nomo_ui"), inline = TRUE), div(class = "hdr", uiOutput(ns("header_nomo_ui"))), uiOutput(ns("nomo_help_ui")), plotOutput(ns("nomograma"), height = "520px")),
            tabPanel(uiOutput(ns("tab_glos_ui"), inline = TRUE), div(class = "hdr", uiOutput(ns("header_glos_ui"))), uiOutput(ns("glossary_ui")))
          ),
          div(class = "print-template", style = "display:none;", uiOutput(ns("print_content")))
        )
      )
    )
  )
}

# =========================
# Module Server
# =========================
ensayos_clinicos_1_Server <- function(id, lang) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    res <- reactive({
      req(input$a, input$b, input$c, input$d)
      calc_ec1(a = as.integer(input$a), b = as.integer(input$b), c = as.integer(input$c), d = as.integer(input$d), yates_chi2 = isTRUE(input$yates))
    })
    observeEvent(lang(), {
      tr <- get_translations(lang(), "ensayos_clinicos_1")
      updateNumericInput(session, "rb", label = tr$lbl_rb)
      updateNumericInput(session, "veces_rb", label = tr$lbl_veces_rb)
      updateNumericInput(session, "nnt_user", label = tr$lbl_nnt_user)
      updateNumericInput(session, "te", label = tr$lbl_te)
      updateNumericInput(session, "tp", label = tr$lbl_tp)
      updateCheckboxInput(session, "yates", label = tr$lbl_cont)
    })
    output$header_data_ui <- renderUI({
      get_translations(lang(), "ensayos_clinicos_1")$header_data
    })
    output$lbl_event_ui <- renderUI({
      get_translations(lang(), "ensayos_clinicos_1")$tabs_out[1]
    })
    output$lbl_noevent_ui <- renderUI({
      get_translations(lang(), "ensayos_clinicos_1")$tabs_out[2]
    })
    output$lbl_treat_ui <- renderUI({
      get_translations(lang(), "ensayos_clinicos_1")$tabs_grp[1]
    })
    output$lbl_ctrl_ui <- renderUI({
      get_translations(lang(), "ensayos_clinicos_1")$tabs_grp[2]
    })
    output$header_app_ui <- renderUI({
      get_translations(lang(), "ensayos_clinicos_1")$header_app
    })
    output$header_time_ui <- renderUI({
      get_translations(lang(), "ensayos_clinicos_1")$header_time
    })
    output$tab_t22_ui <- renderUI({
      get_translations(lang(), "ensayos_clinicos_1")$header_tab
    })
    output$tab_sig_ui <- renderUI({
      get_translations(lang(), "ensayos_clinicos_1")$header_sig
    })
    output$tab_risk_ui <- renderUI({
      get_translations(lang(), "ensayos_clinicos_1")$header_risk
    })
    output$tab_imp_ui <- renderUI({
      get_translations(lang(), "ensayos_clinicos_1")$header_imp
    })
    output$tab_app_ui <- renderUI({
      get_translations(lang(), "ensayos_clinicos_1")$header_app
    })
    output$tab_time_ui <- renderUI({
      get_translations(lang(), "ensayos_clinicos_1")$header_time
    })
    output$tab_nomo_ui <- renderUI({
      if (lang() == "es") "Nomograma" else "Nomogram"
    })
    output$tab_glos_ui <- renderUI({
      get_translations(lang(), "ensayos_clinicos_1")$header_glos
    })
    output$header_tab_ui <- renderUI({
      get_translations(lang(), "ensayos_clinicos_1")$header_tab
    })
    output$header_sig_ui <- renderUI({
      get_translations(lang(), "ensayos_clinicos_1")$header_sig
    })
    output$header_risk_ui <- renderUI({
      get_translations(lang(), "ensayos_clinicos_1")$header_risk
    })
    output$header_imp_ui <- renderUI({
      get_translations(lang(), "ensayos_clinicos_1")$header_imp
    })
    output$header_app_tab_ui <- renderUI({
      get_translations(lang(), "ensayos_clinicos_1")$header_app
    })
    output$header_time_tab_ui <- renderUI({
      get_translations(lang(), "ensayos_clinicos_1")$header_time
    })
    output$header_nomo_ui <- renderUI({
      get_translations(lang(), "ensayos_clinicos_1")$header_nomo
    })
    output$header_glos_ui <- renderUI({
      req(lang())
      get_translations(lang(), "ensayos_clinicos_1")$header_glos
    })
    output$test_chi_ui <- renderUI({
      get_translations(lang(), "ensayos_clinicos_1")$tests[1]
    })
    output$test_fisher_ui <- renderUI({
      get_translations(lang(), "ensayos_clinicos_1")$tests[2]
    })
    output$test_binom_ui <- renderUI({
      get_translations(lang(), "ensayos_clinicos_1")$tests[3]
    })
    output$lbl_nnt_user_ui <- renderUI({
      get_translations(lang(), "ensayos_clinicos_1")$lbl_nnt_user
    })
    output$lbl_nnt_ct_ui <- renderUI({
      get_translations(lang(), "ensayos_clinicos_1")$lbl_nnt_ct
    })
    output$nomo_help_ui <- renderUI({
      p(get_translations(lang(), "ensayos_clinicos_1")$nomo_help)
    })
    output$t22_ui_out <- renderUI({
      req(lang())
      req(input$a, input$b, input$c, input$d)
      tr <- get_translations(lang(), "ensayos_clinicos_1")
      a <- as.integer(input$a)
      b <- as.integer(input$b)
      c <- as.integer(input$c)
      d <- as.integer(input$d)
      nt <- a + b
      nc <- c + d
      evt <- a + c
      noevt <- b + d
      n <- nt + nc
      tags$table(class = "t22-mod", tags$tr(tags$th(""), tags$th(tr$tabs_out[1]), tags$th(tr$tabs_out[2]), tags$th("Total")), tags$tr(tags$td(class = "lbl", tr$tabs_grp[1]), tags$td(a), tags$td(b), tags$td(nt)), tags$tr(tags$td(class = "lbl", tr$tabs_grp[2]), tags$td(c), tags$td(d), tags$td(nc)), tags$tr(tags$td(class = "lbl", "Total"), tags$td(evt), tags$td(noevt), tags$td(n)))
    })
    output$p_chi2_out <- renderUI({
      div(class = "out-box-small", pval(res()$tests$chi2))
    })
    output$p_fisher_out <- renderUI({
      div(class = "out-box-small", pval(res()$tests$fisher1s))
    })
    output$p_binom_out <- renderUI({
      div(class = "out-box-small", pval(res()$tests$binom))
    })
    output$risk_table_ui <- renderUI({
      req(lang())
      r <- res()
      tr <- get_translations(lang(), "ensayos_clinicos_1")
      data <- list(list(tr$risk_rows[1], pc3(r$risk$rt), pc3(r$risk$rt_ci[1]), pc3(r$risk$rt_ci[2])), list(tr$risk_rows[2], pc3(r$risk$rc), pc3(r$risk$rc_ci[1]), pc3(r$risk$rc_ci[2])), list(tr$risk_rows[3], pc3(r$risk$rar), pc3(r$risk$rar_ci[1]), pc3(r$risk$rar_ci[2])), list(tr$risk_rows[4], pc3(r$risk$p1c), pc3(r$risk$p1c_ci[1]), pc3(r$risk$p1c_ci[2])), list(tr$risk_rows[5], pc3(r$risk$p2c), pc3(r$risk$p2c_ci[1]), pc3(r$risk$p2c_ci[2])), list(tr$risk_rows[6], pc3(r$risk$rarc), pc3(r$risk$rarc_ci[1]), pc3(r$risk$rarc_ci[2])), list(tr$risk_rows[7], num3(r$risk$rr), num3(r$risk$rr_ci[1]), num3(r$risk$rr_ci[2])), list(tr$risk_rows[8], pc3(r$risk$rrr), pc3(r$risk$rrr_ci[1]), pc3(r$risk$rrr_ci[2])), list(tr$risk_rows[9], num3(r$risk$or), num3(r$risk$or_ci[1]), num3(r$risk$or_ci[2])))
      div(fluidRow(column(4, div(class = "subhdr", tr$res_risk[1])), column(3, div(class = "subhdr", tr$res_risk[2])), column(2, div(class = "subhdr", tr$res_risk[3])), column(3, div(class = "subhdr", tr$res_risk[4]))), lapply(data, function(row) {
        fluidRow(column(4, div(class = "lbl", row[[1]])), column(3, div(class = "out-box-small", row[[2]])), column(2, div(class = "out-box-small", row[[3]])), column(3, div(class = "out-box-small", row[[4]])))
      }))
    })
    output$impact_table_ui <- renderUI({
      req(lang())
      r <- res()
      tr <- get_translations(lang(), "ensayos_clinicos_1")
      rar <- r$risk$rar
      nntw <- r$impact$nnt_w
      ci_nntw <- r$impact$nnt_w_ci
      rarc <- r$risk$rarc
      nntwc <- r$impact$nnt_wc
      ci_nntwc <- r$impact$nnt_wc_ci
      veces <- if (is.na(input$veces_rb)) NA_real_ else as.numeric(input$veces_rb)
      nntv <- calc_nnt_veces_rb(r$risk$rar, r$risk$rar_ci, veces)
      rows <- list()
      if (!is.na(nntw) & is.finite(nntw) & rar > 0) {
        rows[[1]] <- list(tr$impact_rows[1], fmt_int_or_inf(nntw), fmt_int_or_inf(ci_nntw[1]), fmt_int_or_inf(ci_nntw[2]))
        rows[[2]] <- list(tr$impact_rows[2], "", "", "")
      } else if (!is.na(nntw) & is.finite(nntw) & rar < 0) {
        rows[[1]] <- list(tr$impact_rows[1], "", "", "")
        rows[[2]] <- list(tr$impact_rows[2], fmt_int_or_inf(nntw), fmt_int_or_inf(ci_nntw[1]), fmt_int_or_inf(ci_nntw[2]))
      } else {
        rows[[1]] <- list(tr$impact_rows[1], "", "", "")
        rows[[2]] <- list(tr$impact_rows[2], "", "", "")
      }
      if (!is.na(nntwc) & is.finite(nntwc) & rarc > 0) {
        rows[[3]] <- list(tr$impact_rows[3], fmt_int_or_inf(nntwc), fmt_int_or_inf(ci_nntwc[1]), fmt_int_or_inf(ci_nntwc[2]))
        rows[[4]] <- list(tr$impact_rows[4], "", "", "")
      } else if (!is.na(nntwc) & is.finite(nntwc) & rarc < 0) {
        rows[[3]] <- list(tr$impact_rows[3], "", "", "")
        rows[[4]] <- list(tr$impact_rows[4], fmt_int_or_inf(nntwc), fmt_int_or_inf(ci_nntwc[1]), fmt_int_or_inf(ci_nntwc[2]))
      } else {
        rows[[3]] <- list(tr$impact_rows[3], "", "", "")
        rows[[4]] <- list(tr$impact_rows[4], "", "", "")
      }
      rows[[5]] <- list(tr$impact_rows[5], num3(r$impact$nrr), "", "")
      rows[[6]] <- list(tr$impact_rows[6], pc3(r$impact$eer), pc3(r$impact$eer_ci[1]), pc3(r$impact$eer_ci[2]))
      rows[[7]] <- list(tr$impact_rows[7], fmt_int_or_inf(nntv$val), fmt_int_or_inf(nntv$ci[1]), fmt_int_or_inf(nntv$ci[2]))
      div(fluidRow(column(4, div(class = "subhdr", tr$res_risk[1])), column(3, div(class = "subhdr", tr$res_risk[2])), column(2, div(class = "subhdr", tr$res_risk[3])), column(3, div(class = "subhdr", tr$res_risk[4]))), lapply(rows, function(row) {
        fluidRow(column(4, div(class = "lbl", row[[1]])), column(3, div(class = "out-box-small", row[[2]])), column(2, div(class = "out-box-small", row[[3]])), column(3, div(class = "out-box-small", row[[4]])))
      }))
    })
    output$app_table_ui <- renderUI({
      tr <- get_translations(lang(), "ensayos_clinicos_1")
      rb <- if (is.na(input$rb)) NA_real_ else input$rb / 100
      app <- calc_applicability(rb, res()$risk$rr, res()$risk$rr_ci)
      div(fluidRow(column(4, div(class = "subhdr", tr$res_risk[1])), column(3, div(class = "subhdr", tr$res_risk[2])), column(2, div(class = "subhdr", tr$res_risk[3])), column(3, div(class = "subhdr", tr$res_risk[4]))), fluidRow(column(4, div(class = "lbl", "NNTc")), column(3, div(class = "out-box-small", fmt_int_or_inf(app$nntc))), column(2, div(class = "out-box-small", fmt_int_or_inf(app$nntc_ci[1]))), column(3, div(class = "out-box-small", fmt_int_or_inf(app$nntc_ci[2])))))
    })
    output$nnt_user_out <- renderUI({
      div(class = "out-box", ifelse(is.na(input$nnt_user), "", as.character(as.integer(input$nnt_user))))
    })
    output$nnt_ct_out <- renderUI({
      nntct <- calc_nntct(input$nnt_user, input$te, input$tp)
      div(class = "out-box", ifelse(is.na(nntct), "", sprintf("%.3f", nntct)))
    })
    output$nomograma <- renderPlot({
      nomogram_plot(res()$risk$rc, res()$risk$rrr)
    })
    output$glossary_ui <- renderUI({
      tags$ul(lapply(get_translations(lang(), "ensayos_clinicos_1")$glossary, tags$li))
    })
    output$print_content <- renderUI({
      req(lang())
      tr <- get_translations(lang(), "ensayos_clinicos_1")
      tagList(div(class = "printable-section", h3(tr$header_tab), uiOutput(ns("t22_ui_out"))), div(class = "printable-section", h3(tr$header_sig), fluidRow(column(6, tr$tests[1]), column(6, pval(res()$tests$chi2))), fluidRow(column(6, tr$tests[2]), column(6, pval(res()$tests$fisher1s))), fluidRow(column(6, tr$tests[3]), column(6, pval(res()$tests$binom)))), div(class = "printable-section", h3(tr$header_risk), uiOutput(ns("risk_table_ui"))), div(class = "printable-section", h3(tr$header_imp), uiOutput(ns("impact_table_ui"))))
    })
    outputOptions(output, "t22_ui_out", suspendWhenHidden = FALSE)
    outputOptions(output, "risk_table_ui", suspendWhenHidden = FALSE)
    outputOptions(output, "impact_table_ui", suspendWhenHidden = FALSE)
  })
}
