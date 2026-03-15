library(shiny)

# ---------- Module UI ----------
pruebas_diagnosticas_2_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        h4(id = ns("h_sense"), "Sensibilidad (Se)"),
        numericInput(ns("Se_pct"), "Se (%)", value = 84, min = 0, max = 100, step = 0.1),
        numericInput(ns("Se_lo_pct"), "IC95 inferior Se (%)", value = 76, min = 0, max = 100, step = 0.1),
        numericInput(ns("Se_hi_pct"), "IC95 superior Se (%)", value = 90, min = 0, max = 100, step = 0.1),
        hr(),
        h4(id = ns("h_spec"), "Especificidad (Sp)"),
        numericInput(ns("Sp_pct"), "Sp (%)", value = 81, min = 0, max = 100, step = 0.1),
        numericInput(ns("Sp_lo_pct"), "IC95 inferior Sp (%)", value = 73, min = 0, max = 100, step = 0.1),
        numericInput(ns("Sp_hi_pct"), "IC95 superior Sp (%)", value = 88, min = 0, max = 100, step = 0.1),
        hr(),
        h4(id = ns("h_prev_cost"), "Prevalencia y costes"),
        numericInput(ns("Prev_pct"), "Prevalencia (%, fija)", value = 20, min = 0, max = 100, step = 0.1),
        numericInput(ns("c0"), "c0 = coste FP", value = 1, min = 0, step = 0.1),
        numericInput(ns("c1"), "c1 = coste FN", value = 1, min = 0, step = 0.1),
        hr(),
        sliderInput(ns("alpha"), "Nivel de confianza", min = 0.80, max = 0.99, value = 0.95, step = 0.01),
        numericInput(ns("nsim"), "Simulaciones bootstrap", value = 5000, min = 1000, step = 1000),
        helpText(id = ns("help_params"), "Se/Sp se modelan como logit-normales a partir de sus IC95; se propagan IC95 a VPP/VPN/LR/DOR/NND etc.")
      ),
      mainPanel(
        h4(id = ns("h_in_par"), "Parámetros de entrada"),
        tableOutput(ns("in_par")),
        hr(),
        h4(id = ns("h_main_metrics"), "Métricas principales (punto e IC)"),
        tableOutput(ns("main_metrics")),
        hr(),
        h4(id = ns("h_error_metrics"), "Errores esperados y números necesarios"),
        tableOutput(ns("error_metrics")),
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
pruebas_diagnosticas_2_Server <- function(id, lang) {
  moduleServer(id, function(input, output, session) {
    observeEvent(lang(), {
      tr <- get_translations(lang(), "pruebas_diagnosticas_2")

      # Update Inputs
      updateNumericInput(session, "Se_pct", label = tr$lbl_se)
      updateNumericInput(session, "Se_lo_pct", label = tr$lbl_se_lo)
      updateNumericInput(session, "Se_hi_pct", label = tr$lbl_se_hi)

      updateNumericInput(session, "Sp_pct", label = tr$lbl_sp)
      updateNumericInput(session, "Sp_lo_pct", label = tr$lbl_sp_lo)
      updateNumericInput(session, "Sp_hi_pct", label = tr$lbl_sp_hi)

      updateNumericInput(session, "Prev_pct", label = tr$lbl_prev)
      updateNumericInput(session, "c0", label = tr$lbl_c0)
      updateNumericInput(session, "c1", label = tr$lbl_c1)

      updateSliderInput(session, "alpha", label = tr$lbl_alpha)
      updateNumericInput(session, "nsim", label = tr$lbl_nsim)

      # Update Static UI
      ids_map <- list(
        "h_sense" = "h_sense",
        "h_spec" = "h_spec",
        "h_prev_cost" = "h_prev_cost",
        "help_params" = "help_params",
        "h_in_par" = "h_in_par",
        "h_main_metrics" = "h_main_metrics",
        "h_error_metrics" = "h_error_metrics"
      )
      update_static_ui(session, ids_map, tr, session$ns)
    })

    # ========= Utilidades =========
    logit <- function(p) log(p / (1 - p))
    ilogit <- function(x) 1 / (1 + exp(-x))
    pc <- function(x) x / 100
    fmt_pct <- function(x) ifelse(is.na(x), NA, sprintf("%.2f%%", 100 * x))
    fmt_num <- function(x, k = 3) ifelse(is.na(x), NA, sprintf(paste0("%.", k, "f"), x))

    # Estimar (mu, sigma) de una logit-normal a partir de p y su IC (lo, hi)
    logitnorm_from_ci <- function(p, lo, hi, level = 0.95) {
      if (any(is.na(c(p, lo, hi))) || p <= 0 || p >= 1 || lo <= 0 || hi >= 1 || lo >= hi) {
        return(NULL)
      }
      z <- qnorm(1 - (1 - level) / 2)
      mu <- logit(p)
      sig <- (logit(hi) - logit(lo)) / (2 * z)
      if (sig <= 0 || !is.finite(sig)) {
        return(NULL)
      }
      list(mu = mu, sigma = sig)
    }

    # Derivadas diagnósticas a una prevalencia dada
    calc_metrics <- function(Se, Sp, Prev, c0 = 1, c1 = 1) {
      # Valores predictivos
      VPP <- (Se * Prev) / (Se * Prev + (1 - Sp) * (1 - Prev))
      VPN <- (Sp * (1 - Prev)) / ((1 - Se) * Prev + Sp * (1 - Prev))
      # Cocientes y DOR
      LRp <- Se / (1 - Sp)
      LRm <- (1 - Se) / Sp
      DOR <- LRp / LRm
      # Exactitud y Youden
      Acc <- Se * Prev + Sp * (1 - Prev)
      J <- Se + Sp - 1
      # Errores esperados en población unitaria
      FP <- (1 - Prev) * (1 - Sp)
      FN <- Prev * (1 - Se)
      err_rate <- FP + FN
      err_cost <- (c0 * FP + c1 * FN) / (c0 + c1)
      list(
        VPP = VPP, VPN = VPN, LRp = LRp, LRm = LRm, DOR = DOR, Acc = Acc, J = J,
        FP = FP, FN = FN, err_rate = err_rate, err_cost = err_cost
      )
    }

    # IC por bootstrap paramétrico en el espacio logit
    bootstrap_metrics <- function(Se_p, Se_lo, Se_hi, Sp_p, Sp_lo, Sp_hi, Prev, c0, c1, nsim = 5000, level = 0.95) {
      z <- qnorm(1 - (1 - level) / 2)
      Se_par <- logitnorm_from_ci(Se_p, Se_lo, Se_hi, level)
      Sp_par <- logitnorm_from_ci(Sp_p, Sp_lo, Sp_hi, level)
      if (is.null(Se_par) || is.null(Sp_par)) {
        return(NULL)
      }

      Se_s <- ilogit(rnorm(nsim, Se_par$mu, Se_par$sigma))
      Sp_s <- ilogit(rnorm(nsim, Sp_par$mu, Sp_par$sigma))

      # Evitar extremos 0/1 exactos
      eps <- 1e-9
      Se_s <- pmin(pmax(Se_s, eps), 1 - eps)
      Sp_s <- pmin(pmax(Sp_s, eps), 1 - eps)

      M <- calc_metrics(Se_s, Sp_s, Prev, c0, c1)
      pct <- function(v) {
        if (all(is.na(v))) {
          return(c(NA, NA, NA))
        }
        c(
          mean(v, na.rm = TRUE),
          quantile(v, probs = c((1 - level) / 2, 1 - (1 - level) / 2), na.rm = TRUE)
        )
      }

      out <- list(
        Se = c(Se_p, Se_lo, Se_hi),
        Sp = c(Sp_p, Sp_lo, Sp_hi),
        VPP = pct(M$VPP), VPN = pct(M$VPN),
        LRp = pct(M$LRp), LRm = pct(M$LRm),
        DOR = pct(M$DOR), Acc = pct(M$Acc),
        J = pct(M$J),
        NND = pct(ifelse(M$J > 0, 1 / M$J, NA)),
        NNDM = pct(ifelse(M$err_rate > 0, 1 / M$err_rate, NA)),
        NNDMc = pct(ifelse(M$err_cost > 0, 1 / M$err_cost, NA)),
        FP = pct(M$FP), FN = pct(M$FN),
        err_rate = pct(M$err_rate), err_cost = pct(M$err_cost)
      )
      out
    }

    boot <- reactive({
      Se <- pc(input$Se_pct)
      Se_lo <- pc(input$Se_lo_pct)
      Se_hi <- pc(input$Se_hi_pct)
      Sp <- pc(input$Sp_pct)
      Sp_lo <- pc(input$Sp_lo_pct)
      Sp_hi <- pc(input$Sp_hi_pct)
      Prev <- pc(input$Prev_pct)
      alpha <- input$alpha
      # limit nsim to reasonable range if user clears it
      nsim <- if (is.na(input$nsim) || input$nsim < 100) 1000 else input$nsim
      bootstrap_metrics(Se, Se_lo, Se_hi, Sp, Sp_lo, Sp_hi, Prev, input$c0, input$c1, nsim = nsim, level = alpha)
    })

    output$in_par <- renderTable({
      tr <- get_translations(lang(), "pruebas_diagnosticas_2")
      b <- boot()
      if (is.null(b)) {
        return(NULL)
      }

      df <- data.frame(
        item = c(tr$row_se, tr$row_sp, tr$row_prev_fixed),
        est = c(fmt_pct(b$Se[1]), fmt_pct(b$Sp[1]), fmt_pct(pc(input$Prev_pct))),
        lo = c(fmt_pct(b$Se[2]), fmt_pct(b$Sp[2]), NA),
        hi = c(fmt_pct(b$Se[3]), fmt_pct(b$Sp[3]), NA),
        check.names = FALSE
      )
      names(df) <- c(tr$col_measure, tr$col_est, tr$col_lo, tr$col_hi)
      df
    })

    output$main_metrics <- renderTable({
      tr <- get_translations(lang(), "pruebas_diagnosticas_2")
      b <- boot()
      if (is.null(b)) {
        return(NULL)
      }

      df <- data.frame(
        item = c(tr$row_ppv, tr$row_npv, tr$row_lrp, tr$row_lrn, tr$row_dor, tr$row_acc, tr$row_j, tr$row_nnd),
        est = c(
          fmt_pct(b$VPP[1]), fmt_pct(b$VPN[1]),
          fmt_num(b$LRp[1]), fmt_num(b$LRm[1]),
          fmt_num(b$DOR[1]), fmt_pct(b$Acc[1]),
          fmt_pct(b$J[1]), fmt_num(b$NND[1], 2)
        ),
        lo = c(
          fmt_pct(b$VPP[2]), fmt_pct(b$VPN[2]),
          fmt_num(b$LRp[2]), fmt_num(b$LRm[2]),
          fmt_num(b$DOR[2]), fmt_pct(b$Acc[2]),
          fmt_pct(b$J[2]), fmt_num(b$NND[2], 2)
        ),
        hi = c(
          fmt_pct(b$VPP[3]), fmt_pct(b$VPN[3]),
          fmt_num(b$LRp[3]), fmt_num(b$LRm[3]),
          fmt_num(b$DOR[3]), fmt_pct(b$Acc[3]),
          fmt_pct(b$J[3]), fmt_num(b$NND[3], 2)
        ),
        check.names = FALSE
      )
      names(df) <- c(tr$col_measure, tr$col_est, tr$col_lo, tr$col_hi)
      df
    })

    output$error_metrics <- renderTable({
      tr <- get_translations(lang(), "pruebas_diagnosticas_2")
      b <- boot()
      if (is.null(b)) {
        return(NULL)
      }

      df <- data.frame(
        item = c(
          tr$row_fp, tr$row_fn,
          tr$row_err, tr$row_nndm,
          tr$row_err_cost, tr$row_nndmc
        ),
        est = c(
          fmt_pct(b$FP[1]), fmt_pct(b$FN[1]),
          fmt_pct(b$err_rate[1]), fmt_num(b$NNDM[1], 2),
          fmt_pct(b$err_cost[1]), fmt_num(b$NNDMc[1], 2)
        ),
        lo = c(
          fmt_pct(b$FP[2]), fmt_pct(b$FN[2]),
          fmt_pct(b$err_rate[2]), fmt_num(b$NNDM[2], 2),
          fmt_pct(b$err_cost[2]), fmt_num(b$NNDMc[2], 2)
        ),
        hi = c(
          fmt_pct(b$FP[3]), fmt_pct(b$FN[3]),
          fmt_pct(b$err_rate[3]), fmt_num(b$NNDM[3], 2),
          fmt_pct(b$err_cost[3]), fmt_num(b$NNDMc[3], 2)
        ),
        check.names = FALSE
      )
      names(df) <- c(tr$col_measure, tr$col_est, tr$col_lo, tr$col_hi)
      df
    })

    # ========= Print Duplicate Outputs =========
    output$in_par_print <- renderTable({
      req(lang())
      tr <- get_translations(lang(), "pruebas_diagnosticas_2")
      b <- boot()
      if (is.null(b)) {
        return(NULL)
      }
      df <- data.frame(
        item = c(tr$row_se, tr$row_sp, tr$row_prev_fixed),
        est = c(fmt_pct(b$Se[1]), fmt_pct(b$Sp[1]), fmt_pct(pc(input$Prev_pct))),
        lo = c(fmt_pct(b$Se[2]), fmt_pct(b$Sp[2]), NA),
        hi = c(fmt_pct(b$Se[3]), fmt_pct(b$Sp[3]), NA),
        check.names = FALSE
      )
      names(df) <- c(tr$col_measure, tr$col_est, tr$col_lo, tr$col_hi)
      df
    })

    output$main_metrics_print <- renderTable({
      req(lang())
      tr <- get_translations(lang(), "pruebas_diagnosticas_2")
      b <- boot()
      if (is.null(b)) {
        return(NULL)
      }
      df <- data.frame(
        item = c(tr$row_ppv, tr$row_npv, tr$row_lrp, tr$row_lrn, tr$row_dor, tr$row_acc, tr$row_j, tr$row_nnd),
        est = c(fmt_pct(b$VPP[1]), fmt_pct(b$VPN[1]), fmt_num(b$LRp[1]), fmt_num(b$LRm[1]), fmt_num(b$DOR[1]), fmt_pct(b$Acc[1]), fmt_pct(b$J[1]), fmt_num(b$NND[1], 2)),
        lo = c(fmt_pct(b$VPP[2]), fmt_pct(b$VPN[2]), fmt_num(b$LRp[2]), fmt_num(b$LRm[2]), fmt_num(b$DOR[2]), fmt_pct(b$Acc[2]), fmt_pct(b$J[2]), fmt_num(b$NND[2], 2)),
        hi = c(fmt_pct(b$VPP[3]), fmt_pct(b$VPN[3]), fmt_num(b$LRp[3]), fmt_num(b$LRm[3]), fmt_num(b$DOR[3]), fmt_pct(b$Acc[3]), fmt_pct(b$J[3]), fmt_num(b$NND[3], 2)),
        check.names = FALSE
      )
      names(df) <- c(tr$col_measure, tr$col_est, tr$col_lo, tr$col_hi)
      df
    })

    output$error_metrics_print <- renderTable({
      req(lang())
      tr <- get_translations(lang(), "pruebas_diagnosticas_2")
      b <- boot()
      if (is.null(b)) {
        return(NULL)
      }
      df <- data.frame(
        item = c(tr$row_fp, tr$row_fn, tr$row_err, tr$row_nndm, tr$row_err_cost, tr$row_nndmc),
        est = c(fmt_pct(b$FP[1]), fmt_pct(b$FN[1]), fmt_pct(b$err_rate[1]), fmt_num(b$NNDM[1], 2), fmt_pct(b$err_cost[1]), fmt_num(b$NNDMc[1], 2)),
        lo = c(fmt_pct(b$FP[2]), fmt_pct(b$FN[2]), fmt_pct(b$err_rate[2]), fmt_num(b$NNDM[2], 2), fmt_pct(b$err_cost[2]), fmt_num(b$NNDMc[2], 2)),
        hi = c(fmt_pct(b$FP[3]), fmt_pct(b$FN[3]), fmt_pct(b$err_rate[3]), fmt_num(b$NNDM[3], 2), fmt_pct(b$err_cost[3]), fmt_num(b$NNDMc[3], 2)),
        check.names = FALSE
      )
      names(df) <- c(tr$col_measure, tr$col_est, tr$col_lo, tr$col_hi)
      df
    })

    # ========= Print View Generator =========
    output$print_view <- renderUI({
      ns <- session$ns
      tr <- get_translations(lang(), "pruebas_diagnosticas_2")
      tagList(
        div(
          class = "printable-section",
          h3(paste(tr$h_sense, "/", tr$h_spec)),
          p(paste0(
            tr$lbl_se, ": ", input$Se_pct, "% [", input$Se_lo_pct, "-", input$Se_hi_pct, "] | ",
            tr$lbl_sp, ": ", input$Sp_pct, "% [", input$Sp_lo_pct, "-", input$Sp_hi_pct, "] | ",
            tr$lbl_prev, ": ", input$Prev_pct, "%"
          ))
        ),
        div(class = "printable-section", h4(tr$h_in_par), tableOutput(ns("in_par_print"))),
        div(class = "printable-section", h4(tr$h_main_metrics), tableOutput(ns("main_metrics_print"))),
        div(class = "printable-section", h4(tr$h_error_metrics), tableOutput(ns("error_metrics_print")))
      )
    })

    # Force Rendering of Hidden Print Outputs
    outputOptions(output, "print_view", suspendWhenHidden = FALSE)
    outputOptions(output, "in_par_print", suspendWhenHidden = FALSE)
    outputOptions(output, "main_metrics_print", suspendWhenHidden = FALSE)
    outputOptions(output, "error_metrics_print", suspendWhenHidden = FALSE)
  })
}
