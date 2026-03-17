library(shiny)

# ---------- Module UI ----------
pruebas_diagnosticas_1_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        h4(id = ns("title_data"), "Datos (prueba vs. referencia)"),
        tags$pre(id = ns("pre_diagram"), "                 Referencia +
                 |   Referencia -
Test +    TP (a)       FP (b)
Test -    FN (c)       TN (d)"),
        numericInput(ns("TP"), "TP (a)", value = 80, min = 0, step = 1),
        numericInput(ns("FP"), "FP (b)", value = 20, min = 0, step = 1),
        numericInput(ns("FN"), "FN (c)", value = 15, min = 0, step = 1),
        numericInput(ns("TN"), "TN (d)", value = 85, min = 0, step = 1),
        hr(),
        sliderInput(ns("alpha"), "Nivel de confianza", min = 0.80, max = 0.99, value = 0.95, step = 0.01),
        hr(),
        h4(id = ns("title_bayes_pre"), "Bayes (preprueba)"),
        numericInput(ns("prev_pct"), "Prevalencia preprueba (%, opcional)", value = NA, min = 0, max = 100, step = 0.1),
        helpText(id = ns("help_prev"), "Si no indicas, se usa la prevalencia observada en el estudio."),
        hr(),
        h4(id = ns("title_costs"), "Costes de error (opcional)"),
        numericInput(ns("c0"), "c0 = coste de FP", value = 1, min = 0, step = 0.1),
        numericInput(ns("c1"), "c1 = coste de FN", value = 1, min = 0, step = 0.1)
      ),
      mainPanel(
        h4(id = ns("h_tab2x2"), "Tabla 2×2"),
        tableOutput(ns("tab2x2")),
        hr(),
        h4(id = ns("h_basic"), "Medidas básicas"),
        tableOutput(ns("basic")),
        hr(),
        h4(id = ns("h_lr"), "Cocientes de probabilidad y DOR"),
        tableOutput(ns("likelihoods")),
        hr(),
        h4(id = ns("h_bayes"), "Bayes: probabilidades postprueba"),
        tableOutput(ns("bayes")),
        hr(),
        h4(id = ns("h_nnd"), "Números necesarios en diagnóstico"),
        tableOutput(ns("n_numbers")),
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
pruebas_diagnosticas_1_Server <- function(id, lang) {
  moduleServer(id, function(input, output, session) {
    # ========= I18N Logic =========
    observeEvent(lang(), {
      tr <- get_translations(lang(), "pruebas_diag_1")

      # Update input labels
      updateNumericInput(session, "TP", label = tr$input_tp)
      updateNumericInput(session, "FP", label = tr$input_fp)
      updateNumericInput(session, "FN", label = tr$input_fn)
      updateNumericInput(session, "TN", label = tr$input_tn)
      updateSliderInput(session, "alpha", label = tr$confidence_level)
      updateNumericInput(session, "prev_pct", label = tr$input_prev)
      updateNumericInput(session, "c0", label = tr$input_c0)
      updateNumericInput(session, "c1", label = tr$input_c1)

      # Update static UI elements
      ids_map <- list(
        "title_data"      = "title_data",
        "pre_diagram"     = "pre_diagram",
        "title_bayes_pre" = "title_bayes_pre",
        "help_prev"       = "help_prev",
        "title_costs"     = "title_costs",
        "h_tab2x2"        = "h_tab2x2",
        "h_basic"         = "h_basic",
        "h_lr"            = "h_lr",
        "h_bayes"         = "h_bayes",
        "h_nnd"           = "h_nnd"
      )
      update_static_ui(session, ids_map, tr, session$ns)
    })

    # ========= Utilidades =========
    fmt_pct <- function(x) ifelse(is.na(x), NA, sprintf("%.2f%%", 100 * x))
    fmt_num <- function(x, k = 3) ifelse(is.na(x), NA, sprintf(paste0("%.", k, "f"), x))

    # IC Wilson para proporción x/n
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

    # LR y DOR con manejo de ceros (Haldane-Anscombe)
    safe_ratio <- function(num, den) ifelse(is.na(num) | is.na(den) | den == 0, NA, num / den)
    lr_plus <- function(Se, Sp) safe_ratio(Se, 1 - Sp)
    lr_minus <- function(Se, Sp) safe_ratio(1 - Se, Sp)
    dor <- function(Se, Sp) {
      lp <- lr_plus(Se, Sp)
      lm <- lr_minus(Se, Sp)
      safe_ratio(lp, lm)
    }

    # Bayes: de prob a odds y viceversa
    prob2odds <- function(p) ifelse(is.na(p) | p <= 0, 0, p / (1 - p))
    odds2prob <- function(o) ifelse(is.na(o), NA, o / (1 + o))

    k <- reactive({
      a <- as.integer(input$TP)
      b <- as.integer(input$FP)
      c <- as.integer(input$FN)
      d <- as.integer(input$TN)
      N <- a + b + c + d

      # Totales por filas/columnas
      ref_pos <- a + c
      ref_neg <- b + d
      test_pos <- a + b
      test_neg <- c + d

      # Proporciones
      Se <- if (ref_pos > 0) a / ref_pos else NA
      Sp <- if (ref_neg > 0) d / ref_neg else NA
      Prev_study <- if (N > 0) ref_pos / N else NA

      VPP <- if (test_pos > 0) a / test_pos else NA
      VPN <- if (test_neg > 0) d / test_neg else NA

      FPR <- if (!is.na(Sp)) 1 - Sp else NA
      FNR <- if (!is.na(Se)) 1 - Se else NA

      Acc <- if (N > 0) (a + d) / N else NA
      J <- if (!any(is.na(c(Se, Sp)))) Se + Sp - 1 else NA

      list(
        a = a, b = b, c = c, d = d, N = N,
        ref_pos = ref_pos, ref_neg = ref_neg, test_pos = test_pos, test_neg = test_neg,
        Se = Se, Sp = Sp, Prev_study = Prev_study, VPP = VPP, VPN = VPN,
        FPR = FPR, FNR = FNR, Acc = Acc, J = J
      )
    })

    output$tab2x2 <- renderTable(
      {
        x <- k()
        tr <- get_translations(lang(), "pruebas_diag_1")
        df <- matrix(c(x$a, x$b, x$c, x$d),
          nrow = 2, byrow = TRUE,
          dimnames = list(
            c(tr$tab2x2_pos, tr$tab2x2_neg),
            c(tr$tab2x2_pos, tr$tab2x2_neg)
          )
        )
        as.data.frame.matrix(df)
      },
      rownames = TRUE
    )

    output$basic <- renderTable({
      x <- k()
      tr <- get_translations(lang(), "pruebas_diag_1")
      # IC Wilson para proporciones clave
      Se_ci <- wilson_ci(x$a, x$ref_pos, level = input$alpha)
      Sp_ci <- wilson_ci(x$d, x$ref_neg, level = input$alpha)
      VPP_ci <- wilson_ci(x$a, x$test_pos, level = input$alpha)
      VPN_ci <- wilson_ci(x$d, x$test_neg, level = input$alpha)
      Prev_ci <- wilson_ci(x$ref_pos, x$N, level = input$alpha)
      Acc_ci <- wilson_ci(x$a + x$d, x$N, level = input$alpha)

      df <- data.frame(
        col1 = c(
          tr$sensitivity, tr$specificity,
          tr$prevalence_study, tr$vpp, tr$vpn,
          tr$fpr, tr$fnr,
          tr$accuracy, tr$youden
        ),
        col2 = c(
          fmt_pct(x$Se), fmt_pct(x$Sp),
          fmt_pct(x$Prev_study), fmt_pct(x$VPP), fmt_pct(x$VPN),
          fmt_pct(x$FPR), fmt_pct(x$FNR),
          fmt_pct(x$Acc), fmt_pct(x$J)
        ),
        col3 = c(
          fmt_pct(Se_ci[1]), fmt_pct(Sp_ci[1]),
          fmt_pct(Prev_ci[1]), fmt_pct(VPP_ci[1]), fmt_pct(VPN_ci[1]),
          NA, NA, fmt_pct(Acc_ci[1]), NA
        ),
        col4 = c(
          fmt_pct(Se_ci[2]), fmt_pct(Sp_ci[2]),
          fmt_pct(Prev_ci[2]), fmt_pct(VPP_ci[2]), fmt_pct(VPN_ci[2]),
          NA, NA, fmt_pct(Acc_ci[2]), NA
        ),
        check.names = FALSE
      )
      setNames(df, c(tr$col_measure, tr$col_estimate, tr$col_ci_lo, tr$col_ci_hi))
    })

    output$likelihoods <- renderTable({
      x <- k()
      tr <- get_translations(lang(), "pruebas_diag_1")
      LP <- lr_plus(x$Se, x$Sp)
      LM <- lr_minus(x$Se, x$Sp)
      DOR <- dor(x$Se, x$Sp)
      df <- data.frame(
        col1 = c(tr$lr_plus, tr$lr_minus, tr$dor_label),
        col2 = c(fmt_num(LP), fmt_num(LM), fmt_num(DOR)),
        check.names = FALSE
      )
      setNames(df, c(tr$col_measure, tr$col_value))
    })

    output$bayes <- renderTable({
      x <- k()
      tr <- get_translations(lang(), "pruebas_diag_1")
      # Prevalencia preprueba (si no se da, usar la muestral)
      prev <- if (!is.na(input$prev_pct)) input$prev_pct / 100 else x$Prev_study
      LP <- lr_plus(x$Se, x$Sp)
      LM <- lr_minus(x$Se, x$Sp)

      pre_odds <- prob2odds(prev)
      post_odds_pos <- if (!is.na(pre_odds) && !is.na(LP)) pre_odds * LP else NA
      post_odds_neg <- if (!is.na(pre_odds) && !is.na(LM)) pre_odds * LM else NA

      post_prob_pos <- odds2prob(post_odds_pos)
      post_prob_neg <- odds2prob(post_odds_neg)

      df <- data.frame(
        col1 = c(
          tr$prev_pretest, tr$odds_pretest,
          "LR+", tr$odds_post_pos, tr$prob_post_pos,
          "LR−", tr$odds_post_neg, tr$prob_post_neg
        ),
        col2 = c(
          fmt_pct(prev), fmt_num(pre_odds),
          fmt_num(LP), fmt_num(post_odds_pos), fmt_pct(post_prob_pos),
          fmt_num(LM), fmt_num(post_odds_neg), fmt_pct(post_prob_neg)
        ),
        check.names = FALSE
      )
      setNames(df, c(tr$col_measure, tr$col_value))
    })

    output$n_numbers <- renderTable({
      x <- k()
      tr <- get_translations(lang(), "pruebas_diag_1")
      # NND = 1 / J si J > 0
      NND <- if (!is.na(x$J) && x$J > 0) 1 / x$J else NA

      # Tasa de error = (FP + FN) / N; NNDM = 1 / tasa_error
      err_rate <- if (x$N > 0) (x$FP + x$FN) / x$N else NA
      NNDM <- if (!is.na(err_rate) && err_rate > 0) 1 / err_rate else NA

      # Tasa de error corregida por costes c0 (FP) y c1 (FN)
      c0 <- input$c0
      c1 <- input$c1
      err_cost_rate <- if (x$N > 0 && (c0 + c1) > 0) (c0 * x$FP + c1 * x$FN) / ((c0 + c1) * x$N) else NA
      NNDMc <- if (!is.na(err_cost_rate) && err_cost_rate > 0) 1 / err_cost_rate else NA

      df <- data.frame(
        col1 = c(tr$nnd_label, tr$nndm_label, tr$nndmc_label),
        col2 = c(
          ifelse(is.na(NND), NA, sprintf("%.2f", NND)),
          ifelse(is.na(NNDM), NA, sprintf("%.2f", NNDM)),
          ifelse(is.na(NNDMc), NA, sprintf("%.2f", NNDMc))
        ),
        col3 = c(tr$rate_def_j, tr$rate_def_err, tr$rate_def_cost),
        check.names = FALSE
      )
      setNames(df, c(tr$col_measure, tr$col_value, tr$col_rate_def))
    })

    # ========= Print Duplicate Outputs =========
    output$tab2x2_print <- renderTable(
      {
        req(lang())
        tr <- get_translations(lang(), "pruebas_diag_1")
        x <- k()
        df <- matrix(c(x$a, x$b, x$c, x$d),
          nrow = 2, byrow = TRUE,
          dimnames = list(c(tr$tab2x2_pos, tr$tab2x2_neg), c(tr$tab2x2_pos, tr$tab2x2_neg))
        )
        as.data.frame.matrix(df)
      },
      rownames = TRUE
    )

    output$basic_print <- renderTable({
      req(lang())
      tr <- get_translations(lang(), "pruebas_diag_1")
      x <- k()
      Se_ci <- wilson_ci(x$a, x$ref_pos, level = input$alpha)
      Sp_ci <- wilson_ci(x$d, x$ref_neg, level = input$alpha)
      VPP_ci <- wilson_ci(x$a, x$test_pos, level = input$alpha)
      VPN_ci <- wilson_ci(x$d, x$test_neg, level = input$alpha)
      Prev_ci <- wilson_ci(x$ref_pos, x$N, level = input$alpha)
      Acc_ci <- wilson_ci(x$a + x$d, x$N, level = input$alpha)
      df <- data.frame(
        col1 = c(tr$sensitivity, tr$specificity, tr$prevalence_study, tr$vpp, tr$vpn, tr$fpr, tr$fnr, tr$accuracy, tr$youden),
        col2 = c(fmt_pct(x$Se), fmt_pct(x$Sp), fmt_pct(x$Prev_study), fmt_pct(x$VPP), fmt_pct(x$VPN), fmt_pct(x$FPR), fmt_pct(x$FNR), fmt_pct(x$Acc), fmt_pct(x$J)),
        col3 = c(fmt_pct(Se_ci[1]), fmt_pct(Sp_ci[1]), fmt_pct(Prev_ci[1]), fmt_pct(VPP_ci[1]), fmt_pct(VPN_ci[1]), NA, NA, fmt_pct(Acc_ci[1]), NA),
        col4 = c(fmt_pct(Se_ci[2]), fmt_pct(Sp_ci[2]), fmt_pct(Prev_ci[2]), fmt_pct(VPP_ci[2]), fmt_pct(VPN_ci[2]), NA, NA, fmt_pct(Acc_ci[2]), NA),
        check.names = FALSE
      )
      setNames(df, c(tr$col_measure, tr$col_estimate, tr$col_ci_lo, tr$col_ci_hi))
    })

    output$likelihoods_print <- renderTable({
      req(lang())
      tr <- get_translations(lang(), "pruebas_diag_1")
      x <- k()
      LP <- lr_plus(x$Se, x$Sp)
      LM <- lr_minus(x$Se, x$Sp)
      DOR <- dor(x$Se, x$Sp)
      df <- data.frame(col1 = c(tr$lr_plus, tr$lr_minus, tr$dor_label), col2 = c(fmt_num(LP), fmt_num(LM), fmt_num(DOR)), check.names = FALSE)
      setNames(df, c(tr$col_measure, tr$col_value))
    })

    output$bayes_print <- renderTable({
      req(lang())
      tr <- get_translations(lang(), "pruebas_diag_1")
      x <- k()
      prev <- if (!is.na(input$prev_pct)) input$prev_pct / 100 else x$Prev_study
      LP <- lr_plus(x$Se, x$Sp)
      LM <- lr_minus(x$Se, x$Sp)
      pre_odds <- prob2odds(prev)
      post_odds_pos <- if (!is.na(pre_odds) && !is.na(LP)) pre_odds * LP else NA
      post_odds_neg <- if (!is.na(pre_odds) && !is.na(LM)) pre_odds * LM else NA
      post_prob_pos <- odds2prob(post_odds_pos)
      post_prob_neg <- odds2prob(post_odds_neg)
      df <- data.frame(
        col1 = c(tr$prev_pretest, tr$odds_pretest, "LR+", tr$odds_post_pos, tr$prob_post_pos, "LR\u2212", tr$odds_post_neg, tr$prob_post_neg),
        col2 = c(fmt_pct(prev), fmt_num(pre_odds), fmt_num(LP), fmt_num(post_odds_pos), fmt_pct(post_prob_pos), fmt_num(LM), fmt_num(post_odds_neg), fmt_pct(post_prob_neg)),
        check.names = FALSE
      )
      setNames(df, c(tr$col_measure, tr$col_value))
    })

    output$n_numbers_print <- renderTable({
      req(lang())
      tr <- get_translations(lang(), "pruebas_diag_1")
      x <- k()
      NND <- if (!is.na(x$J) && x$J > 0) 1 / x$J else NA
      err_rate <- if (x$N > 0) (x$FP + x$FN) / x$N else NA
      NNDM <- if (!is.na(err_rate) && err_rate > 0) 1 / err_rate else NA
      c0 <- input$c0
      c1 <- input$c1
      err_cost_rate <- if (x$N > 0 && (c0 + c1) > 0) (c0 * x$FP + c1 * x$FN) / ((c0 + c1) * x$N) else NA
      NNDMc <- if (!is.na(err_cost_rate) && err_cost_rate > 0) 1 / err_cost_rate else NA
      df <- data.frame(
        col1 = c(tr$nnd_label, tr$nndm_label, tr$nndmc_label),
        col2 = c(ifelse(is.na(NND), NA, sprintf("%.2f", NND)), ifelse(is.na(NNDM), NA, sprintf("%.2f", NNDM)), ifelse(is.na(NNDMc), NA, sprintf("%.2f", NNDMc))),
        col3 = c(tr$rate_def_j, tr$rate_def_err, tr$rate_def_cost),
        check.names = FALSE
      )
      setNames(df, c(tr$col_measure, tr$col_value, tr$col_rate_def))
    })

    # ========= Print View Generator =========
    output$print_view <- renderUI({
      ns <- session$ns
      tr <- get_translations(lang(), "pruebas_diag_1")
      tagList(
        div(
          class = "printable-section",
          h3(tr$title_data),
          p(paste0(
            "TP=", input$TP, ", FP=", input$FP, ", FN=", input$FN, ", TN=", input$TN,
            " | ", tr$confidence_level, ": ", input$alpha * 100, "%"
          ))
        ),
        div(class = "printable-section", h4(tr$h_tab2x2), tableOutput(ns("tab2x2_print"))),
        div(class = "printable-section", h4(tr$h_basic), tableOutput(ns("basic_print"))),
        div(class = "printable-section", h4(tr$h_lr), tableOutput(ns("likelihoods_print"))),
        div(class = "printable-section", h4(tr$h_bayes), tableOutput(ns("bayes_print"))),
        div(class = "printable-section", h4(tr$h_nnd), tableOutput(ns("n_numbers_print")))
      )
    })

    # Force Rendering of Hidden Print Outputs
    outputOptions(output, "print_view", suspendWhenHidden = FALSE)
    outputOptions(output, "tab2x2_print", suspendWhenHidden = FALSE)
    outputOptions(output, "basic_print", suspendWhenHidden = FALSE)
    outputOptions(output, "likelihoods_print", suspendWhenHidden = FALSE)
    outputOptions(output, "bayes_print", suspendWhenHidden = FALSE)
    outputOptions(output, "n_numbers_print", suspendWhenHidden = FALSE)
  })
}
