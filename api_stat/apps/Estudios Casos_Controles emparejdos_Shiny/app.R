library(shiny)
library(DT)

# --- UI Module ---
cc_emparejados_UI <- function(id) {
  ns <- NS(id)
  tagList(
    h3(id = ns("title"), "Análisis de casos y controles emparejados (1:1)"),
    sidebarLayout(
      sidebarPanel(
        h4(id = ns("h_params"), "Parejas Caso y Control"),
        numericInput(ns("a"), "Caso y control expuestos (a):", 85, min = 0),
        numericInput(ns("b"), "Caso expuesto / Control no (b):", 40, min = 0),
        numericInput(ns("c"), "Caso no expuesto / Control sí (c):", 25, min = 0),
        numericInput(ns("d"), "Caso y control no expuestos (d):", 100, min = 0),
        br(),
        # Removed calculate button for real-time update
        br(), br(),
        width = 3
      ),
      mainPanel(
        h4(id = ns("h_tab_cont"), "Tabla de contingencia (Casos × Controles)"),
        DTOutput(ns("cont_table")),
        br(),
        h4(id = ns("h_res"), "Resultados principales"),
        DTOutput(ns("summary_table")),
        br(),
        h4(id = ns("h_mc"), "Prueba de McNemar"),
        DTOutput(ns("mcnemar_table")),
        tags$small(em(id = ns("legend"), "Leyenda: OR = odds ratio emparejada (b/c). OR > 1 → FAE/FAP. OR < 1 → FPE/FPP.")),
        width = 9
      )
    ),
    div(
      class = "print-template",
      uiOutput(ns("print_view"))
    )
  )
}

# --- Server Module ---
cc_emparejados_Server <- function(id, lang) {
  moduleServer(id, function(input, output, session) {
    # NEW: I18N Logic
    observeEvent(lang(), {
      tr <- get_translations(lang(), "cc_emparejados")

      # Inputs
      updateNumericInput(session, "a", label = tr$lbl_a)
      updateNumericInput(session, "b", label = tr$lbl_b)
      updateNumericInput(session, "c", label = tr$lbl_c)
      updateNumericInput(session, "d", label = tr$lbl_d)


      # Static UI mappings
      ids_map <- list(
        "title" = "title",
        "h_params" = "h_params",
        "h_tab_cont" = "h_tab_cont",
        "h_res" = "h_res",
        "h_mc" = "h_mc",
        "legend" = "legend"
      )

      update_static_ui(session, ids_map, tr, session$ns)
    })

    # Helpers
    or_ci_aprox <- function(b, c, conf.level = 0.95) {
      if (b == 0 | c == 0) {
        b2 <- ifelse(b == 0, 0.5, b)
        c2 <- ifelse(c == 0, 0.5, c)
      } else {
        b2 <- b
        c2 <- c
      }
      or <- b2 / c2
      se_log <- sqrt(1 / b2 + 1 / c2)
      z <- qnorm((1 + conf.level) / 2)
      lci <- exp(log(or) - z * se_log)
      uci <- exp(log(or) + z * se_log)
      list(OR = or, LCI = lci, UCI = uci)
    }
    or_ci_exact <- function(b, c, conf.level = 0.95) {
      n <- b + c
      if (n == 0) {
        return(list(OR = NA, LCI = NA, UCI = NA, pL = NA, pU = NA))
      }
      bt <- binom.test(b, n, p = 0.5, conf.level = conf.level)
      pL <- bt$conf.int[1]
      pU <- bt$conf.int[2]
      or_L <- pL / (1 - pL)
      or_U <- pU / (1 - pU)
      or <- ifelse(c == 0, Inf, b / c)
      list(OR = or, LCI = or_L, UCI = or_U, pL = pL, pU = pU)
    }
    fae_from_or <- function(or) (or - 1) / or
    fae_ci_from_or_ci <- function(lci, uci) c(lower = (lci - 1) / lci, upper = (uci - 1) / uci)
    diff_prop_paired_ci_aprox <- function(b, c, N, conf.level = 0.95) {
      diff <- (b - c) / N
      pb <- b / N
      pc <- c / N
      var_diff <- (pb + pc - (pb - pc)^2) / N
      se <- sqrt(var_diff)
      z <- qnorm((1 + conf.level) / 2)
      list(diff = diff, lci = diff - z * se, uci = diff + z * se)
    }
    diff_prop_exact <- function(b, c, N, conf.level = 0.95) {
      n <- b + c
      if (n == 0) {
        return(list(diff = NA, lci = NA, uci = NA))
      }
      bt <- binom.test(b, n, p = 0.5, conf.level = conf.level)
      pL <- bt$conf.int[1]
      pU <- bt$conf.int[2]
      diff_est <- (b - c) / N
      lci <- (2 * pL - 1) * n / N
      uci <- (2 * pU - 1) * n / N
      list(diff = diff_est, lci = lci, uci = uci)
    }

    # Converted to reactive for real-time updates
    realizar_analisis <- reactive({
      a <- input$a
      b <- input$b
      c <- input$c
      d <- input$d
      mat <- matrix(c(a, b, c, d), nrow = 2, byrow = TRUE)
      # dimnames will be handled in render based on lang

      N <- a + b + c + d
      casos_exp <- a + b
      controles_exp <- a + c
      p_casos_exp <- ifelse(N > 0, casos_exp / N, NA)
      p_controles_exp <- ifelse(N > 0, controles_exp / N, NA)

      or_ap <- or_ci_aprox(b, c)
      or_ex <- or_ci_exact(b, c)
      diff_ap <- diff_prop_paired_ci_aprox(b, c, N)
      diff_ex <- diff_prop_exact(b, c, N)
      chi2_uncorr <- tryCatch(as.numeric(mcnemar.test(mat, correct = FALSE)$statistic), error = function(e) NA)

      efecto_protector <- or_ap$OR < 1
      fae_ap <- fae_from_or(or_ap$OR)
      fae_ci_ap <- fae_ci_from_or_ci(or_ap$LCI, or_ap$UCI)
      fae_ex <- fae_from_or(or_ex$OR)
      fae_ci_ex <- fae_ci_from_or_ci(or_ex$LCI, or_ex$UCI)

      if (efecto_protector) {
        fae_ap <- abs(fae_ap)
        fae_ci_ap <- abs(fae_ci_ap)
        fae_ex <- abs(fae_ex)
        fae_ci_ex <- abs(fae_ci_ex)
      }

      fap <- p_casos_exp * fae_ap
      if (!is.na(chi2_uncorr) && chi2_uncorr > 0) {
        exp_low <- 1 - 1.96 / sqrt(chi2_uncorr)
        exp_high <- 1 + 1.96 / sqrt(chi2_uncorr)
        fap_L <- min(1 - (1 - fap)^exp_high, 1 - (1 - fap)^exp_low)
        fap_U <- max(1 - (1 - fap)^exp_high, 1 - (1 - fap)^exp_low)
      } else {
        fap_L <- NA
        fap_U <- NA
      }

      list(
        mat = mat, N = N,
        p_c_e = p_casos_exp, p_k_e = p_controles_exp,
        or_ap = or_ap, or_ex = or_ex,
        diff_ap = diff_ap, diff_ex = diff_ex,
        fae_ap = fae_ap, fae_ex = fae_ex,
        fae_ci_ap = fae_ci_ap, fae_ci_ex = fae_ci_ex,
        fap = fap, fap_L = fap_L, fap_U = fap_U,
        prot = efecto_protector,
        mc_u = tryCatch(mcnemar.test(mat, correct = FALSE), error = function(e) NULL),
        mc_c = tryCatch(mcnemar.test(mat, correct = TRUE), error = function(e) NULL)
      )
    })

    # Render Tables
    output$cont_table <- renderDT({
      req(realizar_analisis())
      res <- realizar_analisis()
      tr <- get_translations(lang(), "cc_emparejados")

      df <- as.data.frame(res$mat)
      df$Total <- rowSums(res$mat)
      df <- rbind(df, c(colSums(res$mat), sum(res$mat)))
      colnames(df) <- tr$tab_cont_cols
      rownames(df) <- tr$tab_cont_rows

      DT::datatable(df, options = list(dom = "t", paging = FALSE), rownames = TRUE)
    })

    output$summary_table <- renderDT({
      req(realizar_analisis())
      res <- realizar_analisis()
      tr <- get_translations(lang(), "cc_emparejados")

      fae_lbl_ap <- if (res$prot) "FPE (aprox)" else "FAE (aprox)"
      fae_lbl_ex <- if (res$prot) "FPE (exact)" else "FAE (exact)"
      fap_lbl <- if (res$prot) "FPP (aprox)" else "FAP (aprox)"

      df <- data.frame(
        Measure = c(
          tr$res_prop_c_exp, tr$res_prop_k_exp,
          tr$res_or_pair_ap, tr$res_or_pair_ex,
          tr$res_diff_ap, tr$res_diff_ex,
          fae_lbl_ap, fae_lbl_ex, fap_lbl
        ),
        Est = c(
          sprintf("%.4f (%.1f%%)", res$p_c_e, res$p_c_e * 100),
          sprintf("%.4f (%.1f%%)", res$p_k_e, res$p_k_e * 100),
          sprintf("%.4f", res$or_ap$OR), sprintf("%.4f", res$or_ex$OR),
          sprintf("%.4f", res$diff_ap$diff), sprintf("%.4f", res$diff_ex$diff),
          sprintf("%.4f (%.1f%%)", res$fae_ap, res$fae_ap * 100),
          sprintf("%.4f (%.1f%%)", res$fae_ex, res$fae_ex * 100),
          sprintf("%.4f (%.1f%%)", res$fap, res$fap * 100)
        ),
        IC95 = c(
          "-", "-",
          sprintf("%.4f - %.4f", res$or_ap$LCI, res$or_ap$UCI),
          sprintf("%.4f - %.4f", res$or_ex$LCI, res$or_ex$UCI),
          sprintf("%.4f - %.4f", res$diff_ap$lci, res$diff_ap$uci),
          sprintf("%.4f - %.4f", res$diff_ex$lci, res$diff_ex$uci),
          sprintf("%.4f - %.4f", res$fae_ci_ap[1], res$fae_ci_ap[2]),
          sprintf("%.4f - %.4f", res$fae_ci_ex[1], res$fae_ci_ex[2]),
          if (is.na(res$fap_L)) "NA" else sprintf("%.4f - %.4f", res$fap_L, res$fap_U)
        )
      )
      DT::datatable(df, options = list(dom = "t", paging = FALSE), rownames = FALSE)
    })

    output$mcnemar_table <- renderDT({
      req(realizar_analisis())
      res <- realizar_analisis()
      tr <- get_translations(lang(), "cc_emparejados")
      df <- data.frame(
        Ver = c(tr$mc_uncorr, tr$mc_corr),
        Stat = c(
          if (is.null(res$mc_u)) NA else sprintf("%.4f", res$mc_u$statistic),
          if (is.null(res$mc_c)) NA else sprintf("%.4f", res$mc_c$statistic)
        ),
        PVal = c(
          if (is.null(res$mc_u)) NA else sprintf("%.4f", res$mc_u$p.value),
          if (is.null(res$mc_c)) NA else sprintf("%.4f", res$mc_c$p.value)
        )
      )
      colnames(df) <- c(tr$mc_ver, tr$stat, tr$pval)
      DT::datatable(df, options = list(dom = "t", paging = FALSE), rownames = FALSE)
    })

    # ===== Print Template =====
    output$cont_table_print <- renderTable(
      {
        req(lang())
        tr <- get_translations(lang(), "cc_emparejados")
        req(realizar_analisis())
        res <- realizar_analisis()
        df <- as.data.frame(res$mat)
        df$Total <- rowSums(res$mat)
        df <- rbind(df, c(colSums(res$mat), sum(res$mat)))
        colnames(df) <- tr$tab_cont_cols
        rownames(df) <- tr$tab_cont_rows
        df
      },
      rownames = TRUE
    )

    output$summary_table_print <- renderTable({
      req(lang())
      tr <- get_translations(lang(), "cc_emparejados")
      req(realizar_analisis())
      res <- realizar_analisis()
      fae_lbl_ap <- if (res$prot) "FPE (aprox)" else "FAE (aprox)"
      fae_lbl_ex <- if (res$prot) "FPE (exact)" else "FAE (exact)"
      fap_lbl <- if (res$prot) "FPP (aprox)" else "FAP (aprox)"
      data.frame(
        Measure = c(
          tr$res_prop_c_exp, tr$res_prop_k_exp,
          tr$res_or_pair_ap, tr$res_or_pair_ex,
          tr$res_diff_ap, tr$res_diff_ex,
          fae_lbl_ap, fae_lbl_ex, fap_lbl
        ),
        Est = c(
          sprintf("%.4f (%.1f%%)", res$p_c_e, res$p_c_e * 100),
          sprintf("%.4f (%.1f%%)", res$p_k_e, res$p_k_e * 100),
          sprintf("%.4f", res$or_ap$OR), sprintf("%.4f", res$or_ex$OR),
          sprintf("%.4f", res$diff_ap$diff), sprintf("%.4f", res$diff_ex$diff),
          sprintf("%.4f (%.1f%%)", res$fae_ap, res$fae_ap * 100),
          sprintf("%.4f (%.1f%%)", res$fae_ex, res$fae_ex * 100),
          sprintf("%.4f (%.1f%%)", res$fap, res$fap * 100)
        ),
        IC95 = c(
          "-", "-",
          sprintf("%.4f - %.4f", res$or_ap$LCI, res$or_ap$UCI),
          sprintf("%.4f - %.4f", res$or_ex$LCI, res$or_ex$UCI),
          sprintf("%.4f - %.4f", res$diff_ap$lci, res$diff_ap$uci),
          sprintf("%.4f - %.4f", res$diff_ex$lci, res$diff_ex$uci),
          sprintf("%.4f - %.4f", res$fae_ci_ap[1], res$fae_ci_ap[2]),
          sprintf("%.4f - %.4f", res$fae_ci_ex[1], res$fae_ci_ex[2]),
          if (is.na(res$fap_L)) "NA" else sprintf("%.4f - %.4f", res$fap_L, res$fap_U)
        )
      )
    })

    output$mcnemar_table_print <- renderTable({
      req(lang())
      tr <- get_translations(lang(), "cc_emparejados")
      req(realizar_analisis())
      res <- realizar_analisis()
      df <- data.frame(
        Ver = c(tr$mc_uncorr, tr$mc_corr),
        Stat = c(
          if (is.null(res$mc_u)) NA else sprintf("%.4f", res$mc_u$statistic),
          if (is.null(res$mc_c)) NA else sprintf("%.4f", res$mc_c$statistic)
        ),
        PVal = c(
          if (is.null(res$mc_u)) NA else sprintf("%.4f", res$mc_u$p.value),
          if (is.null(res$mc_c)) NA else sprintf("%.4f", res$mc_c$p.value)
        )
      )
      colnames(df) <- c(tr$mc_ver, tr$stat, tr$pval)
      df
    })

    output$print_view <- renderUI({
      ns <- session$ns
      tr <- get_translations(lang(), "cc_emparejados")
      tagList(
        div(class = "printable-section", h3(tr$title)),
        div(class = "printable-section", h4(tr$h_cont), tableOutput(ns("cont_table_print"))),
        div(class = "printable-section", h4(tr$h_sum), tableOutput(ns("summary_table_print"))),
        div(class = "printable-section", h4(tr$h_mcn), tableOutput(ns("mcnemar_table_print")))
      )
    })

    outputOptions(output, "print_view", suspendWhenHidden = FALSE)
    outputOptions(output, "cont_table_print", suspendWhenHidden = FALSE)
    outputOptions(output, "summary_table_print", suspendWhenHidden = FALSE)
    outputOptions(output, "mcnemar_table_print", suspendWhenHidden = FALSE)
  })
}
