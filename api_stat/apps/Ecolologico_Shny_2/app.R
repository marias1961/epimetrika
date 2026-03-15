library(shiny)

# --- UI Module ---
ecologico_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("title_ui")),
    sidebarLayout(
      sidebarPanel(
        h4(uiOutput(ns("h_settings"))),
        # Menu items logic simulated with conditional panels or just inputs that switch context
        # But simpler: Just separate tabs in main panel, or inputs change?
        # The original had sidebar menu. We can use a radio button or select input to switch mode in a module
        # OR just put everything in tabs.
        # Better: Tabs in Main Panel, Inputs in Sidebar conditional on tab?
        # Standard Shiny: unconditional inputs for both models if they fit, or conditionalPanel.
        # Since logic is distinct, tabs in UI is good.

        tabsetPanel(
          id = ns("model_tabs"),
          tabPanel(
            title = uiOutput(ns("tab_lin_title")), value = "linear",
            br(),
            numericInput(ns("b0_linear"), "b0 (Avg in Unexposed):", value = 4),
            numericInput(ns("se_b0_lin"), "SE(b0)", value = NA, min = 0),
            numericInput(ns("b1_linear"), "b1 (Increment by Exp):", value = 2),
            numericInput(ns("se_b1_lin"), "SE(b1)", value = NA, min = 0),
            numericInput(ns("X_linear"), "Prev. Exposure (X):", value = 0.25, min = 0, max = 1, step = 0.01),
            actionButton(ns("btn_example_lin"), "Cargar ejemplo con SE"),
            br(), br()
          ),
          tabPanel(
            title = uiOutput(ns("tab_exp_title")), value = "exponential",
            br(),
            numericInput(ns("b0_exp"), "b0 (Log-rate Unexposed):", value = 2.2),
            numericInput(ns("se_b0_exp"), "SE(b0)", value = NA, min = 0),
            numericInput(ns("b1_exp"), "b1 (Log increment):", value = 0.4),
            numericInput(ns("se_b1_exp"), "SE(b1)", value = NA, min = 0),
            numericInput(ns("X_exp"), "Prev. Exposure (X):", value = 0.25, min = 0, max = 1, step = 0.01),
            actionButton(ns("btn_example_exp"), "Cargar ejemplo con SE"),
            br(), br()
          )
        )
      ),
      mainPanel(
        h4(uiOutput(ns("h_results"))),
        tableOutput(ns("tab_res_view"))
      )
    ),
    div(
      class = "print-template",
      uiOutput(ns("print_view"))
    )
  )
}

# --- Server Module ---
ecologico_Server <- function(id, lang) {
  moduleServer(id, function(input, output, session) {
    fmt <- function(x, digits = 4) {
      if (is.null(x) || length(x) == 0 || is.na(x) || !is.finite(x)) {
        return("")
      }
      format(round(x, digits), nsmall = digits, trim = TRUE)
    }

    fmt_pct <- function(x, digits = 2) {
      if (is.null(x) || length(x) == 0 || is.na(x) || !is.finite(x)) {
        return("")
      }
      paste0(format(round(x * 100, digits), nsmall = digits, trim = TRUE), "%")
    }

    fmt_ic <- function(lo, hi, digits = 4, is_pct = FALSE) {
      if (is.na(lo) || is.na(hi)) {
        return("---")
      }
      if (is_pct) {
        paste0("[", fmt_pct(lo, digits), " ; ", fmt_pct(hi, digits), "]")
      } else {
        paste0("[", fmt(lo, digits), " ; ", fmt(hi, digits), "]")
      }
    }

    output$title_ui <- renderUI({
      tr <- get_translations(lang(), "ecologico")
      h3(tr$title)
    })
    output$h_settings <- renderUI({
      tr <- get_translations(lang(), "ecologico")
      tr$h_settings
    })
    output$h_results <- renderUI({
      tr <- get_translations(lang(), "ecologico")
      tr$h_results
    })

    output$tab_lin_title <- renderUI({
      tr <- get_translations(lang(), "ecologico")
      tr$tab_lin
    })
    output$tab_exp_title <- renderUI({
      tr <- get_translations(lang(), "ecologico")
      tr$tab_exp
    })

    observeEvent(lang(), {
      tr <- get_translations(lang(), "ecologico")
      updateNumericInput(session, "b0_linear", label = tr$lbl_b0_lin)
      updateNumericInput(session, "se_b0_lin", label = paste0("SE(", tr$lbl_b0_lin, ")"))
      updateNumericInput(session, "b1_linear", label = tr$lbl_b1_lin)
      updateNumericInput(session, "se_b1_lin", label = paste0("SE(", tr$lbl_b1_lin, ")"))
      updateNumericInput(session, "X_linear", label = tr$lbl_X_lin)

      updateNumericInput(session, "b0_exp", label = tr$lbl_b0_exp)
      updateNumericInput(session, "se_b0_exp", label = paste0("SE(", tr$lbl_b0_exp, ")"))
      updateNumericInput(session, "b1_exp", label = tr$lbl_b1_exp)
      updateNumericInput(session, "se_b1_exp", label = paste0("SE(", tr$lbl_b1_exp, ")"))
      updateNumericInput(session, "X_exp", label = tr$lbl_X_exp)

      updateActionButton(session, "btn_example_lin", label = tr$btn_example %||% "Cargar ejemplo con SE")
      updateActionButton(session, "btn_example_exp", label = tr$btn_example %||% "Cargar ejemplo con SE")
    })

    # ---- Example Handlers ----
    observeEvent(input$btn_example_lin, {
      updateNumericInput(session, "se_b0_lin", value = 0.4)
      updateNumericInput(session, "se_b1_lin", value = 0.2)
    })
    observeEvent(input$btn_example_exp, {
      updateNumericInput(session, "se_b0_exp", value = 0.1)
      updateNumericInput(session, "se_b1_exp", value = 0.05)
    })

    # Converted to reactive for real-time updates
    res_lin <- reactive({
      b0 <- as.numeric(input$b0_linear)
      b1 <- as.numeric(input$b1_linear)
      X <- as.numeric(input$X_linear)
      sb0 <- as.numeric(input$se_b0_lin)
      sb1 <- as.numeric(input$se_b1_lin)

      ok <- is.finite(b0) && is.finite(b1) && is.finite(X)
      if (!ok) {
        return(list(ok = FALSE))
      }

      calc_lin <- function(b0, b1, X) {
        TExp <- b0 + b1
        TnExp <- b0
        DT <- b1
        RT <- if (TnExp != 0) TExp / TnExp else NA
        FAE <- if (!is.na(RT) && RT != 0) (RT - 1) / RT else NA
        Tasa_X <- b0 + b1 * X
        FAP <- if (Tasa_X != 0) (Tasa_X - TnExp) / Tasa_X else NA
        c(TExp, TnExp, DT, RT, FAE, FAP, Tasa_X)
      }

      point <- calc_lin(b0, b1, X)
      ic <- matrix(NA, nrow = 7, ncol = 2)
      sb0_v <- if (is.finite(sb0)) sb0 else 0
      sb1_v <- if (is.finite(sb1)) sb1 else 0

      if (sb0_v > 0 || sb1_v > 0) {
        B <- 2000
        set.seed(123)
        s0 <- rnorm(B, b0, sb0_v)
        s1 <- rnorm(B, b1, sb1_v)
        res_sim <- sapply(1:B, function(i) calc_lin(s0[i], s1[i], X))
        ic <- t(apply(res_sim, 1, quantile, probs = c(0.025, 0.975), na.rm = TRUE))
      } else if (is.finite(sb0) || is.finite(sb1)) {
        ic <- cbind(point, point)
      }

      list(ok = TRUE, pt = point, ic = ic, X = X)
    })

    res_exp <- reactive({
      b0 <- as.numeric(input$b0_exp)
      b1 <- as.numeric(input$b1_exp)
      X <- as.numeric(input$X_exp)
      sb0 <- as.numeric(input$se_b0_exp)
      sb1 <- as.numeric(input$se_b1_exp)

      ok <- is.finite(b0) && is.finite(b1) && is.finite(X)
      if (!ok) {
        return(list(ok = FALSE))
      }

      calc_exp <- function(b0, b1, X) {
        TExp <- exp(b0 + b1)
        TnExp <- exp(b0)
        DT <- TExp - TnExp
        RT <- exp(b1)
        FAE <- if (RT != 0) (RT - 1) / RT else NA
        Tasa_X <- exp(b0 + b1 * X)
        FAP <- if (Tasa_X != 0) (Tasa_X - TnExp) / Tasa_X else NA
        c(TExp, TnExp, DT, RT, FAE, FAP, Tasa_X)
      }

      point <- calc_exp(b0, b1, X)
      ic <- matrix(NA, nrow = 7, ncol = 2)
      sb0_v <- if (is.finite(sb0)) sb0 else 0
      sb1_v <- if (is.finite(sb1)) sb1 else 0

      if (sb0_v > 0 || sb1_v > 0) {
        B <- 2000
        set.seed(123)
        s0 <- rnorm(B, b0, sb0_v)
        s1 <- rnorm(B, b1, sb1_v)
        res_sim <- sapply(1:B, function(i) calc_exp(s0[i], s1[i], X))
        ic <- t(apply(res_sim, 1, quantile, probs = c(0.025, 0.975), na.rm = TRUE))
      } else if (is.finite(sb0) || is.finite(sb1)) {
        ic <- cbind(point, point)
      }

      list(ok = TRUE, pt = point, ic = ic, X = X)
    })

    output$tab_res_view <- renderTable({
      tr <- get_translations(lang(), "ecologico")
      v <- if (!is.null(input$model_tabs) && input$model_tabs == "exponential") res_exp() else res_lin()

      if (!v$ok) {
        return(data.frame())
      }

      df <- data.frame(
        Medida = c(
          tr$res_texp %||% "TExp",
          tr$res_tnexp %||% "TnExp",
          tr$res_dt %||% "DT",
          tr$res_rt %||% "RT",
          tr$res_fae %||% "FAE (%)",
          tr$res_fap %||% "FAP (%)",
          tr$res_tasa_x %||% "Tasa X"
        ),
        Valor = c(
          fmt(v$pt[1]), fmt(v$pt[2]), fmt(v$pt[3]), fmt(v$pt[4]),
          fmt_pct(v$pt[5]), fmt_pct(v$pt[6]), fmt(v$pt[7])
        ),
        IC = c(
          fmt_ic(v$ic[1, 1], v$ic[1, 2]),
          fmt_ic(v$ic[2, 1], v$ic[2, 2]),
          fmt_ic(v$ic[3, 1], v$ic[3, 2]),
          fmt_ic(v$ic[4, 1], v$ic[4, 2]),
          fmt_ic(v$ic[5, 1], v$ic[5, 2], is_pct = TRUE),
          fmt_ic(v$ic[6, 1], v$ic[6, 2], is_pct = TRUE),
          fmt_ic(v$ic[7, 1], v$ic[7, 2])
        ),
        stringsAsFactors = FALSE
      )
      colnames(df) <- c(tr$medida %||% "Medida", tr$valor %||% "Valor", "95% CI")
      df
    })

    # ===== Print Template =====
    output$print_view <- renderUI({
      tr <- get_translations(lang(), "ecologico")
      v <- if (input$model_tabs == "exponential") res_exp() else res_lin()

      text <- if (v$ok) {
        paste0(
          "TExp = ", fmt(v$pt[1]), " ", fmt_ic(v$ic[1, 1], v$ic[1, 2]), "\n",
          "TnExp = ", fmt(v$pt[2]), " ", fmt_ic(v$ic[2, 1], v$ic[2, 2]), "\n",
          "DT = ", fmt(v$pt[3]), " ", fmt_ic(v$ic[3, 1], v$ic[3, 2]), "\n",
          "RT = ", fmt(v$pt[4]), " ", fmt_ic(v$ic[4, 1], v$ic[4, 2]), "\n",
          "FAE = ", fmt_pct(v$pt[5]), " ", fmt_ic(v$ic[5, 1], v$ic[5, 2], is_pct = TRUE), "\n",
          "FAP = ", fmt_pct(v$pt[6]), " ", fmt_ic(v$ic[6, 1], v$ic[6, 2], is_pct = TRUE), "\n",
          "Tasa X = ", fmt(v$pt[7]), " ", fmt_ic(v$ic[7, 1], v$ic[7, 2])
        )
      } else {
        ""
      }

      tagList(
        div(class = "printable-section", h3(tr$title)),
        div(class = "printable-section", tags$pre(text))
      )
    })
    outputOptions(output, "print_view", suspendWhenHidden = FALSE)
  })
}
