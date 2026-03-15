library(shiny)
library(dplyr)
library(epiR)

# --- UI Module ---
cross_sectional_UI <- function(id) {
  ns <- NS(id)
  tagList(
    h3(id = ns("title_ui"), "Calculadora 2×2 estratificada (Transversal)"),
    tags$hr(),
    fluidRow(
      column(
        6,
        h3(id = ns("h_s1"), "Estrato 1"),
        p(id = ns("txt_s1"), "Edita los recuentos..."),
        fluidRow(column(6, strong(id = ns("h_dis_y"), "Enfermedad: Sí")), column(6, strong(id = ns("h_dis_n"), "Enfermedad: No"))),
        fluidRow(
          column(6, numericInput(ns("a1"), "Exp (+) & Yes", value = 13, min = 0, step = 1)),
          column(6, numericInput(ns("b1"), "Exp (+) & No", value = 23, min = 0, step = 1))
        ),
        fluidRow(
          column(6, numericInput(ns("c1"), "Exp (-) & Yes", value = 20, min = 0, step = 1)),
          column(6, numericInput(ns("d1"), "Exp (-) & No", value = 22, min = 0, step = 1))
        ),
        actionButton(ns("reset1"), "Reset Stratum 1", class = "btn btn-outline-primary")
      ),
      column(
        6,
        h3(id = ns("h_s2"), "Estrato 2"),
        textOutput(ns("txt_s2_dynamic")), # Use dynamic text for composed string
        fluidRow(column(6, strong(id = ns("h_dis_y2"), "Enfermedad: Sí")), column(6, strong(id = ns("h_dis_n2"), "Enfermedad: No"))),
        fluidRow(
          column(6, numericInput(ns("a2"), "Exp (+) & Yes", value = 31, min = 0, step = 1)),
          column(6, numericInput(ns("b2"), "Exp (+) & No", value = 19, min = 0, step = 1))
        ),
        fluidRow(
          column(6, numericInput(ns("c2"), "Exp (-) & Yes", value = 16, min = 0, step = 1)),
          column(6, numericInput(ns("d2"), "Exp (-) & No", value = 21, min = 0, step = 1))
        ),
        actionButton(ns("reset2"), "Reset Stratum 2", class = "btn btn-outline-primary")
      )
    ),
    tags$hr(),
    h3(id = ns("h_t2"), "Tabla 2..."),
    tableOutput(ns("tabla2")),
    tags$hr(),
    h3(id = ns("h_t3"), "Tabla 3..."),
    tableOutput(ns("tabla3")),
    tags$hr(),
    h3(id = ns("h_t4"), "Tabla 4..."),
    tableOutput(ns("tabla4")),
    tags$hr(),
    h3(id = ns("h_hom"), "Pruebas de homogeneidad"),
    tableOutput(ns("tabla_homogeneidad")),
    tags$br(),
    div(
      class = "print-template",
      uiOutput(ns("print_view"))
    )
  )
}

# --- Server Module ---
cross_sectional_Server <- function(id, lang) {
  moduleServer(id, function(input, output, session) {
    observeEvent(lang(), {
      tr <- get_translations(lang(), "transversal_estrat")

      # Update Inputs
      updateNumericInput(session, "a1", label = tr$lbl_a)
      updateNumericInput(session, "a2", label = tr$lbl_a)
      updateNumericInput(session, "b1", label = tr$lbl_b)
      updateNumericInput(session, "b2", label = tr$lbl_b)
      updateNumericInput(session, "c1", label = tr$lbl_c)
      updateNumericInput(session, "c2", label = tr$lbl_c)
      updateNumericInput(session, "d1", label = tr$lbl_d)
      updateNumericInput(session, "d2", label = tr$lbl_d)
      updateActionButton(session, "reset1", label = paste(tr$btn_reset, "1"))
      updateActionButton(session, "reset2", label = paste(tr$btn_reset, "2"))

      # Update Static UI
      ids_map <- list(
        "title_ui" = "title",
        "h_s1" = "s1",
        "txt_s1" = "txt_s1",
        "h_dis_y" = "dis_y", "h_dis_n" = "dis_n",
        "h_s2" = "s2",
        "h_dis_y2" = "dis_y", "h_dis_n2" = "dis_n",
        "h_t2" = "h_t2",
        "h_t3" = "h_t3",
        "h_t4" = "h_t4",
        "h_hom" = "h_hom"
      )
      update_static_ui(session, ids_map, tr, session$ns)
    })

    output$txt_s2_dynamic <- renderText({
      req(lang())
      tr <- get_translations(lang(), "transversal_estrat")
      paste(tr$txt_s1, paste0("(", tr$s2, ")"))
    })

    observeEvent(input$reset1, {
      updateNumericInput(session, "a1", value = 13)
      updateNumericInput(session, "b1", value = 23)
      updateNumericInput(session, "c1", value = 20)
      updateNumericInput(session, "d1", value = 22)
    })
    observeEvent(input$reset2, {
      updateNumericInput(session, "a2", value = 31)
      updateNumericInput(session, "b2", value = 19)
      updateNumericInput(session, "c2", value = 16)
      updateNumericInput(session, "d2", value = 21)
    })

    counts_rx <- reactive({
      vals <- suppressWarnings(as.numeric(c(
        input$a1, input$b1, input$c1, input$d1,
        input$a2, input$b2, input$c2, input$d2
      )))
      validate(need(!any(is.na(vals)), "Fill all cells."))
      vals <- floor(pmax(0, vals))
      validate(need(sum(vals) > 0, "Enter at least one count > 0."))
      vals
    })

    tab_raw_rx <- reactive({
      v <- counts_rx()
      arr <- array(0,
        dim = c(2, 2, 2),
        dimnames = list(
          Grupo = c("Exposicion(+)", "Exposicion(-)"),
          Enfermedad = c("Si", "No"),
          Centro = c("Estrato1", "Estrato2")
        )
      )
      arr[1, 1, 1] <- v[1]
      arr[1, 2, 1] <- v[2]
      arr[2, 1, 1] <- v[3]
      arr[2, 2, 1] <- v[4]
      arr[1, 1, 2] <- v[5]
      arr[1, 2, 2] <- v[6]
      arr[2, 1, 2] <- v[7]
      arr[2, 2, 2] <- v[8]
      as.table(arr)
    })

    res3_rx <- reactive({
      tab <- tab_raw_rx()
      for (k in 1:2) if (any(tab[, , k] == 0)) tab[, , k] <- tab[, , k] + 0.5
      epi.2by2(dat = tab, method = "cross.sectional", digits = 2, conf.level = 0.95, units = 100, interpret = FALSE, outcome = "as.columns")
    })

    fmt_ci <- function(df) {
      tr <- get_translations(lang(), "transversal_estrat")

      # Ensure numeric columns before rounding
      safe_round <- function(x, k) {
        if (is.numeric(x)) round(x, k) else x
      }

      df %>%
        dplyr::select(Medida, est, lower, upper) %>%
        dplyr::mutate(
          est = safe_round(est, 3),
          lower = safe_round(lower, 3),
          upper = safe_round(upper, 3)
        ) %>%
        setNames(c(tr$col_measure, tr$col_est, tr$col_lo, tr$col_hi))
    }

    output$tabla2 <- renderTable(
      {
        req(lang())
        tr <- get_translations(lang(), "transversal_estrat")
        res3 <- res3_rx()

        OR_estratos <- res3$massoc.detail$OR.strata.wald %>%
          as.data.frame() %>%
          mutate(Medida = c(tr$row_or_s1, tr$row_or_s2))
        OR_crudo <- res3$massoc.detail$OR.crude.wald %>%
          as.data.frame() %>%
          mutate(Medida = tr$row_or_crude)
        OR_mh <- res3$massoc.detail$OR.mh.wald %>%
          as.data.frame() %>%
          mutate(Medida = tr$row_or_mh)

        PR_estratos <- res3$massoc.detail$PR.strata.score %>%
          as.data.frame() %>%
          mutate(Medida = c(tr$row_pr_s1, tr$row_pr_s2))
        PR_crudo <- res3$massoc.detail$PR.crude.miettinen %>%
          as.data.frame() %>%
          mutate(Medida = tr$row_pr_crude)
        PR_mh <- res3$massoc.detail$PR.mh.wald %>%
          as.data.frame() %>%
          mutate(Medida = tr$row_pr_mh)

        bind_rows(OR_estratos, OR_crudo, OR_mh, PR_estratos, PR_crudo, PR_mh) %>% fmt_ci()
      },
      striped = TRUE,
      bordered = TRUE
    )

    output$tabla3 <- renderTable(
      {
        req(lang())
        tr <- get_translations(lang(), "transversal_estrat")
        res3 <- res3_rx()

        RAE_estratos <- res3$massoc.detail$ARisk.strata.wald %>%
          as.data.frame() %>%
          mutate(Medida = c(tr$row_ar_s1, tr$row_ar_s2))
        RAE_crudo <- res3$massoc.detail$ARisk.crude.wald %>%
          as.data.frame() %>%
          mutate(Medida = tr$row_ar_crude)

        RAP_estratos <- res3$massoc.detail$PARisk.strata.wald %>%
          as.data.frame() %>%
          mutate(Medida = c(tr$row_par_s1, tr$row_par_s2))
        RAP_crudo <- res3$massoc.detail$PARisk.crude.wald %>%
          as.data.frame() %>%
          mutate(Medida = tr$row_par_crude)

        bind_rows(RAE_estratos, RAE_crudo, RAP_estratos, RAP_crudo) %>% fmt_ci()
      },
      striped = TRUE,
      bordered = TRUE
    )

    output$tabla4 <- renderTable(
      {
        req(lang())
        tr <- get_translations(lang(), "transversal_estrat")
        res3 <- res3_rx()

        FAE_estratos <- res3$massoc.detail$AFRisk.strata.wald %>%
          as.data.frame() %>%
          mutate(Medida = c(tr$row_af_s1, tr$row_af_s2))
        FAE_cruda <- res3$massoc.detail$AFRisk.crude.wald %>%
          as.data.frame() %>%
          mutate(Medida = tr$row_af_crude)

        FAP_estratos <- res3$massoc.detail$PAFRisk.strata.wald %>%
          as.data.frame() %>%
          mutate(Medida = c(tr$row_paf_s1, tr$row_paf_s2))
        FAP_cruda <- res3$massoc.detail$PAFRisk.crude.wald %>%
          as.data.frame() %>%
          mutate(Medida = tr$row_paf_crude)

        bind_rows(FAE_estratos, FAE_cruda, FAP_estratos, FAP_cruda) %>% fmt_ci()
      },
      striped = TRUE,
      bordered = TRUE
    )

    output$tabla_homogeneidad <- renderTable(
      {
        req(lang())
        tr <- get_translations(lang(), "transversal_estrat")
        res3 <- res3_rx()

        data.frame(
          Test = c(tr$test_homog_or_woolf, tr$test_homog_or_bd, tr$test_homog_pr_woolf),
          Statistic = safe_round(c(res3$massoc.detail$OR.homog.woolf$statistic, res3$massoc.detail$OR.homog.brday$statistic, res3$massoc.detail$PR.homog.woolf$statistic), 3),
          df = c(res3$massoc.detail$OR.homog.woolf$parameter, res3$massoc.detail$OR.homog.brday$parameter, res3$massoc.detail$PR.homog.woolf$df),
          p_val = safe_round(c(res3$massoc.detail$OR.homog.woolf$p.value, res3$massoc.detail$OR.homog.brday$p.value, res3$massoc.detail$PR.homog.woolf$p.value), 5)
        ) %>% setNames(c(tr$col_test, tr$col_stat, tr$col_df, tr$col_pval))
      },
      striped = TRUE,
      bordered = TRUE
    )
    # ===== Print Template =====
    output$print_view <- renderUI({
      req(lang())
      ns <- session$ns
      tr <- get_translations(lang(), "transversal_estrat")
      tagList(
        div(class = "printable-section", h3(tr$title)),
        div(class = "printable-section", h4(tr$h_t2), tableOutput(ns("tabla2"))),
        div(class = "printable-section", h4(tr$h_t3), tableOutput(ns("tabla3"))),
        div(class = "printable-section", h4(tr$h_t4), tableOutput(ns("tabla4"))),
        div(class = "printable-section", h4(tr$h_hom), tableOutput(ns("tabla_homogeneidad")))
      )
    })

    outputOptions(output, "print_view", suspendWhenHidden = FALSE)
  })
}
