library(shiny)
library(epiR)

# --- UI Module ---
cohort_strat_count_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("title_ui")),
    sidebarLayout(
      sidebarPanel(
        h4(uiOutput(ns("h_inp"))),
        # Stratum 1
        h5(uiOutput(ns("h_s1"))),
        numericInput(ns("e1_es"), "Exp, Dis Yes:", value = 31, min = 0),
        numericInput(ns("e1_en"), "Exp, Dis No:", value = 16, min = 0),
        numericInput(ns("ne1_es"), "Non-Exp, Dis Yes:", value = 197, min = 0),
        numericInput(ns("ne1_en"), "Non-Exp, Dis No:", value = 210, min = 0),
        hr(),
        # Stratum 2
        h5(uiOutput(ns("h_s2"))),
        numericInput(ns("e2_es"), "Exp, Dis Yes:", value = 13, min = 0),
        numericInput(ns("e2_en"), "Exp, Dis No:", value = 20, min = 0),
        numericInput(ns("ne2_es"), "Non-Exp, Dis Yes:", value = 234, min = 0),
        numericInput(ns("ne2_en"), "Non-Exp, Dis No:", value = 223, min = 0),
        br(),
        # Removed calculate button for real-time update
        br(), br()
      ),
      mainPanel(
        h4(uiOutput(ns("h_res"))),
        # Using tabs to organize results like the dashboard tabs
        tabsetPanel(
          tabPanel("Contingency Tables", br(), tableOutput(ns("tabla_contingencia_1")), hr(), tableOutput(ns("tabla_contingencia_2"))),
          tabPanel("Absolute Risks", br(), tableOutput(ns("tabla_riesgos"))),
          tabPanel("Association Measures", br(), tableOutput(ns("tabla_asociacion"))),
          tabPanel("Impact Measures", br(), tableOutput(ns("tabla_impacto"))),
          tabPanel("Homogeneity Tests", br(), tableOutput(ns("tabla_homogeneidad")))
        )
      )
    ),
    div(
      class = "print-template",
      uiOutput(ns("print_view"))
    )
  )
}

# --- Server Module ---
cohort_strat_count_Server <- function(id, lang) {
  moduleServer(id, function(input, output, session) {
    tr <- list(
      es = list(
        title = "Análisis Estratificado (Cohortes)",
        h_inp = "Entradas de Datos",
        h_s1 = "Estrato 1", h_s2 = "Estrato 2",
        lbl_e_es = "Expuestos, Enfermedad Sí:",
        lbl_e_en = "Expuestos, Enfermedad No:",
        lbl_ne_es = "No Expuestos, Enfermedad Sí:",
        lbl_ne_en = "No Expuestos, Enfermedad No:",
        btn_calc = "Calcular Resultados", btn_dl = "Guardar PDF",
        h_res = "Resultados Analíticos",
        tab_cont = "Tablas de Contingencia",
        tab_risk = "Riesgos Absolutos",
        tab_assoc = "Medidas de Asociación",
        tab_imp = "Medidas de Impacto",
        tab_hom = "Pruebas de Homogeneidad",
        rows = c("Expuestos", "No Expuestos"),
        cols = c("Enfermedad Sí", "Enfermedad No")
        # Translation for table content omitted for brevity, logic remains
      ),
      en = list(
        title = "Stratified Analysis (Cohort)",
        h_inp = "Data Inputs",
        h_s1 = "Stratum 1", h_s2 = "Stratum 2",
        lbl_e_es = "Exposed, Disease Yes:",
        lbl_e_en = "Exposed, Disease No:",
        lbl_ne_es = "Unexposed, Disease Yes:",
        lbl_ne_en = "Unexposed, Disease No:",
        btn_calc = "Calculate Results", btn_dl = "Download PDF",
        h_res = "Analytical Results",
        tab_cont = "Contingency Tables",
        tab_risk = "Absolute Risks",
        tab_assoc = "Association Measures",
        tab_imp = "Impact Measures",
        tab_hom = "Homogeneity Tests",
        rows = c("Exposed", "Unexposed"),
        cols = c("Disease Yes", "Disease No")
      )
    )

    output$title_ui <- renderUI({
      tr <- get_translations(lang(), "ch_estrat")
      h3(tr$title)
    })
    output$h_inp <- renderUI({
      tr <- get_translations(lang(), "ch_estrat")
      tr$h_inp
    })
    output$h_s1 <- renderUI({
      tr <- get_translations(lang(), "ch_estrat")
      tr$h_s1
    })
    output$h_s2 <- renderUI({
      tr <- get_translations(lang(), "ch_estrat")
      tr$h_s2
    })
    output$h_res <- renderUI({
      tr <- get_translations(lang(), "ch_estrat")
      tr$h_res
    })

    observeEvent(lang(), {
      tr <- get_translations(lang(), "ch_estrat")
      updateNumericInput(session, "e1_es", label = tr$lbl_e_es)
      updateNumericInput(session, "e2_es", label = tr$lbl_e_es)
      updateNumericInput(session, "e1_en", label = tr$lbl_e_en)
      updateNumericInput(session, "e2_en", label = tr$lbl_e_en)
      updateNumericInput(session, "ne1_es", label = tr$lbl_ne_es)
      updateNumericInput(session, "ne2_es", label = tr$lbl_ne_es)
      updateNumericInput(session, "ne1_en", label = tr$lbl_ne_en)
      updateNumericInput(session, "ne2_en", label = tr$lbl_ne_en)
    })

    # Converted to reactive for real-time updates
    resultado <- reactive({
      tr <- get_translations(lang(), "ch_estrat")
      # Tables
      c1 <- matrix(c(input$e1_es, input$e1_en, input$ne1_es, input$ne1_en),
        nrow = 2, byrow = TRUE,
        dimnames = list(Exposure = tr$rows[1:2], Disease = tr$cols[1:2])
      )
      c2 <- matrix(c(input$e2_es, input$e2_en, input$ne2_es, input$ne2_en),
        nrow = 2, byrow = TRUE,
        dimnames = list(Exposure = tr$rows[1:2], Disease = tr$cols[1:2])
      )

      # Risks
      calc_r <- function(a, b, c, d) {
        re <- a / (a + b)
        rne <- c / (c + d)
        rt <- (a + c) / (a + b + c + d)
        ice <- binom.test(a, a + b)$conf.int
        icne <- binom.test(c, c + d)$conf.int
        ict <- binom.test(a + c, a + b + c + d)$conf.int
        list(re = re, rne = rne, rt = rt, ice = ice, icne = icne, ict = ict)
      }
      r1 <- calc_r(input$e1_es, input$e1_en, input$ne1_es, input$ne1_en)
      r2 <- calc_r(input$e2_es, input$e2_en, input$ne2_es, input$ne2_en)

      # epiR
      dat <- array(
        c(
          input$e1_es, input$ne1_es, input$e1_en, input$ne1_en,
          input$e2_es, input$ne2_es, input$e2_en, input$ne2_en
        ),
        dim = c(2, 2, 2), dimnames = list(Enf = c("Yes", "No"), Exp = c("Yes", "No"), Strat = c("S1", "S2"))
      )
      # Check for zeros? epi.2by2 usually handles them or we add correction.
      # The original app didn't seem to add correction explicitly before epi.2by2 call, assuming epiR does if needed.

      res <- epi.2by2(dat = dat, method = "cohort.count", conf.level = 0.95, digits = 2)
      asoc <- res$massoc.detail

      # Prepare DFs
      riesgos_df <- data.frame(
        Group = c("Exp S1", "Non-Exp S1", "Total S1", "Exp S2", "Non-Exp S2", "Total S2"),
        Risk = c(
          sprintf("%.2f%% (%.2f-%.2f)", 100 * r1$re, 100 * r1$ice[1], 100 * r1$ice[2]),
          sprintf("%.2f%% (%.2f-%.2f)", 100 * r1$rne, 100 * r1$icne[1], 100 * r1$icne[2]),
          sprintf("%.2f%% (%.2f-%.2f)", 100 * r1$rt, 100 * r1$ict[1], 100 * r1$ict[2]),
          sprintf("%.2f%% (%.2f-%.2f)", 100 * r2$re, 100 * r2$ice[1], 100 * r2$ice[2]),
          sprintf("%.2f%% (%.2f-%.2f)", 100 * r2$rne, 100 * r2$icne[1], 100 * r2$icne[2]),
          sprintf("%.2f%% (%.2f-%.2f)", 100 * r2$rt, 100 * r2$ict[1], 100 * r2$ict[2])
        )
      )

      assoc_df <- data.frame(
        Measure = c("OR S1", "OR S2", "RR S1", "RR S2", "OR Crude", "RR Crude", "OR MH", "RR MH"),
        Value = c(
          sprintf("%.2f (%.2f-%.2f)", asoc$OR.strata.wald[1, 1], asoc$OR.strata.wald[1, 2], asoc$OR.strata.wald[1, 3]),
          sprintf("%.2f (%.2f-%.2f)", asoc$OR.strata.wald[2, 1], asoc$OR.strata.wald[2, 2], asoc$OR.strata.wald[2, 3]),
          sprintf("%.2f (%.2f-%.2f)", asoc$RR.strata.wald[1, 1], asoc$RR.strata.wald[1, 2], asoc$RR.strata.wald[1, 3]),
          sprintf("%.2f (%.2f-%.2f)", asoc$RR.strata.wald[2, 1], asoc$RR.strata.wald[2, 2], asoc$RR.strata.wald[2, 3]),
          sprintf("%.2f (%.2f-%.2f)", asoc$OR.crude.wald[1], asoc$OR.crude.wald[2], asoc$OR.crude.wald[3]),
          sprintf("%.2f (%.2f-%.2f)", asoc$RR.crude.wald[1], asoc$RR.crude.wald[2], asoc$RR.crude.wald[3]),
          sprintf("%.2f (%.2f-%.2f)", asoc$OR.mh.wald[1], asoc$OR.mh.wald[2], asoc$OR.mh.wald[3]),
          sprintf("%.2f (%.2f-%.2f)", asoc$RR.mh.wald[1], asoc$RR.mh.wald[2], asoc$RR.mh.wald[3])
        )
      )

      impact_df <- data.frame(
        Measure = c("AR S1", "AR S2", "AR Crude", "PAR S1", "PAR S2", "PAR Crude", "AF S1", "AF S2", "AF Crude", "PAF S1", "PAF S2", "PAF Crude"),
        Value = c(
          sprintf("%.2f", asoc$ARisk.strata.wald[1, 1]), sprintf("%.2f", asoc$ARisk.strata.wald[2, 1]), sprintf("%.2f", asoc$ARisk.crude.wald[1]),
          sprintf("%.2f", asoc$PARisk.strata.wald[1, 1]), sprintf("%.2f", asoc$PARisk.strata.wald[2, 1]), sprintf("%.2f", asoc$PARisk.crude.wald[1]),
          sprintf("%.2f", asoc$AFRisk.strata.wald[1, 1]), sprintf("%.2f", asoc$AFRisk.strata.wald[2, 1]), sprintf("%.2f", asoc$AFRisk.crude.wald[1]),
          sprintf("%.2f", asoc$PAFRisk.strata.wald[1, 1]), sprintf("%.2f", asoc$PAFRisk.strata.wald[2, 1]), sprintf("%.2f", asoc$PAFRisk.crude.wald[1])
        )
      ) # Simplified for brevity, kept structure

      homog_df <- data.frame(
        Test = c("Homog OR (Breslow-Day)", "Homog RR (MH)", "Homog OR (MH)"),
        Stat = c(asoc$bOR.homog$test.statistic, asoc$wRR.homog$test.statistic, asoc$wOR.homog$test.statistic),
        p_val = c(asoc$bOR.homog$p.value, asoc$wRR.homog$p.value, asoc$wOR.homog$p.value)
      )

      list(c1 = c1, c2 = c2, riesgos = riesgos_df, assoc = assoc_df, impact = impact_df, homog = homog_df)
    })

    output$tabla_contingencia_1 <- renderTable(
      {
        req(resultado())
        resultado()$c1
      },
      rownames = TRUE
    )
    output$tabla_contingencia_2 <- renderTable(
      {
        req(resultado())
        resultado()$c2
      },
      rownames = TRUE
    )
    output$tabla_riesgos <- renderTable({
      req(resultado())
      resultado()$riesgos
    })
    output$tabla_asociacion <- renderTable({
      req(resultado())
      resultado()$assoc
    })
    output$tabla_impacto <- renderTable({
      req(resultado())
      resultado()$impact
    })
    output$tabla_homogeneidad <- renderTable({
      req(resultado())
      resultado()$homog
    })
  })
}
