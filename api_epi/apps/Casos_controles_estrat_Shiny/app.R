library(shiny)
library(epiR)

# --- UI Module ---
casos_controles_estrat_UI <- function(id) {
  ns <- NS(id)
  tagList(
    h3(id = ns("title"), "Análisis de Casos y Controles Estratificados"),
    sidebarLayout(
      sidebarPanel(
        h4(id = ns("h_stratum1"), "Estrato 1"),
        numericInput(ns("casos_exp_1"), "Casos expuestos / Exposed Cases:", 50),
        numericInput(ns("casos_noexp_1"), "Casos no expuestos / Unexposed Cases:", 30),
        numericInput(ns("controles_exp_1"), "Controles expuestos / Exposed Controls:", 100),
        numericInput(ns("controles_noexp_1"), "Controles no expuestos / Unexposed Controls:", 120),
        hr(),
        h4(id = ns("h_stratum2"), "Estrato 2"),
        numericInput(ns("casos_exp_2"), "Casos expuestos / Exposed Cases:", 40),
        numericInput(ns("casos_noexp_2"), "Casos no expuestos / Unexposed Cases:", 60),
        numericInput(ns("controles_exp_2"), "Controles expuestos / Exposed Controls:", 80),
        numericInput(ns("controles_noexp_2"), "Controles no expuestos / Unexposed Controls:", 160),
        hr(),
        # Removed calculate button for real-time update
        br(), br()
      ),
      mainPanel(
        h4(id = ns("h_results"), "Resultados"),
        tableOutput(ns("tabla_datos")),
        h5(id = ns("h_assoc"), "Medidas de Asociación"),
        tableOutput(ns("medidas_asociacion")),
        h5(id = ns("h_imp_abs"), "Medidas de Impacto (Absolutas - Casos)"),
        tableOutput(ns("medidas_impacto_abs")),
        h5(id = ns("h_imp_rel"), "Medidas de Impacto (Relativas - Poblacional)"),
        tableOutput(ns("medidas_impacto_rel")),
        h5(id = ns("h_homog"), "Pruebas de Homogeneidad"),
        tableOutput(ns("pruebas_homogeneidad"))
      )
    ),
    div(
      class = "print-template",
      uiOutput(ns("print_view"))
    )
  )
}

# --- Server Module ---
casos_controles_estrat_Server <- function(id, lang) {
  moduleServer(id, function(input, output, session) {
    # NEW: I18N Logic
    observeEvent(lang(), {
      tr <- get_translations(lang(), "cc_estrat")

      updateNumericInput(session, "casos_exp_1", label = paste(tr$lbl_ce, "(1)"))
      updateNumericInput(session, "casos_noexp_1", label = paste(tr$lbl_cne, "(1)"))
      updateNumericInput(session, "controles_exp_1", label = paste(tr$lbl_kte, "(1)"))
      updateNumericInput(session, "controles_noexp_1", label = paste(tr$lbl_ktne, "(1)"))

      updateNumericInput(session, "casos_exp_2", label = paste(tr$lbl_ce, "(2)"))
      updateNumericInput(session, "casos_noexp_2", label = paste(tr$lbl_cne, "(2)"))
      updateNumericInput(session, "controles_exp_2", label = paste(tr$lbl_kte, "(2)"))
      updateNumericInput(session, "controles_noexp_2", label = paste(tr$lbl_ktne, "(2)"))


      # Static UI mappings
      ids_map <- list(
        "title" = "title",
        "h_stratum1" = "stratum1",
        "h_stratum2" = "stratum2",
        "h_results" = "res_header",
        "h_assoc" = "h_assoc",
        "h_imp_abs" = "h_imp_abs",
        "h_imp_rel" = "h_imp_rel",
        "h_homog" = "h_homog"
      )
      update_static_ui(session, ids_map, tr, session$ns)
    })

    datos_reactivos <- reactive({
      array(
        c(
          input$casos_exp_1, input$casos_noexp_1, input$controles_exp_1, input$controles_noexp_1,
          input$casos_exp_2, input$casos_noexp_2, input$controles_exp_2, input$controles_noexp_2
        ),
        dim = c(2, 2, 2),
        dimnames = list(
          Exposicion = c("Expuesto", "No expuesto"),
          Enfermedad = c("Caso", "Control"),
          Estrato = c("Estrato 1", "Estrato 2")
        )
      )
    })

    resultado <- reactive({
      epi.2by2(dat = datos_reactivos(), method = "case.control")
    })

    output$tabla_datos <- renderTable({
      datos_reactivos()
    })

    # Helper for table generation
    make_tab <- function(measures, estimates, lowers, uppers, tr) {
      data.frame(
        M = measures,
        Est = round(estimates, 4),
        IC = paste0("(", round(lowers, 4), " - ", round(uppers, 4), ")")
      ) %>% setNames(tr$cols_tab)
    }

    output$medidas_asociacion <- renderTable(
      {
        res <- resultado()
        tr <- get_translations(lang(), "cc_estrat")
        make_tab(
          tr$tab_assoc_labels,
          c(res$massoc.detail$OR.crude.wald$est, res$massoc.detail$OR.strata.wald$est[1], res$massoc.detail$OR.strata.wald$est[2], res$massoc.detail$OR.mh.wald$est),
          c(res$massoc.detail$OR.crude.wald$lower, res$massoc.detail$OR.strata.wald$lower[1], res$massoc.detail$OR.strata.wald$lower[2], res$massoc.detail$OR.mh.wald$lower),
          c(res$massoc.detail$OR.crude.wald$upper, res$massoc.detail$OR.strata.wald$upper[1], res$massoc.detail$OR.strata.wald$upper[2], res$massoc.detail$OR.mh.wald$upper),
          tr
        )
      },
      striped = TRUE
    )

    output$medidas_impacto_abs <- renderTable(
      {
        res <- resultado()
        tr <- get_translations(lang(), "cc_estrat")
        make_tab(
          tr$tab_imp_abs_labels,
          c(res$massoc.detail$AFest.crude.wald$est, res$massoc.detail$AFest.strata.wald$est[1], res$massoc.detail$AFest.strata.wald$est[2]),
          c(res$massoc.detail$AFest.crude.wald$lower, res$massoc.detail$AFest.strata.wald$lower[1], res$massoc.detail$AFest.strata.wald$lower[2]),
          c(res$massoc.detail$AFest.crude.wald$upper, res$massoc.detail$AFest.strata.wald$upper[1], res$massoc.detail$AFest.strata.wald$upper[2]),
          tr
        )
      },
      striped = TRUE
    )

    output$medidas_impacto_rel <- renderTable(
      {
        res <- resultado()
        tr <- get_translations(lang(), "cc_estrat")
        make_tab(
          tr$tab_imp_rel_labels,
          c(res$massoc.detail$PAFest.crude.wald$est, res$massoc.detail$PAFest.strata.wald$est[1], res$massoc.detail$PAFest.strata.wald$est[2]),
          c(res$massoc.detail$PAFest.crude.wald$lower, res$massoc.detail$PAFest.strata.wald$lower[1], res$massoc.detail$PAFest.strata.wald$lower[2]),
          c(res$massoc.detail$PAFest.crude.wald$upper, res$massoc.detail$PAFest.strata.wald$upper[1], res$massoc.detail$PAFest.strata.wald$upper[2]),
          tr
        )
      },
      striped = TRUE
    )

    output$pruebas_homogeneidad <- renderTable(
      {
        res <- resultado()
        tr <- get_translations(lang(), "cc_estrat")
        data.frame(
          Test = tr$tab_homog_labels,
          P_Val = round(c(res$massoc.detail$OR.homog.woolf$p.value, res$massoc.detail$OR.homog.brday$p.value), 4)
        ) %>% setNames(c(tr$cols_tab[1], "P-Value"))
      },
      striped = TRUE
    )

    # ===== Print Template =====
    output$medidas_asociacion_print <- renderTable(
      {
        req(lang())
        tr <- get_translations(lang(), "cc_estrat")
        res <- resultado()
        make_tab(
          tr$tab_assoc_labels,
          c(res$massoc.detail$OR.crude.wald$est, res$massoc.detail$OR.strata.wald$est[1], res$massoc.detail$OR.strata.wald$est[2], res$massoc.detail$OR.mh.wald$est),
          c(res$massoc.detail$OR.crude.wald$lower, res$massoc.detail$OR.strata.wald$lower[1], res$massoc.detail$OR.strata.wald$lower[2], res$massoc.detail$OR.mh.wald$lower),
          c(res$massoc.detail$OR.crude.wald$upper, res$massoc.detail$OR.strata.wald$upper[1], res$massoc.detail$OR.strata.wald$upper[2], res$massoc.detail$OR.mh.wald$upper),
          tr
        )
      },
      striped = TRUE
    )

    output$medidas_impacto_abs_print <- renderTable(
      {
        req(lang())
        tr <- get_translations(lang(), "cc_estrat")
        res <- resultado()
        make_tab(
          tr$tab_imp_abs_labels,
          c(res$massoc.detail$AFest.crude.wald$est, res$massoc.detail$AFest.strata.wald$est[1], res$massoc.detail$AFest.strata.wald$est[2]),
          c(res$massoc.detail$AFest.crude.wald$lower, res$massoc.detail$AFest.strata.wald$lower[1], res$massoc.detail$AFest.strata.wald$lower[2]),
          c(res$massoc.detail$AFest.crude.wald$upper, res$massoc.detail$AFest.strata.wald$upper[1], res$massoc.detail$AFest.strata.wald$upper[2]),
          tr
        )
      },
      striped = TRUE
    )

    output$medidas_impacto_rel_print <- renderTable(
      {
        req(lang())
        tr <- get_translations(lang(), "cc_estrat")
        res <- resultado()
        make_tab(
          tr$tab_imp_rel_labels,
          c(res$massoc.detail$PAFest.crude.wald$est, res$massoc.detail$PAFest.strata.wald$est[1], res$massoc.detail$PAFest.strata.wald$est[2]),
          c(res$massoc.detail$PAFest.crude.wald$lower, res$massoc.detail$PAFest.strata.wald$lower[1], res$massoc.detail$PAFest.strata.wald$lower[2]),
          c(res$massoc.detail$PAFest.crude.wald$upper, res$massoc.detail$PAFest.strata.wald$upper[1], res$massoc.detail$PAFest.strata.wald$upper[2]),
          tr
        )
      },
      striped = TRUE
    )

    output$pruebas_homogeneidad_print <- renderTable(
      {
        req(lang())
        tr <- get_translations(lang(), "cc_estrat")
        res <- resultado()
        data.frame(
          Test = tr$tab_homog_labels,
          P_Val = round(c(res$massoc.detail$OR.homog.woolf$p.value, res$massoc.detail$OR.homog.brday$p.value), 4)
        ) %>% setNames(c(tr$cols_tab[1], "P-Value"))
      },
      striped = TRUE
    )

    output$datos_print <- renderTable({
      datos_reactivos()
    })

    output$print_view <- renderUI({
      ns <- session$ns
      tr <- get_translations(lang(), "cc_estrat")
      tagList(
        div(
          class = "printable-section",
          h3(tr$title),
          p(paste0(
            tr$stratum1, ": ", input$casos_exp_1, "/", input$casos_noexp_1, "/", input$controles_exp_1, "/", input$controles_noexp_1,
            " | ", tr$stratum2, ": ", input$casos_exp_2, "/", input$casos_noexp_2, "/", input$controles_exp_2, "/", input$controles_noexp_2
          ))
        ),
        div(class = "printable-section", h4(tr$res_header), tableOutput(ns("datos_print"))),
        div(class = "printable-section", h4(tr$h_assoc), tableOutput(ns("medidas_asociacion_print"))),
        div(class = "printable-section", h4(tr$h_imp_abs), tableOutput(ns("medidas_impacto_abs_print"))),
        div(class = "printable-section", h4(tr$h_imp_rel), tableOutput(ns("medidas_impacto_rel_print"))),
        div(class = "printable-section", h4(tr$h_homog), tableOutput(ns("pruebas_homogeneidad_print")))
      )
    })

    outputOptions(output, "print_view", suspendWhenHidden = FALSE)
    outputOptions(output, "datos_print", suspendWhenHidden = FALSE)
    outputOptions(output, "medidas_asociacion_print", suspendWhenHidden = FALSE)
    outputOptions(output, "medidas_impacto_abs_print", suspendWhenHidden = FALSE)
    outputOptions(output, "medidas_impacto_rel_print", suspendWhenHidden = FALSE)
    outputOptions(output, "pruebas_homogeneidad_print", suspendWhenHidden = FALSE)
  })
}
