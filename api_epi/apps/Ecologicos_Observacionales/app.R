library(shiny)

# ---------- Module UI ----------
ecologicos_observacionales_UI <- function(id) {
    ns <- NS(id)
    tagList(
        sidebarLayout(
            sidebarPanel(
                h4(id = ns("h_input"), "Datos de entrada"),
                numericInput(ns("t_exp"), "Tasa de enfermedad en los expuestos", value = NA, min = 0, step = 0.01),
                numericInput(ns("t_nexp"), "Tasa de enfermedad en los no expuestos", value = NA, min = 0, step = 0.01),
                numericInput(ns("prev_pct"), "Prevalencia de la enfermedad (%)", value = NA, min = 0, max = 100, step = 1),
                hr(),
                helpText(id = ns("help_formulas"), "Notas: DT = TExp − TnExp; RT = TExp/TnExp; FAE = (RT−1)/RT; FAP = p·(RT−1)/(p·(RT−1)+1), con p = prevalencia/100.")
            ),
            mainPanel(
                h4(id = ns("h_results"), "Resultados"),
                tableOutput(ns("tab_results"))
            )
        ),
        div(
            class = "print-template",
            uiOutput(ns("print_view"))
        )
    )
}

# ---------- Module Server ----------
ecologicos_observacionales_Server <- function(id, lang) {
    moduleServer(id, function(input, output, session) {
        observeEvent(lang(), {
            tr <- get_translations(lang(), "ecologicos_observacionales")

            updateNumericInput(session, "t_exp", label = tr$lbl_t_exp)
            updateNumericInput(session, "t_nexp", label = tr$lbl_t_nexp)
            updateNumericInput(session, "prev_pct", label = tr$lbl_prev_pct)

            ids_map <- list(
                "h_input" = "h_input",
                "h_results" = "h_results",
                "help_formulas" = "help_formulas"
            )
            update_static_ui(session, ids_map, tr, session$ns)
        })

        vals <- reactive({
            texp <- suppressWarnings(as.numeric(input$t_exp))
            tnexp <- suppressWarnings(as.numeric(input$t_nexp))
            p <- suppressWarnings(as.numeric(input$prev_pct)) / 100

            ok_rates <- is.finite(texp) && is.finite(tnexp) && texp >= 0 && tnexp > 0
            ok_p <- is.finite(p) && p >= 0 && p <= 1

            if (!ok_rates) {
                return(list(dt = NA, rt = NA, fae = NA, fap = NA, ok_rates = FALSE, ok_p = ok_p))
            }

            dt <- texp - tnexp
            rt <- texp / tnexp
            fae <- (rt - 1) / rt

            fap <- NA
            if (ok_p) {
                fap <- (p * (rt - 1)) / (p * (rt - 1) + 1)
            }

            list(dt = dt, rt = rt, fae = fae, fap = fap, ok_rates = TRUE, ok_p = ok_p)
        })

        fmt <- function(x, digits = 4) {
            if (!is.finite(x)) {
                return("")
            }
            format(round(x, digits), nsmall = digits, trim = TRUE)
        }

        output$tab_results <- renderTable({
            tr <- get_translations(lang(), "ecologicos_observacionales")
            v <- vals()

            df <- data.frame(
                Measure = c(tr$row_dt, tr$row_rt, tr$row_fae, tr$row_fap),
                Value = c(
                    fmt(v$dt, 4),
                    fmt(v$rt, 4),
                    fmt(v$fae, 4),
                    fmt(v$fap, 4)
                ),
                stringsAsFactors = FALSE
            )

            colnames(df) <- c(tr$col_measure, tr$col_value)
            df
        })

        output$print_view <- renderUI({
            ns <- session$ns
            tr <- get_translations(lang(), "ecologicos_observacionales")
            tagList(
                div(
                    class = "printable-section",
                    h3(tr$title),
                    p(paste0(tr$lbl_t_exp, ": ", input$t_exp)),
                    p(paste0(tr$lbl_t_nexp, ": ", input$t_nexp)),
                    p(paste0(tr$lbl_prev_pct, ": ", input$prev_pct))
                ),
                div(
                    class = "printable-section",
                    h4(tr$h_results),
                    tableOutput(ns("tab_results_print"))
                )
            )
        })

        output$tab_results_print <- renderTable({
            tr <- get_translations(lang(), "ecologicos_observacionales")
            v <- vals()
            df <- data.frame(
                Measure = c(tr$row_dt, tr$row_rt, tr$row_fae, tr$row_fap),
                Value = c(fmt(v$dt, 4), fmt(v$rt, 4), fmt(v$fae, 4), fmt(v$fap, 4)),
                stringsAsFactors = FALSE
            )
            colnames(df) <- c(tr$col_measure, tr$col_value)
            df
        })

        outputOptions(output, "print_view", suspendWhenHidden = FALSE)
        outputOptions(output, "tab_results_print", suspendWhenHidden = FALSE)
    })
}
