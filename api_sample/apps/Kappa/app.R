# apps/Kappa/app.R

library(shiny)
library(bslib)
library(irr)
library(psych)
library(ggplot2)

# =========================
# Module UI
# =========================
kappa_UI <- function(id) {
    ns <- NS(id)
    tagList(
        tags$style(HTML(paste0("
      #", ns("main_container"), " .lbl { font-size: 15px; padding-top: 9px; }
      #", ns("main_container"), " .hdr { font-size: 20px; font-weight: 700; margin: 8px 0 12px; }
      #", ns("main_container"), " .out-box { width: 100%; border: 2px solid #1558b0; background: #d2e3fc; border-radius: 4px; padding: 10px; font-size: 15px; margin-bottom: 5px; }
      #", ns("main_container"), " .matrix-input-container { display: grid; grid-template-columns: 1fr 1fr; gap: 10px; }
      #", ns("main_container"), " .method-box { background-color: #f8f9fa; border-left: 5px solid #2c3e50; padding: 15px; margin-bottom: 20px; }
      #", ns("main_container"), " .frac { display: inline-block; vertical-align: middle; margin: 0 0.2em; text-align: center; }
      #", ns("main_container"), " .frac > span { display: block; padding: 0.1em; }
      #", ns("main_container"), " .frac span.top { border-bottom: thin solid black; }
    "))),
        div(
            id = ns("main_container"),
            sidebarLayout(
                sidebarPanel(
                    uiOutput(ns("input_mode_ui")),
                    hr(),
                    conditionalPanel(
                        condition = paste0("input['", ns("input_mode"), "'] == 'vec'"),
                        uiOutput(ns("vector_inputs_ui"))
                    ),
                    conditionalPanel(
                        condition = paste0("input['", ns("input_mode"), "'] == 'mat'"),
                        uiOutput(ns("matrix_inputs_ui"))
                    ),
                    hr(),
                    uiOutput(ns("conf_ui")),
                    br(),
                    actionButton(ns("help"), "Metodología", icon = icon("info-circle"))
                ),
                mainPanel(
                    uiOutput(ns("main_content")),
                    div(class = "print-template", style = "display:none;", uiOutput(ns("print_content")))
                )
            )
        )
    )
}

# =========================
# Module Server
# =========================
kappa_Server <- function(id, lang) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Internal state to keep values during language change
        vals <- reactiveValues(
            input_mode = "vec",
            r1 = "1, 1, 1, 1, 0, 0, 0, 0, 0, 0",
            r2 = "1, 1, 1, 0, 1, 0, 0, 0, 0, 0",
            m11 = 30, m12 = 10, m21 = 10, m22 = 50,
            conf = 0.95
        )

        observeEvent(input$input_mode,
            {
                vals$input_mode <- input$input_mode
            },
            ignoreInit = TRUE
        )
        observeEvent(input$r1,
            {
                vals$r1 <- input$r1
            },
            ignoreInit = TRUE
        )
        observeEvent(input$r2,
            {
                vals$r2 <- input$r2
            },
            ignoreInit = TRUE
        )
        observeEvent(input$m11,
            {
                vals$m11 <- input$m11
            },
            ignoreInit = TRUE
        )
        observeEvent(input$m12,
            {
                vals$m12 <- input$m12
            },
            ignoreInit = TRUE
        )
        observeEvent(input$m21,
            {
                vals$m21 <- input$m21
            },
            ignoreInit = TRUE
        )
        observeEvent(input$m22,
            {
                vals$m22 <- input$m22
            },
            ignoreInit = TRUE
        )
        observeEvent(input$conf,
            {
                vals$conf <- input$conf
            },
            ignoreInit = TRUE
        )

        tr <- reactive({
            get_translations(lang(), "kappa")
        })

        output$input_mode_ui <- renderUI({
            radioButtons(ns("input_mode"), tr()$input_mode_label,
                choices = setNames(c("vec", "mat"), c(tr()$choice_vec, tr()$choice_mat)),
                selected = vals$input_mode
            )
        })

        output$vector_inputs_ui <- renderUI({
            tagList(
                helpText(tr()$help_vec),
                textInput(ns("r1"), tr()$lbl_r1, vals$r1),
                textInput(ns("r2"), tr()$lbl_r2, vals$r2)
            )
        })

        output$matrix_inputs_ui <- renderUI({
            tagList(
                helpText(tr()$help_mat),
                div(
                    class = "matrix-input-container",
                    numericInput(ns("m11"), tr()$lbl_pos_pos, vals$m11, min = 0),
                    numericInput(ns("m12"), tr()$lbl_pos_neg, vals$m12, min = 0),
                    numericInput(ns("m21"), tr()$lbl_neg_pos, vals$m21, min = 0),
                    numericInput(ns("m22"), tr()$lbl_neg_neg, vals$m22, min = 0)
                )
            )
        })

        output$conf_ui <- renderUI({
            numericInput(ns("conf"), tr()$conf_label, value = vals$conf, min = 0.80, max = 0.99, step = 0.01)
        })

        results <- reactive({
            if (input$input_mode == "vec") {
                req(input$r1, input$r2)
                vec1 <- as.numeric(unlist(strsplit(input$r1, ",")))
                vec2 <- as.numeric(unlist(strsplit(input$r2, ",")))
                if (length(vec1) != length(vec2) | any(is.na(vec1)) | any(is.na(vec2))) {
                    return(NULL)
                }
                lvls <- sort(unique(c(vec1, vec2)))
                df <- data.frame(R1 = factor(vec1, levels = lvls), R2 = factor(vec2, levels = lvls))
            } else {
                req(input$m11, input$m12, input$m21, input$m22)
                # For simplicity in 2x2, use strings
                vec1 <- c(rep("+", input$m11 + input$m12), rep("-", input$m21 + input$m22))
                vec2 <- c(rep("+", input$m11), rep("-", input$m12), rep("+", input$m21), rep("-", input$m22))
                df <- data.frame(R1 = factor(vec1, levels = c("+", "-")), R2 = factor(vec2, levels = c("+", "-")))
            }

            suppressWarnings({
                df_num <- data.frame(V1 = as.numeric(df$R1), V2 = as.numeric(df$R2))
                k_simple <- kappa2(df_num)
                k_weighted <- cohen.kappa(df_num, alpha = (1 - input$conf))
            })

            list(
                simple = k_simple$value,
                p_val = k_simple$p.value,
                weighted = k_weighted$weighted.kappa,
                lower = k_weighted$confid[2, 1],
                upper = k_weighted$confid[2, 3],
                matrix = table(df$R1, df$R2),
                n = nrow(df)
            )
        })

        output$stats_table <- renderTable(
            {
                res <- results()
                if (is.null(res)) {
                    return(data.frame(Error = tr()$err_insufficient))
                }
                data.frame(
                    Métrica = c(tr()$row_kappa, tr()$row_pval, tr()$row_ci_lo, tr()$row_ci_hi, tr()$row_n),
                    Valor = c(
                        round(res$simple, 4), format.pval(res$p_val, eps = .001),
                        round(res$lower, 4), round(res$upper, 4), res$n
                    )
                )
            },
            colnames = FALSE
        )

        output$interpretation_ui <- renderUI({
            res <- results()
            if (is.null(res)) {
                return(tr()$wait_data)
            }

            val <- res$simple
            # Match cut points with Landis & Koch
            label_key <- cut(val,
                breaks = c(-Inf, 0, 0.2, 0.4, 0.6, 0.8, 1),
                labels = c("label_poor", "label_slight", "label_fair", "label_moderate", "label_substantial", "label_perfect")
            )
            label_text <- tr()[[as.character(label_key)]]

            color <- switch(as.character(label_key),
                "label_poor" = "#6c757d",
                "label_slight" = "#dc3545",
                "label_fair" = "#fd7e14",
                "label_moderate" = "#17a2b8", # Using bootstrap info color instead of neon
                "label_substantial" = "#007bff", # Using bootstrap primary
                "label_perfect" = "#28a745"
            ) # Using bootstrap success

            sig_text <- if (res$p_val < 0.05) tr()$sig_yes else tr()$sig_no

            tagList(
                tags$div(
                    style = paste0("background-color:", color, "22; padding: 15px; border-radius: 10px; border: 1px solid ", color),
                    tags$h3(label_text, style = paste0("color:", color, "; font-weight: bold; margin-bottom: 5px;")),
                    tags$p(tags$b(paste0(tr()$kappa_val, ": ", round(val, 3)))),
                    tags$p(sig_text),
                    tags$small(paste0(tr()$ci_pref, " (", input$conf * 100, "%): [", round(res$lower, 3), " ", tr()$ci_to, " ", round(res$upper, 3), "]"))
                )
            )
        })

        plot_kappa <- function() {
            res <- results()
            if (is.null(res)) {
                return(NULL)
            }
            df_mat <- as.data.frame(res$matrix)
            colnames(df_mat) <- c("R1", "R2", "Freq")

            ggplot(df_mat, aes(x = .data$R1, y = .data$R2, fill = .data$Freq)) +
                geom_tile(color = "white") +
                geom_text(aes(label = .data$Freq), color = "white", size = 8) +
                scale_fill_gradient(low = "#95a5a6", high = "#2980b9") +
                theme_minimal() +
                labs(x = tr()$lbl_eval1, y = tr()$lbl_eval2) +
                theme(axis.text = element_text(size = 14))
        }

        output$conf_matrix_plot <- renderPlot({
            plot_kappa()
        })

        output$main_content <- renderUI({
            req(results())
            tagList(
                card(
                    card_header(tr()$h_interp),
                    uiOutput(ns("interpretation_ui"))
                ),
                layout_columns(
                    card(
                        card_header(tr()$h_stats),
                        tableOutput(ns("stats_table"))
                    ),
                    card(
                        card_header(tr()$h_viz),
                        plotOutput(ns("conf_matrix_plot"))
                    )
                )
            )
        })

        observeEvent(input$help, {
            showModal(modalDialog(
                title = tr()$methodology_title,
                tags$div(
                    class = "method-box",
                    HTML(tr()$metod_desc)
                ),
                easyClose = TRUE
            ))
        })

        # Print support
        output$print_content <- renderUI({
            res <- results()
            req(res)
            tagList(
                div(
                    class = "printable-section",
                    h3(tr()$title),
                    h4(tr()$h_interp),
                    uiOutput(ns("interpretation_print"))
                ),
                div(
                    class = "printable-section",
                    h4(tr()$h_stats),
                    tableOutput(ns("stats_table_print"))
                ),
                div(
                    class = "printable-section",
                    h4(tr()$h_viz),
                    plotOutput(ns("conf_matrix_plot_print"))
                )
            )
        })

        output$interpretation_print <- renderUI({
            # Reuse logic without div wrappers if needed, but for print we can just copy
            results() # trigger
            uiOutput(ns("interpretation_ui"))
        })
        output$stats_table_print <- renderTable(
            {
                res <- results()
                if (is.null(res)) {
                    return(NULL)
                }
                data.frame(
                    Métrica = c(tr()$row_kappa, tr()$row_pval, tr()$row_ci_lo, tr()$row_ci_hi, tr()$row_n),
                    Valor = c(
                        round(res$simple, 4), format.pval(res$p_val, eps = .001),
                        round(res$lower, 4), round(res$upper, 4), res$n
                    )
                )
            },
            colnames = FALSE
        )
        output$conf_matrix_plot_print <- renderPlot({
            plot_kappa()
        })
    })
}
