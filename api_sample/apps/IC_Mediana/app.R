library(shiny)
library(ggplot2)

# =========================
# Module UI
# =========================
ic_mediana_UI <- function(id) {
    ns <- NS(id)
    tagList(
        tags$style(HTML(paste0("
      #", ns("main_container"), " .lbl { font-size: 15px; padding-top: 9px; }
      #", ns("main_container"), " .hdr { font-size: 20px; font-weight: 700; margin: 8px 0 12px; }
      #", ns("main_container"), " .out-box { width: 100%; border: 2px solid #1558b0; background: #d2e3fc; border-radius: 4px; padding: 10px; font-size: 15px; margin-bottom: 5px; }
    "))),
        div(
            id = ns("main_container"),
            sidebarLayout(
                sidebarPanel(
                    uiOutput(ns("mode_ui")),
                    hr(),
                    conditionalPanel(
                        condition = paste0("input['", ns("mode"), "'] == 'manual'"),
                        uiOutput(ns("manual_inputs_ui"))
                    ),
                    conditionalPanel(
                        condition = paste0("input['", ns("mode"), "'] == 'upload'"),
                        uiOutput(ns("upload_inputs_ui"))
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
ic_mediana_Server <- function(id, lang) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Internal state to keep values during language change
        vals <- reactiveValues(
            mode = "manual",
            med_val = 50,
            sd_val = 10,
            n_val = 100,
            conf = 0.95,
            header = TRUE,
            selected_col = NULL
        )

        observeEvent(input$mode,
            {
                vals$mode <- input$mode
            },
            ignoreInit = TRUE
        )
        observeEvent(input$med_val,
            {
                vals$med_val <- input$med_val
            },
            ignoreInit = TRUE
        )
        observeEvent(input$sd_val,
            {
                vals$sd_val <- input$sd_val
            },
            ignoreInit = TRUE
        )
        observeEvent(input$n_val,
            {
                vals$n_val <- input$n_val
            },
            ignoreInit = TRUE
        )
        observeEvent(input$conf,
            {
                vals$conf <- input$conf
            },
            ignoreInit = TRUE
        )
        observeEvent(input$header,
            {
                vals$header <- input$header
            },
            ignoreInit = TRUE
        )
        observeEvent(input$selected_col,
            {
                vals$selected_col <- input$selected_col
            },
            ignoreInit = TRUE
        )

        tr <- reactive({
            get_translations(lang(), "ic_mediana")
        })

        output$mode_ui <- renderUI({
            radioButtons(ns("mode"), tr()$mode_label,
                choices = setNames(c("manual", "upload"), c(tr()$choice_manual, tr()$choice_upload)),
                selected = vals$mode
            )
        })

        output$manual_inputs_ui <- renderUI({
            tagList(
                numericInput(ns("med_val"), tr()$med_label, value = vals$med_val),
                numericInput(ns("sd_val"), tr()$sd_label, value = vals$sd_val),
                numericInput(ns("n_val"), tr()$n_label, value = vals$n_val, min = 2)
            )
        })

        output$upload_inputs_ui <- renderUI({
            tagList(
                fileInput(ns("file1"), tr()$file_label, accept = c(".csv", ".RData", ".rds")),
                checkboxInput(ns("header"), tr()$header_label, vals$header),
                uiOutput(ns("select_col"))
            )
        })

        output$conf_ui <- renderUI({
            sliderInput(ns("conf"), tr()$conf_label, min = 0.80, max = 0.99, value = vals$conf, step = 0.01)
        })

        data_df <- reactive({
            req(input$file1)
            ext <- tools::file_ext(input$file1$name)
            switch(ext,
                csv = read.csv(input$file1$datapath, header = input$header),
                rds = {
                    res <- readRDS(input$file1$datapath)
                    if (!is.data.frame(res)) res <- as.data.frame(res)
                    res
                },
                RData = {
                    env <- new.env()
                    load(input$file1$datapath, envir = env)
                    objs <- ls(env)
                    res <- env[[objs[1]]]
                    if (!is.data.frame(res)) res <- as.data.frame(res)
                    res
                },
                stop("Format not supported")
            )
        })

        output$select_col <- renderUI({
            df <- data_df()
            nums <- sapply(df, is.numeric)
            choices <- names(df)[nums]
            if (length(choices) == 0) {
                return(NULL)
            }
            sel <- vals$selected_col
            if (is.null(sel) || !(sel %in% choices)) sel <- choices[1]
            selectInput(ns("selected_col"), tr()$col_label, choices = choices, selected = sel)
        })

        results <- reactive({
            if (input$mode == "manual") {
                mediana <- input$med_val
                sd_sample <- input$sd_val
                n <- input$n_val
                var_name <- tr()$manual_entry
                vec_plot <- rnorm(n, mean = mediana, sd = sd_sample)
                n_detected <- n
            } else {
                req(input$selected_col)
                var_name <- input$selected_col
                vec_plot <- na.omit(data_df()[[input$selected_col]])
                n_detected <- length(vec_plot)
                mediana <- median(vec_plot)
                sd_sample <- sd(vec_plot)
                n <- n_detected
            }

            se_med <- 1.2533 * (sd_sample / sqrt(n))
            z_val <- qnorm(1 - (1 - input$conf) / 2)
            li <- mediana - (z_val * se_med)
            ls <- mediana + (z_val * se_med)

            boot_available <- is.finite(n) && n > 1 && n < 30
            B <- 2000
            alpha <- 1 - input$conf
            boot_meds <- NULL
            boot_li <- NA_real_
            boot_ls <- NA_real_

            if (boot_available) {
                set.seed(123)
                boot_meds <- replicate(B, median(sample(vec_plot, size = n, replace = TRUE)))
                qs <- as.numeric(quantile(boot_meds, probs = c(alpha / 2, 1 - alpha / 2), names = FALSE))
                boot_li <- qs[1]
                boot_ls <- qs[2]
            }

            list(
                mediana = mediana, li = li, ls = ls, se = se_med, n_detected = n_detected,
                data_vec = vec_plot, conf = input$conf, var_name = var_name,
                boot_available = boot_available, boot_meds = boot_meds, boot_li = boot_li, boot_ls = boot_ls
            )
        })

        # Plot functions
        plot_dist <- function() {
            res <- results()
            x <- seq(res$mediana - 4 * res$se, res$mediana + 4 * res$se, length.out = 300)
            y <- dnorm(x, mean = res$mediana, sd = res$se)
            df <- data.frame(x, y)
            ggplot(df, aes(x, y)) +
                geom_area(data = subset(df, x >= res$li & x <= res$ls), fill = "#3498db", alpha = 0.4) +
                geom_line(color = "#2980b9", linewidth = 1) +
                geom_vline(xintercept = c(res$li, res$mediana, res$ls), color = "red", linetype = "dashed") +
                labs(title = paste(tr()$dist_title, res$var_name), x = tr()$x_med, y = tr()$y_den) +
                theme_minimal()
        }

        plot_box <- function() {
            res <- results()
            df_v <- data.frame(val = res$data_vec)
            ggplot(df_v, aes(y = .data$val, x = "")) +
                geom_boxplot(fill = "#ecf0f1", outlier.color = "red", width = 0.4) +
                geom_hline(yintercept = res$mediana, color = "red", linewidth = 1) +
                labs(title = paste(tr()$box_title, res$var_name), y = tr()$y_val, x = "") +
                theme_minimal()
        }

        plot_boot_dist <- function() {
            res <- results()
            req(res$boot_available)
            dfb <- data.frame(med = res$boot_meds)
            ggplot(dfb, aes(x = .data$med)) +
                geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#3498db", alpha = 0.35) +
                geom_density(linewidth = 1) +
                geom_vline(xintercept = c(res$boot_li, res$mediana, res$boot_ls), color = "red", linetype = "dashed") +
                labs(title = paste(tr()$boot_dist_title, res$var_name), x = tr()$x_med, y = tr()$y_den) +
                theme_minimal()
        }

        output$distPlot <- renderPlot({
            plot_dist()
        })
        output$boxPlot <- renderPlot({
            plot_box()
        })
        output$bootDistPlot <- renderPlot({
            plot_boot_dist()
        })

        output$main_content <- renderUI({
            req(results())
            res <- results()
            tagList(
                wellPanel(
                    h3(style = "text-align: center;", tr()$results_title),
                    p(style = "text-align: center;", strong(tr()$asym_ci), paste0("[", round(res$li, 4), " , ", round(res$ls, 4), "]")),
                    if (res$boot_available) {
                        tagList(
                            p(style = "text-align: center;", strong(tr()$boot_note_small)),
                            p(style = "text-align: center;", strong(tr()$boot_ci), paste0("[", round(res$boot_li, 4), " , ", round(res$boot_ls, 4), "]"))
                        )
                    } else {
                        p(style = "text-align: center;", em(tr()$boot_note_large))
                    },
                    p(style = "text-align: center;", strong(tr()$point_median), round(res$mediana, 4)),
                    p(style = "text-align: center;", strong(tr()$detected_n), res$n_detected)
                ),
                fluidRow(column(6, plotOutput(ns("distPlot"))), column(6, plotOutput(ns("boxPlot")))),
                if (res$boot_available) {
                    tagList(br(), fluidRow(column(6, plotOutput(ns("bootDistPlot")))))
                }
            )
        })

        observeEvent(input$help, {
            showModal(modalDialog(
                title = tr()$methodology_title,
                tags$div(
                    h4(tr()$formulas_used),
                    withMathJax(
                        p(paste0("1. ", tr()$se_lbl, " $$SE = 1.2533 \\times \\dfrac{s}{\\sqrt{n}}$$")),
                        p(paste0("2. ", tr()$me_lbl, " $$ME = Z_{\\alpha/2} \\times SE$$")),
                        p(paste0("3. ", tr()$ci_lbl, " $$IC = \\text{Mediana} \\pm ME$$"))
                    ),
                    hr(),
                    h4(tr()$bibliography),
                    tags$ul(
                        tags$li("Jamshidian, M., & Khatoonabadi, M. (2007). Using asymptotic results to obtain a confidence interval for the population median."),
                        tags$li("DiCiccio, T. J., & Efron, B. (1996). Bootstrap confidence intervals.")
                    )
                ),
                easyClose = TRUE
            ))
        })

        output$print_content <- renderUI({
            res <- results()
            tr_now <- tr()
            tagList(
                div(
                    class = "printable-section",
                    h3(tr_now$print_header),
                    h4(tr_now$print_results),
                    p(strong(tr_now$asym_ci), paste0("[", round(res$li, 4), ", ", round(res$ls, 4), "]")),
                    if (res$boot_available) p(strong(tr_now$boot_ci), paste0("[", round(res$boot_li, 4), ", ", round(res$boot_ls, 4), "]")),
                    p(strong(tr_now$point_median), round(res$mediana, 4)),
                    p(strong(tr_now$detected_n), res$n_detected)
                ),
                div(
                    class = "printable-section",
                    h4(tr_now$print_plots),
                    plotOutput(ns("distPlot_p")),
                    plotOutput(ns("boxPlot_p"))
                )
            )
        })

        # Extra renders for print to ensure they exist even if hidden
        output$distPlot_p <- renderPlot({
            plot_dist()
        })
        output$boxPlot_p <- renderPlot({
            plot_box()
        })
    })
}
