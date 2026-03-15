# apps/CCI_multiples_obs/app.R

library(shiny)
library(bslib)

# =========================
# Module UI
# =========================
cci_multi_UI <- function(id) {
    ns <- NS(id)
    tagList(
        tags$style(HTML(paste0("
      #", ns("main_container"), " .lbl { font-size: 15px; padding-top: 9px; }
      #", ns("main_container"), " .hdr { font-size: 20px; font-weight: 700; margin: 8px 0 12px; }
      #", ns("main_container"), " .small-note { font-size: 13px; color: #666; margin-top: 10px; }
      #", ns("main_container"), " .note-text mjx-container[jax='SVG'] { font-size: 110% !important; }
      #", ns("main_container"), " .card-header { font-weight: bold; }

      /* Force square aspect ratio and adaptive font */
      #", ns("main_container"), " .cci-vbox {
        aspect-ratio: 1 / 1;
        container-type: inline-size;
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        padding: 10px !important;
        overflow: hidden;
      }
      #", ns("main_container"), " .cci-vbox .value-box-value {
        font-size: clamp(1rem, 15cqw, 3rem) !important;
        margin: 0 !important;
        text-align: center;
      }
      #", ns("main_container"), " .cci-vbox .value-box-title {
        font-size: clamp(0.7rem, 8cqw, 1.2rem) !important;
        margin: 0 !important;
        text-align: center;
      }
      #", ns("main_container"), " .cci-vbox .value-box-showcase {
        font-size: clamp(1rem, 20cqw, 4rem) !important;
        opacity: 0.2;
      }
    "))),
        div(
            id = ns("main_container"),
            layout_sidebar(
                sidebar = sidebar(
                    width = 350,
                    title = uiOutput(ns("tab_in_lbl")),
                    uiOutput(ns("box_data_lbl")),
                    textAreaInput(ns("txt"), NULL, rows = 12),
                    layout_columns(
                        actionButton(ns("use_example"), uiOutput(ns("btn_example_lbl")), icon = icon("magic"), class = "btn-sm"),
                        actionButton(ns("clear"), uiOutput(ns("btn_clear_lbl")), icon = icon("eraser"), class = "btn-sm")
                    ),
                    br(),
                    uiOutput(ns("conf_ui")),
                    tags$hr(),
                    uiOutput(ns("pair_select_ui")),
                    uiOutput(ns("icc_note_ui"))
                ),
                navset_card_underline(
                    id = ns("main_tabs"),
                    nav_panel(
                        title = uiOutput(ns("tab_out_lbl")),
                        uiOutput(ns("results_ui"))
                    ),
                    nav_panel(
                        title = uiOutput(ns("tab_notes_lbl")),
                        uiOutput(ns("notes_ui"))
                    )
                ),
                # Hidden print template
                div(class = "print-template", style = "display:none;", uiOutput(ns("print_content")))
            )
        )
    )
}

# =========================
# Module Server
# =========================
cci_multi_Server <- function(id, lang) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Helpers and Logic
        default_data_text <- paste(
            "Obs1\tObs2\tObs3", "52\t58\t57", "53\t55\t54", "59\t56\t58",
            "60\t54\t56", "59\t59\t60", "59\t60\t61", "57\t59\t58",
            "53\t58\t56", "54\t52\t53",
            sep = "\n"
        )

        parse_table_multi <- function(txt) {
            txt <- gsub("\r", "", txt)
            txt <- trimws(txt)
            if (nchar(txt) == 0) {
                return(NULL)
            }
            df <- tryCatch(read.table(text = txt, header = TRUE, sep = "", stringsAsFactors = FALSE, check.names = TRUE), error = function(e) NULL)
            if (is.null(df) || ncol(df) < 2) df <- tryCatch(read.table(text = txt, header = TRUE, sep = ";", stringsAsFactors = FALSE, check.names = TRUE), error = function(e) NULL)
            if (is.null(df) || ncol(df) < 2) {
                df <- tryCatch(read.table(text = txt, header = FALSE, sep = "", stringsAsFactors = FALSE), error = function(e) NULL)
                if (!is.null(df) && ncol(df) >= 2) names(df) <- paste0("Obs", seq_len(ncol(df)))
            }
            if (is.null(df) || ncol(df) < 2) {
                return(NULL)
            }
            to_num <- function(x) {
                x <- trimws(as.character(x))
                x <- gsub(",", ".", x, fixed = TRUE)
                suppressWarnings(as.numeric(x))
            }
            for (j in seq_len(ncol(df))) df[[j]] <- to_num(df[[j]])
            if (nrow(df) < 2) {
                return(NULL)
            }
            df
        }

        anova_ms_two_way_multi <- function(df_wide_complete) {
            n <- nrow(df_wide_complete)
            k <- ncol(df_wide_complete)
            long <- data.frame(
                sujeto = factor(rep(seq_len(n), times = k)),
                rater = factor(rep(colnames(df_wide_complete), each = n)),
                valor = as.vector(as.matrix(df_wide_complete))
            )
            fit <- aov(valor ~ sujeto + rater, data = long)
            a <- anova(fit)
            list(n = n, k = k, anova = a, BMS = a["sujeto", "Mean Sq"], JMS = a["rater", "Mean Sq"], EMS = a["Residuals", "Mean Sq"])
        }

        icc_A1_point <- function(BMS, JMS, EMS, n, k) {
            (BMS - EMS) / (BMS + (k - 1) * EMS + (k * (JMS - EMS) / n))
        }
        icc_Ak_point <- function(BMS, JMS, EMS, n) {
            (BMS - EMS) / (BMS + ((JMS - EMS) / n))
        }

        ba_stats_pair <- function(x, y) {
            d <- x - y
            m <- (x + y) / 2
            md <- mean(d)
            sd_d <- stats::sd(d)
            list(mean = m, diff = d, md = md, sd_d = sd_d, loa_low = md - 1.96 * sd_d, loa_high = md + 1.96 * sd_d)
        }

        paired_bias_t <- function(d, conf = 0.95) {
            n <- length(d)
            md <- mean(d)
            sd_d <- stats::sd(d)
            se <- sd_d / sqrt(n)
            df <- n - 1
            tstat <- md / se
            pval <- 2 * stats::pt(-abs(tstat), df = df)
            alpha <- 1 - conf
            tcrit <- stats::qt(1 - alpha / 2, df = df)
            ci <- md + c(-1, 1) * tcrit * se
            list(n = n, md = md, sd = sd_d, se = se, t = tstat, df = df, p = pval, ci_low = ci[1], ci_high = ci[2])
        }

        bradley_blackwood <- function(x, y) {
            n <- length(x)
            d <- x - y
            s <- x + y
            fit <- lm(d ~ s)
            X <- model.matrix(fit)
            b <- coef(fit)
            SSE <- sum(resid(fit)^2)
            df2 <- n - 2
            MSE <- SSE / df2
            p <- length(b)
            XtX <- crossprod(X)
            Fstat <- as.numeric(t(b) %*% XtX %*% b) / (p * MSE)
            pval <- 1 - pf(Fstat, df1 = p, df2 = df2)
            list(n = n, F = Fstat, df1 = p, df2 = df2, p = pval)
        }

        pairwise_table <- function(df_wide, conf = 0.95) {
            cols <- colnames(df_wide)
            pairs <- utils::combn(cols, 2, simplify = FALSE)
            alpha <- 1 - conf
            rows <- lapply(pairs, function(p) {
                a <- p[1]
                b <- p[2]
                x <- df_wide[[a]]
                y <- df_wide[[b]]
                keep <- stats::complete.cases(x, y)
                x <- x[keep]
                y <- y[keep]
                if (length(x) < 2) {
                    return(NULL)
                }
                ba <- ba_stats_pair(x, y)
                bt <- paired_bias_t(x - y, conf = conf)
                bb <- if (length(x) >= 3) bradley_blackwood(x, y) else list(F = NA_real_, p = NA_real_)
                data.frame(A = a, B = b, n = length(x), bias_mean = bt$md, bias_ci_low = bt$ci_low, bias_ci_high = bt$ci_high, bias_p = bt$p, loa_low = ba$loa_low, loa_high = ba$loa_high, bb_F = bb$F, bb_p = bb$p, stringsAsFactors = FALSE)
            })
            out <- do.call(rbind, rows)
            if (is.null(out) || nrow(out) == 0) {
                return(data.frame())
            }
            out$bias_p_bonf <- p.adjust(out$bias_p, method = "bonferroni")
            out$bb_p_bonf <- p.adjust(out$bb_p, method = "bonferroni")
            out$bias_sig_bonf <- out$bias_p_bonf < alpha
            out$bb_sig_bonf <- out$bb_p_bonf < alpha
            out
        }

        fmtp <- function(p) format.pval(p, digits = 4, eps = 1e-4)

        # Reactives
        vals <- reactiveValues(
            txt = default_data_text,
            conf = 0.95,
            obsA = NULL,
            obsB = NULL
        )

        tr <- reactive({
            get_translations(lang(), "cci_multi")
        })

        # Static labels
        output$tab_in_lbl <- renderUI({
            tr()$tab_in
        })
        output$tab_out_lbl <- renderUI({
            tr()$tab_out
        })
        output$tab_notes_lbl <- renderUI({
            tr()$tab_notes
        })
        output$box_data_lbl <- renderUI({
            labels <- tr()$box_data
            labels
        })
        output$btn_example_lbl <- renderUI({
            tr()$btn_example
        })
        output$btn_clear_lbl <- renderUI({
            tr()$btn_clear
        })
        output$btn_calc_lbl <- renderUI({
            tr()$btn_calc
        })
        output$icc_note_ui <- renderUI({
            tags$div(class = "small-note", tr()$icc_note)
        })

        # Inputs
        observeEvent(input$use_example, {
            updateTextAreaInput(session, "txt", value = default_data_text)
        })
        observeEvent(input$clear, {
            updateTextAreaInput(session, "txt", value = "")
        })

        # State persistence
        observe({
            if (!is.null(input$txt)) vals$txt <- input$txt
            if (!is.null(input$conf)) vals$conf <- input$conf
            if (!is.null(input$obsA)) vals$obsA <- input$obsA
            if (!is.null(input$obsB)) vals$obsB <- input$obsB
        })

        output$conf_ui <- renderUI({
            sliderInput(ns("conf"), tr()$lbl_conf, min = 0.90, max = 0.99, value = vals$conf, step = 0.01)
        })

        data_rv_raw <- reactive({
            parse_table_multi(input$txt)
        })

        # Debounce to prevent heavy calculations on every keystroke
        data_rv <- debounce(data_rv_raw, 500)

        df_ok <- reactive({
            df <- data_rv()
            shiny::validate(shiny::need(!is.null(df), tr()$err_load))
            shiny::validate(shiny::need(ncol(df) >= 2, tr()$err_load))
            shiny::validate(shiny::need(nrow(df) >= 2, tr()$err_load))
            df
        })

        output$pair_select_ui <- renderUI({
            df <- df_ok()
            cols <- colnames(df)
            selA <- if (!is.null(vals$obsA) && vals$obsA %in% cols) vals$obsA else cols[1]
            selB <- if (!is.null(vals$obsB) && vals$obsB %in% cols) vals$obsB else cols[min(2, length(cols))]
            tagList(
                selectInput(ns("obsA"), tr()$lbl_obsA, choices = cols, selected = selA),
                selectInput(ns("obsB"), tr()$lbl_obsB, choices = cols, selected = selB)
            )
        })

        pair_data <- reactive({
            df <- df_ok()
            req(input$obsA, input$obsB)
            shiny::validate(shiny::need(input$obsA != input$obsB, tr()$err_pair))
            x0 <- df[[input$obsA]]
            y0 <- df[[input$obsB]]
            keep <- stats::complete.cases(x0, y0)
            x <- x0[keep]
            y <- y0[keep]
            shiny::validate(shiny::need(length(x) >= 2, tr()$err_pair_n))
            list(x = x, y = y, n = length(x))
        })

        df_icc <- reactive({
            df <- df_ok()
            keep <- stats::complete.cases(df)
            dfi <- df[keep, , drop = FALSE]
            shiny::validate(shiny::need(nrow(dfi) >= 2, tr()$err_icc_n))
            dfi
        })

        # Stats reactives
        ms_ok <- reactive({
            anova_ms_two_way_multi(df_icc())
        })
        ba_ok <- reactive({
            pd <- pair_data()
            ba_stats_pair(pd$x, pd$y)
        })
        bias_ok <- reactive({
            pd <- pair_data()
            paired_bias_t(pd$x - pd$y, conf = input$conf)
        })
        bb_ok <- reactive({
            pd <- pair_data()
            shiny::validate(shiny::need(pd$n >= 3, tr()$err_bb_n))
            bradley_blackwood(pd$x, pd$y)
        })
        pairs_ok <- reactive({
            pairwise_table(df_ok(), conf = input$conf)
        })

        # Main UI Output
        output$results_ui <- renderUI({
            # Catch errors in reactive values to prevent total page failure
            ms <- tryCatch(ms_ok(), error = function(e) NULL)
            ba <- tryCatch(ba_ok(), error = function(e) NULL)
            bt <- tryCatch(bias_ok(), error = function(e) NULL)
            bb <- tryCatch(bb_ok(), error = function(e) NULL)

            if (is.null(ms) || is.null(ba) || is.null(bt)) {
                return(div(class = "alert alert-warning", tr()$wait_data))
            }

            tagList(
                layout_columns(
                    col_widths = c(3, 3, 3, 3),
                    value_box(
                        title = "ICC(A,1)",
                        value = sprintf("%.3f", icc_A1_point(ms$BMS, ms$JMS, ms$EMS, ms$n, ms$k)),
                        showcase = icon("user"),
                        theme = "primary",
                        class = "cci-vbox"
                    ),
                    value_box(
                        title = "ICC(A,k)",
                        value = sprintf("%.3f", icc_Ak_point(ms$BMS, ms$JMS, ms$EMS, ms$n)),
                        showcase = icon("users"),
                        theme = "primary",
                        class = "cci-vbox"
                    ),
                    value_box(
                        title = tr()$row_bias,
                        value = sprintf("%.3f", bt$md),
                        showcase = icon("arrows-left-right"),
                        theme = "success",
                        class = "cci-vbox"
                    ),
                    value_box(
                        title = "LOA 95%",
                        value = paste0(sprintf("%.2f", ba$loa_low), " a ", sprintf("%.2f", ba$loa_high)),
                        showcase = icon("grip-lines-vertical"),
                        theme = "warning",
                        class = "cci-vbox"
                    )
                ),
                layout_columns(
                    col_widths = c(7, 5),
                    card(
                        card_header(tr()$h_ba),
                        plotOutput(ns("ba_plot"), height = 360),
                        uiOutput(ns("ba_stats_ui"))
                    ),
                    card(
                        card_header(tr()$h_bias),
                        uiOutput(ns("bias_t_ui"))
                    )
                ),
                layout_columns(
                    col_widths = c(6, 6),
                    card(
                        card_header(tr()$h_icc),
                        uiOutput(ns("icc_info_ui"))
                    ),
                    card(
                        card_header(tr()$h_bb),
                        uiOutput(ns("bb_ui"))
                    )
                ),
                card(
                    card_header(tr()$h_pairs),
                    tableOutput(ns("pairs_tbl")),
                    tags$div(class = "small-note", tr()$pairs_note)
                ),
                card(
                    card_header(tr()$h_anova),
                    tableOutput(ns("anova_tbl"))
                ),
                card(
                    card_header(tr()$h_data),
                    tableOutput(ns("data_tbl"))
                )
            )
        })

        output$ba_plot <- renderPlot({
            ba <- ba_ok()
            pd <- pair_data()
            plot(ba$mean, ba$diff, xlab = tr()$col_val, ylab = paste0(input$obsA, " - ", input$obsB), main = paste0(input$obsA, " vs ", input$obsB), pch = 16)
            abline(h = ba$md, lwd = 2)
            abline(h = ba$loa_low, lty = 2, lwd = 2)
            abline(h = ba$loa_high, lty = 2, lwd = 2)
            grid()
            legend("topright", legend = c("Bias", "LOA low", "LOA high"), lty = c(1, 2, 2), lwd = 2, bty = "n")
        })

        output$ba_stats_ui <- renderUI({
            ba <- ba_ok()
            tags$div(tags$b(tr()$row_n_pair, ": "), pair_data()$n, tags$br(), tags$b(tr()$row_bias, ": "), sprintf("%.6f", ba$md), tags$br(), tags$b(tr()$row_sd_d, ": "), sprintf("%.6f", ba$sd_d), tags$br(), tags$b(tr()$row_loa, ": "), sprintf("[%.6f, %.6f]", ba$loa_low, ba$loa_high))
        })

        output$bias_t_ui <- renderUI({
            bt <- bias_ok()
            alpha <- 1 - input$conf
            tagList(tags$b("n: "), bt$n, tags$br(), tags$b(tr()$row_bias, ": "), sprintf("%.6f", bt$md), tags$br(), tags$b(tr()$row_sd_d, ": "), sprintf("%.6f", bt$sd), tags$br(), tags$b("t: "), sprintf("%.4f", bt$t), tags$br(), tags$b(tr()$lbl_pval), fmtp(bt$p), tags$br(), tags$b("IC: "), sprintf("[%.6f, %.6f]", bt$ci_low, bt$ci_high), tags$br(), br(), if (bt$p < alpha) tags$span(tr()$lbl_bias_sugar, style = "color:red; font-weight:bold;") else tags$span(tr()$lbl_bias_no_sugar))
        })

        output$bb_ui <- renderUI({
            bb <- bb_ok()
            tagList(tags$b("F: "), sprintf("%.4f", bb$F), tags$br(), tags$b(tr()$col_gl, " 1: "), bb$df1, tags$br(), tags$b(tr()$col_gl, " 2: "), bb$df2, tags$br(), tags$b(tr()$lbl_pval), fmtp(bb$p))
        })

        output$icc_info_ui <- renderUI({
            ms <- ms_ok()
            icc1 <- icc_A1_point(ms$BMS, ms$JMS, ms$EMS, ms$n, ms$k)
            icck <- icc_Ak_point(ms$BMS, ms$JMS, ms$EMS, ms$n)
            tagList(tags$b(tr()$lbl_n_icc), ms$n, tags$br(), tags$b(tr()$lbl_k_obs), ms$k, tags$br(), tags$hr(), tags$b("ICC(A,1): "), sprintf("%.6f", icc1), tags$br(), tags$b("ICC(A,k): "), sprintf("%.6f", icck))
        })

        output$anova_tbl <- renderTable(
            {
                a <- ms_ok()$anova
                out <- data.frame(Fuente = rownames(a), gl = as.integer(a[, "Df"]), SC = a[, "Sum Sq"], CM = a[, "Mean Sq"])
                colnames(out) <- c(tr()$col_fuente, tr()$col_gl, tr()$col_sc, tr()$col_cm)
                out
            },
            striped = TRUE
        )

        output$pairs_tbl <- renderTable(
            {
                tt <- pairs_ok()
                if (nrow(tt) == 0) {
                    return(NULL)
                }
                out <- data.frame(A = tt$A, B = tt$B, n = tt$n, bias = round(tt$bias_mean, 4), p_bonf = signif(tt$bias_p_bonf, 4), LOA = paste0("[", round(tt$loa_low, 2), ", ", round(tt$loa_high, 2), "]"), BB_p_bonf = signif(tt$bb_p_bonf, 4))
                out
            },
            striped = TRUE
        )

        output$data_tbl <- renderTable(
            {
                df_ok()
            },
            striped = TRUE
        )

        output$notes_ui <- renderUI({
            tagList(
                card(card_header(tr()$ba_desc), tags$div(class = "note-text", HTML("\\[ d_i = X_{Ai}-X_{Bi}, \\quad m_i = \\frac{X_{Ai}+X_{Bi}}{2} \\] \\[ LOA_{95\\%} = \\bar d \\pm 1.96\\, s_d \\]"))),
                card(card_header(tr()$icc_desc), tags$div(class = "note-text", HTML("\\[ ICC(A,1)=\\frac{BMS-EMS}{BMS+(k-1)EMS+\\frac{k}{n}(JMS-EMS)} \\]"))),
                card(card_header(tr()$bb_desc), tags$div(class = "note-text", HTML("\\[ d_i = \\beta_0 + \\beta_1 s_i + \\varepsilon_i, \\quad H_0: \\beta_0=\\beta_1=0 \\]")))
            )
        })

        # Print support
        output$print_content <- renderUI({
            tagList(
                h3(tr()$title),
                h4(tr()$h_ba), plotOutput(ns("ba_plot_print")), uiOutput(ns("ba_stats_ui")),
                h4(tr()$h_icc), uiOutput(ns("icc_info_ui")),
                h4(tr()$h_bias), uiOutput(ns("bias_t_ui")),
                h4(tr()$h_bb), uiOutput(ns("bb_ui")),
                h4(tr()$h_pairs), tableOutput(ns("pairs_tbl")),
                h4(tr()$h_anova), tableOutput(ns("anova_tbl"))
            )
        })
        output$ba_plot_print <- renderPlot({
            ba <- ba_ok()
            pd <- pair_data()
            plot(ba$mean, ba$diff, xlab = tr()$col_val, ylab = paste0(input$obsA, " - ", input$obsB), pch = 16)
            abline(h = ba$md, lwd = 2)
            abline(h = ba$loa_low, lty = 2, lwd = 2)
            abline(h = ba$loa_high, lty = 2, lwd = 2)
            grid()
        })
    })
}
