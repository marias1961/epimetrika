# apps/NNT_EC_PT/app.R

library(shiny)

# =========================
# Module UI
# =========================
nnt_ec_pt_UI <- function(id) {
    ns <- NS(id)
    tagList(
        tags$style(HTML(paste0("
      #", ns("main_container"), " .rowline { margin-bottom: 10px; }
      #", ns("main_container"), " .lbl { font-size: 16px; font-weight: 500; padding-top: 8px; }
      #", ns("main_container"), " .subhdr { font-size: 18px; font-style: italic; font-weight: 600; text-align: center; margin-top: 8px; }
      #", ns("main_container"), " .cihdr { font-size: 16px; font-weight: 700; text-align: center; margin-top: 8px; }
      #", ns("main_container"), " .outbox { background:#fff; font-weight: bold; border-color: #1558b0; color: #1558b0; }
      #", ns("main_container"), " .helpi {
        display:inline-block; margin-left:8px; font-weight:700; color:#1f77b4;
        border:2px solid #1f77b4; border-radius:999px; width:22px; height:22px;
        line-height:18px; text-align:center; cursor:help; font-size:14px;
      }
      #", ns("main_container"), " .formula {
        font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
        background:#f6f6f6; border:1px solid #e6e6e6; border-radius:8px;
        padding:8px 10px; display:block; margin:6px 0 0 0;
        white-space: normal;
      }
    "))),
        div(
            id = ns("main_container"),
            div(
                class = "container-fluid",
                uiOutput(ns("header_ui")),
                hr(),
                fluidRow(
                    column(6, uiOutput(ns("intro_ui"))),
                    column(
                        6,
                        uiOutput(ns("ci_header_ui")),
                        fluidRow(
                            column(6, uiOutput(ns("ci_low_ui"))),
                            column(6, uiOutput(ns("ci_high_ui")))
                        )
                    )
                ),
                hr(),

                # Inputs and Outputs Rows
                fluidRow(
                    class = "rowline",
                    column(3, uiOutput(ns("lbl_tec_ui"))),
                    column(3, numericInput(ns("TEC"), NULL, value = 10, min = 0, step = 1)),
                    column(3, ""), column(3, "")
                ),
                fluidRow(
                    class = "rowline",
                    column(3, uiOutput(ns("lbl_tee_ui"))),
                    column(3, numericInput(ns("TEE"), NULL, value = 5, min = 0, step = 1)),
                    column(3, ""), column(3, "")
                ),
                fluidRow(
                    class = "rowline",
                    column(3, uiOutput(ns("lbl_ptc_ui"))),
                    column(3, numericInput(ns("PTc"), NULL, value = 100, min = 0.0001, step = 1)),
                    column(3, ""), column(3, "")
                ),
                fluidRow(
                    class = "rowline",
                    column(3, uiOutput(ns("lbl_pte_ui"))),
                    column(3, numericInput(ns("PTe"), NULL, value = 100, min = 0.0001, step = 1)),
                    column(3, ""), column(3, "")
                ),
                fluidRow(
                    class = "rowline",
                    column(3, uiOutput(ns("lbl_irr_ui"))),
                    column(3, uiOutput(ns("out_irr"))),
                    column(3, ""), column(3, "")
                ),
                hr(),
                fluidRow(
                    class = "rowline",
                    column(3, uiOutput(ns("lbl_r0_ui"))),
                    column(3, uiOutput(ns("out_r0"))),
                    column(3, uiOutput(ns("ci_r0_l"))),
                    column(3, uiOutput(ns("ci_r0_u")))
                ),
                fluidRow(
                    class = "rowline",
                    column(3, uiOutput(ns("lbl_r1_ui"))),
                    column(3, uiOutput(ns("out_r1"))),
                    column(3, uiOutput(ns("ci_r1_l"))),
                    column(3, uiOutput(ns("ci_r1_u")))
                ),
                fluidRow(
                    class = "rowline",
                    column(3, uiOutput(ns("lbl_rar_ui"))),
                    column(3, uiOutput(ns("out_rar"))),
                    column(3, uiOutput(ns("ci_rar_l"))),
                    column(3, uiOutput(ns("ci_rar_u")))
                ),
                fluidRow(
                    class = "rowline",
                    column(3, uiOutput(ns("lbl_nnt_res_ui"))),
                    column(3, uiOutput(ns("out_nnt"))),
                    column(3, uiOutput(ns("ci_nnt_l"))),
                    column(3, uiOutput(ns("ci_nnt_u")))
                ),
                fluidRow(
                    class = "rowline",
                    column(3, uiOutput(ns("lbl_nrr_ui"))),
                    column(3, uiOutput(ns("out_nrr"))),
                    column(3, uiOutput(ns("ci_nrr_l"))),
                    column(3, uiOutput(ns("ci_nrr_u")))
                ),
                fluidRow(
                    class = "rowline",
                    column(3, uiOutput(ns("lbl_nnt_net_res_ui"))),
                    column(3, uiOutput(ns("out_nnt_net"))),
                    column(3, uiOutput(ns("ci_nnt_net_l"))),
                    column(3, uiOutput(ns("ci_nnt_net_u")))
                ),
                hr(),
                uiOutput(ns("method_ui")),

                # Print Template
                div(class = "print-template", style = "display:none;", uiOutput(ns("print_content")))
            )
        )
    )
}

# =========================
# Module Server
# =========================
nnt_ec_pt_Server <- function(id, lang) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        tr <- reactive({
            get_translations(lang(), "nnt_ec_pt")
        })

        # Helpers
        fmt_num <- function(x, digits = 3) {
            if (is.null(x) || length(x) == 0 || is.na(x)) {
                return("")
            }
            if (is.infinite(x)) {
                return("\u221e")
            }
            formatC(x, format = "f", digits = digits, decimal.mark = ",")
        }
        fmt_pct <- function(x, digits = 0) {
            if (is.null(x) || length(x) == 0 || is.na(x)) {
                return("")
            }
            if (is.infinite(x)) {
                return("\u221e")
            }
            paste0(formatC(100 * x, format = "f", digits = digits, decimal.mark = ","), "%")
        }
        fmt_int <- function(x) {
            if (is.null(x) || length(x) == 0 || is.na(x)) {
                return("")
            }
            if (is.infinite(x)) {
                return("\u221e")
            }
            as.character(as.integer(x))
        }
        out_box <- function(value) {
            tags$input(type = "text", class = "form-control outbox", value = value, readonly = NA)
        }

        # Rendering UI components with translations
        output$header_ui <- renderUI({
            tagList(
                div(style = "font-size: 24px; font-weight: 600; margin-bottom: 5px;", tr()$title),
                div(class = "muted", tr()$subtitle)
            )
        })
        output$intro_ui <- renderUI({
            div(class = "muted", tr()$intro)
        })
        output$ci_header_ui <- renderUI({
            div(class = "subhdr", tr()$ci_header)
        })
        output$ci_low_ui <- renderUI({
            div(class = "cihdr", tr()$ci_low)
        })
        output$ci_high_ui <- renderUI({
            div(class = "cihdr", tr()$ci_high)
        })

        output$lbl_tec_ui <- renderUI({
            div(
                class = "lbl", tr()$lbl_tec,
                tags$span(class = "helpi", `data-toggle` = "tooltip", title = tr()$tip_tec, "i")
            )
        })
        output$lbl_tee_ui <- renderUI({
            div(
                class = "lbl", tr()$lbl_tee,
                tags$span(class = "helpi", `data-toggle` = "tooltip", title = tr()$tip_tee, "i")
            )
        })
        output$lbl_ptc_ui <- renderUI({
            div(class = "lbl", tr()$lbl_ptc)
        })
        output$lbl_pte_ui <- renderUI({
            div(class = "lbl", tr()$lbl_pte)
        })
        output$lbl_irr_ui <- renderUI({
            div(class = "lbl", tr()$lbl_irr)
        })

        output$lbl_r0_ui <- renderUI({
            div(
                class = "lbl", tr()$lbl_r0,
                tags$span(class = "helpi", `data-toggle` = "tooltip", title = tr()$tip_r0, "i")
            )
        })
        output$lbl_r1_ui <- renderUI({
            div(
                class = "lbl", tr()$lbl_r1,
                tags$span(class = "helpi", `data-toggle` = "tooltip", title = tr()$tip_r1, "i")
            )
        })
        output$lbl_rar_ui <- renderUI({
            div(class = "lbl", tr()$lbl_rar)
        })
        output$lbl_nrr_ui <- renderUI({
            div(
                class = "lbl", tr()$lbl_nrr,
                tags$span(class = "helpi", `data-toggle` = "tooltip", title = tr()$tip_nrr, "i")
            )
        })

        # Calculation
        calc <- reactive({
            TEC <- suppressWarnings(as.numeric(input$TEC))
            TEE <- suppressWarnings(as.numeric(input$TEE))
            PTc <- suppressWarnings(as.numeric(input$PTc))
            PTe <- suppressWarnings(as.numeric(input$PTe))

            if (is.na(TEC) || is.na(TEE) || is.na(PTc) || is.na(PTe) || PTc <= 0 || PTe <= 0) {
                return(list(ok = FALSE))
            }

            IR0 <- TEC / PTc
            IR1 <- TEE / PTe
            IRR <- if (!is.finite(IR0) || IR0 <= 0) NA_real_ else IR1 / IR0

            R0 <- 1 - exp(-IR0)
            R1 <- 1 - exp(-IR1)
            RAR <- R0 - R1

            ci_IR0 <- tryCatch(poisson.test(TEC, T = PTc, conf.level = 0.95)$conf.int, error = function(e) c(NA, NA))
            ci_IR1 <- tryCatch(poisson.test(TEE, T = PTe, conf.level = 0.95)$conf.int, error = function(e) c(NA, NA))

            ci_R0 <- 1 - exp(-ci_IR0)
            ci_R1 <- 1 - exp(-ci_IR1)

            var_IR0 <- TEC / (PTc^2)
            var_IR1 <- TEE / (PTe^2)
            dR0_dIR0 <- exp(-IR0)
            dR1_dIR1 <- exp(-IR1)
            var_R0 <- (dR0_dIR0^2) * var_IR0
            var_R1 <- (dR1_dIR1^2) * var_IR1

            se_RAR <- sqrt(var_R0 + var_R1)
            ci_RAR <- c(RAR - 1.96 * se_RAR, RAR + 1.96 * se_RAR)

            eps <- 1e-12
            is_benefit <- is.finite(RAR) && (RAR > eps)
            is_harm <- is.finite(RAR) && (RAR < -eps)

            nnt_label <- if (is_harm) tr()$lbl_nnd else tr()$lbl_nnt
            nnt_est <- if (!is.finite(RAR) || abs(RAR) <= eps) Inf else ceiling(1 / abs(RAR))

            nnt_ci <- c(Inf, Inf)
            if (is_benefit) {
                rdL <- ci_RAR[1]
                rdU <- ci_RAR[2]
                nnt_ci[1] <- if (rdU > eps) ceiling(1 / rdU) else Inf
                nnt_ci[2] <- if (rdL > eps) ceiling(1 / rdL) else Inf
            } else if (is_harm) {
                ariL <- -ci_RAR[2]
                ariU <- -ci_RAR[1]
                nnt_ci[1] <- if (ariU > eps) ceiling(1 / ariU) else Inf
                nnt_ci[2] <- if (ariL > eps) ceiling(1 / ariL) else Inf
            }

            nrr_est <- if (!is.finite(RAR) || abs(RAR) <= eps) Inf else ceiling(R1 / abs(RAR))
            nrr_ci <- c(Inf, Inf)
            if (is_benefit) {
                rdL <- ci_RAR[1]
                rdU <- ci_RAR[2]
                nrr_ci[1] <- if (rdU > eps) ceiling(R1 / rdU) else Inf
                nrr_ci[2] <- if (rdL > eps) ceiling(R1 / rdL) else Inf
            } else if (is_harm) {
                ariL <- -ci_RAR[2]
                ariU <- -ci_RAR[1]
                nrr_ci[1] <- if (ariU > eps) ceiling(R1 / ariU) else Inf
                nrr_ci[2] <- if (ariL > eps) ceiling(R1 / ariL) else Inf
            }

            nnt_net_label <- if (is_harm) tr()$lbl_nnd_net else tr()$lbl_nnt_net
            nnt_net_est <- if (!is.finite(RAR) || abs(RAR) <= eps) Inf else ceiling(1 / (2 * abs(RAR)))
            nnt_net_ci <- c(Inf, Inf)
            if (is_benefit) {
                rdL <- ci_RAR[1]
                rdU <- ci_RAR[2]
                nnt_net_ci[1] <- if (rdU > eps) ceiling(1 / (2 * rdU)) else Inf
                nnt_net_ci[2] <- if (rdL > eps) ceiling(1 / (2 * rdL)) else Inf
            } else if (is_harm) {
                ariL <- -ci_RAR[2]
                ariU <- -ci_RAR[1]
                nnt_net_ci[1] <- if (ariU > eps) ceiling(1 / (2 * ariU)) else Inf
                nnt_net_ci[2] <- if (ariL > eps) ceiling(1 / (2 * ariL)) else Inf
            }

            list(
                ok = TRUE, IRR = IRR, R0 = R0, R1 = R1, RAR = RAR,
                ci_R0 = ci_R0, ci_R1 = ci_R1, ci_RAR = ci_RAR,
                nnt_label = nnt_label, nnt_est = nnt_est, nnt_ci = nnt_ci,
                nrr_est = nrr_est, nrr_ci = nrr_ci,
                nnt_net_label = nnt_net_label, nnt_net_est = nnt_net_est, nnt_net_ci = nnt_net_ci
            )
        })

        # Results rendering
        output$lbl_nnt_res_ui <- renderUI({
            div(class = "lbl", if (isTRUE(calc()$ok)) calc()$nnt_label else tr()$lbl_nnt)
        })
        output$lbl_nnt_net_res_ui <- renderUI({
            div(class = "lbl", if (isTRUE(calc()$ok)) calc()$nnt_net_label else tr()$lbl_nnt_net)
        })

        output$out_irr <- renderUI({
            res <- calc()
            if (!res$ok) {
                return(out_box(""))
            }
            out_box(fmt_pct(res$IRR, 0))
        })
        output$out_r0 <- renderUI({
            res <- calc()
            if (!res$ok) {
                return(out_box(""))
            }
            out_box(fmt_num(res$R0, 5))
        })
        output$out_r1 <- renderUI({
            res <- calc()
            if (!res$ok) {
                return(out_box(""))
            }
            out_box(fmt_num(res$R1, 5))
        })
        output$out_rar <- renderUI({
            res <- calc()
            if (!res$ok) {
                return(out_box(""))
            }
            out_box(fmt_num(res$RAR, 5))
        })
        output$out_nnt <- renderUI({
            res <- calc()
            if (!res$ok) {
                return(out_box(""))
            }
            out_box(fmt_int(res$nnt_est))
        })
        output$out_nrr <- renderUI({
            res <- calc()
            if (!res$ok) {
                return(out_box(""))
            }
            out_box(fmt_int(res$nrr_est))
        })
        output$out_nnt_net <- renderUI({
            res <- calc()
            if (!res$ok) {
                return(out_box(""))
            }
            out_box(fmt_int(res$nnt_net_est))
        })

        output$ci_r0_l <- renderUI({
            res <- calc()
            if (!res$ok) {
                return(out_box(""))
            }
            out_box(fmt_num(res$ci_R0[1], 5))
        })
        output$ci_r0_u <- renderUI({
            res <- calc()
            if (!res$ok) {
                return(out_box(""))
            }
            out_box(fmt_num(res$ci_R0[2], 5))
        })
        output$ci_r1_l <- renderUI({
            res <- calc()
            if (!res$ok) {
                return(out_box(""))
            }
            out_box(fmt_num(res$ci_R1[1], 5))
        })
        output$ci_r1_u <- renderUI({
            res <- calc()
            if (!res$ok) {
                return(out_box(""))
            }
            out_box(fmt_num(res$ci_R1[2], 5))
        })
        output$ci_rar_l <- renderUI({
            res <- calc()
            if (!res$ok) {
                return(out_box(""))
            }
            out_box(fmt_num(res$ci_RAR[1], 5))
        })
        output$ci_rar_u <- renderUI({
            res <- calc()
            if (!res$ok) {
                return(out_box(""))
            }
            out_box(fmt_num(res$ci_RAR[2], 5))
        })

        output$ci_nnt_l <- renderUI({
            res <- calc()
            if (!res$ok) {
                return(out_box(""))
            }
            out_box(fmt_int(res$nnt_ci[1]))
        })
        output$ci_nnt_u <- renderUI({
            res <- calc()
            if (!res$ok) {
                return(out_box(""))
            }
            out_box(fmt_int(res$nnt_ci[2]))
        })
        output$ci_nrr_l <- renderUI({
            res <- calc()
            if (!res$ok) {
                return(out_box(""))
            }
            out_box(fmt_int(res$nrr_ci[1]))
        })
        output$ci_nrr_u <- renderUI({
            res <- calc()
            if (!res$ok) {
                return(out_box(""))
            }
            out_box(fmt_int(res$nrr_ci[2]))
        })
        output$ci_nnt_net_l <- renderUI({
            res <- calc()
            if (!res$ok) {
                return(out_box(""))
            }
            out_box(fmt_int(res$nnt_net_ci[1]))
        })
        output$ci_nnt_net_u <- renderUI({
            res <- calc()
            if (!res$ok) {
                return(out_box(""))
            }
            out_box(fmt_int(res$nnt_net_ci[2]))
        })

        output$method_ui <- renderUI({
            tagList(
                div(
                    class = "note",
                    tags$b(tr()$method_note), " ", tr()$method_text, " ", tags$em(tr()$method_formula),
                    br(), tags$b(tr()$ic_note),
                    tags$ul(
                        tags$li(tags$b(tr()$ic_low, "/", tr()$ic_high), " : ", tr()$ic_list$poisson),
                        tags$li(tags$b(tr()$lbl_r0, " y ", tr()$lbl_r1), " : ", tr()$ic_list$transform),
                        tags$li(tags$b(tr()$ic_list$rar), div(class = "formula", tr()$ic_list$rar_se)),
                        tags$li(tags$b(tr()$lbl_nnt, tr()$lbl_nnd), " : ", tr()$ic_list$nnt)
                    ),
                    tr()$reference
                )
            )
        })

        # Print content
        output$print_content <- renderUI({
            res <- calc()
            if (!res$ok) {
                return(NULL)
            }
            t <- tr()
            div(
                class = "printable-section",
                h3(t$print_header),
                hr(),
                p(strong(t$lbl_rar), ": ", fmt_num(res$RAR, 5), " [", fmt_num(res$ci_RAR[1], 5), ", ", fmt_num(res$ci_RAR[2], 5), "]"),
                p(strong(res$nnt_label), ": ", fmt_int(res$nnt_est), " [", fmt_int(res$nnt_ci[1]), ", ", fmt_int(res$nnt_ci[2]), "]"),
                p(strong(t$lbl_nrr), ": ", fmt_int(res$nrr_est), " [", fmt_int(res$nrr_ci[1]), ", ", fmt_int(res$nrr_ci[2]), "]"),
                p(strong(res$nnt_net_label), ": ", fmt_int(res$nnt_net_est), " [", fmt_int(res$nnt_net_ci[1]), ", ", fmt_int(res$nnt_net_ci[2]), "]"),
                br(),
                p(style = "font-size: 0.8em; color: gray;", t$reference)
            )
        })
    })
}
