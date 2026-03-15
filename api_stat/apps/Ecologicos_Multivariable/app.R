library(shiny)

# ---------- Module UI ----------
ecologicos_multivariable_UI <- function(id) {
    ns <- NS(id)
    tagList(
        sidebarLayout(
            sidebarPanel(
                h4(id = ns("h_settings"), "Configuración"),
                tabsetPanel(
                    id = ns("model_tabs"),
                    tabPanel(
                        title = span(id = ns("tab_params_lin"), "Personalizado (Lineal)"),
                        value = "linear",
                        br(),
                        numericInput(ns("b0_lin"), "b0 (Intercepto)", value = 4),
                        numericInput(ns("se_b0_lin"), "SE(b0)", value = NA, min = 0),
                        numericInput(ns("b1_lin"), "b1 (DT ajustada)", value = 2),
                        numericInput(ns("se_b1_lin"), "SE(b1)", value = NA, min = 0),
                        numericInput(ns("b2_lin"), "b2 (Efecto de Z)", value = 1),
                        numericInput(ns("se_b2_lin"), "SE(b2)", value = NA, min = 0),
                        numericInput(ns("z_lin"), "Valor de Z (Z=z)", value = 1),
                        sliderInput(ns("p_lin"), "Prevalencia exposición p = P(X=1)",
                            min = 0, max = 1, value = 0.17, step = 0.01
                        ),
                        numericInput(ns("zbar_lin"), "Z̄ (valor medio poblacional)", value = 1),
                        actionButton(ns("btn_example_lin"), "Cargar ejemplo con SE")
                    ),
                    tabPanel(
                        title = span(id = ns("tab_params_log"), "Personalizado (Loglineal)"),
                        value = "loglinear",
                        br(),
                        numericInput(ns("b0_log"), "b0 (Intercepto)", value = 2.2),
                        numericInput(ns("se_b0_log"), "SE(b0)", value = NA, min = 0),
                        numericInput(ns("b1_log"), "b1 (log-IR ajustada)", value = 0.4),
                        numericInput(ns("se_b1_log"), "SE(b1)", value = NA, min = 0),
                        numericInput(ns("b2_log"), "b2 (Efecto de Z)", value = 0.2),
                        numericInput(ns("se_b2_log"), "SE(b2)", value = NA, min = 0),
                        numericInput(ns("z_log"), "Valor de Z (Z=z)", value = 1),
                        sliderInput(ns("p_log"), "Prevalencia exposición p = P(X=1)",
                            min = 0, max = 1, value = 0.19, step = 0.01
                        ),
                        numericInput(ns("zbar_log"), "Z̄ (valor medio poblacional)", value = 1),
                        actionButton(ns("btn_example_log"), "Cargar ejemplo con SE")
                    )
                ),
                hr(),
                helpText(id = ns("help_se"), "Nota: Debes introducir el Error Estándar (SE) para calcular los Intervalos de Confianza.")
            ),
            mainPanel(
                tabsetPanel(
                    tabPanel(
                        title = span(id = ns("tab_res"), "Resultados"),
                        br(),
                        conditionalPanel(
                            condition = sprintf(
                                "input['%s'] == 'linear' || input['%s'] == null",
                                ns("model_tabs"), ns("model_tabs")
                            ),
                            wellPanel(
                                h4(id = ns("title_lin_h4"), "Resultados: Modelo lineal (aditivo)"),
                                tableOutput(ns("tab_lin"))
                            )
                        ),
                        conditionalPanel(
                            condition = sprintf("input['%s'] == 'loglinear'", ns("model_tabs")),
                            wellPanel(
                                h4(id = ns("title_log_h4"), "Resultados: Modelo loglineal (multiplicativo)"),
                                tableOutput(ns("tab_log"))
                            )
                        )
                    ),
                    tabPanel(
                        title = span(id = ns("tab_formulas"), "Fórmulas"),
                        br(),
                        uiOutput(ns("formulas_ui"))
                    ),
                    tabPanel(
                        title = span(id = ns("tab_glosario"), "Glosario"),
                        br(),
                        tableOutput(ns("tab_glosario_view"))
                    )
                )
            )
        ),
        div(
            class = "print-template",
            uiOutput(ns("print_view"))
        )
    )
}

# ---------- Module Server ----------
ecologicos_multivariable_Server <- function(id, lang) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # ---- Same fmt() pattern as Ecologicos_Observacionales ----
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

        # ---- Translations ----
        observeEvent(lang(), {
            tr <- get_translations(lang(), "ecologicos_multivariable")

            updateNumericInput(session, "b0_lin", label = tr$lbl_b0)
            updateNumericInput(session, "se_b0_lin", label = paste0("SE(", tr$lbl_b0, ")"))
            updateNumericInput(session, "b1_lin", label = tr$lbl_b1_lin)
            updateNumericInput(session, "se_b1_lin", label = paste0("SE(", tr$lbl_b1_lin, ")"))
            updateNumericInput(session, "b2_lin", label = tr$lbl_b2)
            updateNumericInput(session, "se_b2_lin", label = paste0("SE(", tr$lbl_b2, ")"))
            updateNumericInput(session, "z_lin", label = tr$lbl_z_val)
            updateSliderInput(session, "p_lin", label = tr$lbl_p_exp)
            updateNumericInput(session, "zbar_lin", label = tr$lbl_zbar)

            updateNumericInput(session, "b0_log", label = tr$lbl_b0)
            updateNumericInput(session, "se_b0_log", label = paste0("SE(", tr$lbl_b0, ")"))
            updateNumericInput(session, "b1_log", label = tr$lbl_b1_log)
            updateNumericInput(session, "se_b1_log", label = paste0("SE(", tr$lbl_b1_log, ")"))
            updateNumericInput(session, "b2_log", label = tr$lbl_b2)
            updateNumericInput(session, "se_b2_log", label = paste0("SE(", tr$lbl_b2, ")"))
            updateNumericInput(session, "z_log", label = tr$lbl_z_val)
            updateSliderInput(session, "p_log", label = tr$lbl_p_exp)
            updateNumericInput(session, "zbar_log", label = tr$lbl_zbar)

            ids_map <- list(
                "h_settings"     = "h_settings",
                "tab_params_lin" = "tab_params_lin",
                "tab_params_log" = "tab_params_log",
                "tab_res"        = "tab_res",
                "tab_formulas"   = "tab_formulas",
                "tab_glosario"   = "tab_glosario",
                "title_lin_h4"   = "title_lin",
                "title_log_h4"   = "title_log"
            )
            # updateTabsetPanel cannot update titles dynamically easily without re-render or JS.
            # But we used uiOutput in titles, so they update automatically via renderUI above.

            updateActionButton(session, "btn_example_lin", label = tr$btn_example %||% "Cargar ejemplo con SE")
            updateActionButton(session, "btn_example_log", label = tr$btn_example %||% "Cargar ejemplo con SE")
        })

        # ---- Example Handlers ----
        observeEvent(input$btn_example_lin, {
            updateNumericInput(session, "se_b0_lin", value = 0.5)
            updateNumericInput(session, "se_b1_lin", value = 0.3)
            updateNumericInput(session, "se_b2_lin", value = 0.2)
        })

        observeEvent(input$btn_example_log, {
            updateNumericInput(session, "se_b0_log", value = 0.1)
            updateNumericInput(session, "se_b1_log", value = 0.05)
            updateNumericInput(session, "se_b2_log", value = 0.04)
        })

        # ---- Linear model reactive ----
        lin_vals <- reactive({
            b0 <- suppressWarnings(as.numeric(input$b0_lin))
            b1 <- suppressWarnings(as.numeric(input$b1_lin))
            b2 <- suppressWarnings(as.numeric(input$b2_lin))
            sb0 <- suppressWarnings(as.numeric(input$se_b0_lin))
            sb1 <- suppressWarnings(as.numeric(input$se_b1_lin))
            sb2 <- suppressWarnings(as.numeric(input$se_b2_lin))
            z <- suppressWarnings(as.numeric(input$z_lin))
            p <- suppressWarnings(as.numeric(input$p_lin))
            zbar <- suppressWarnings(as.numeric(input$zbar_lin))

            ok <- is.finite(b0) && is.finite(b1) && is.finite(b2) &&
                is.finite(z) && is.finite(p) && is.finite(zbar)

            if (!ok) {
                return(list(ok = FALSE))
            }

            calc_lin <- function(b0, b1, b2, z, p, zbar) {
                Y0_z <- b0 + b2 * z
                Y1_z <- b0 + b1 + b2 * z
                DT_z <- Y1_z - Y0_z
                RT_z <- if (Y0_z != 0) Y1_z / Y0_z else NaN
                FAE_z <- if (is.finite(RT_z) && RT_z != 0) (RT_z - 1) / RT_z else NaN
                Y0_bar <- b0 + b2 * zbar
                Ypop <- Y0_bar + b1 * p
                FAP_pop <- if (is.finite(Ypop) && Ypop != 0) (Ypop - Y0_bar) / Ypop else NaN
                c(Y0_z, Y1_z, DT_z, RT_z, FAE_z, FAP_pop, Ypop)
            }

            point <- calc_lin(b0, b1, b2, z, p, zbar)

            # Bootstrap / Monte Carlo
            ic <- matrix(NA, nrow = 7, ncol = 2)
            sb0_v <- if (is.finite(sb0)) sb0 else 0
            sb1_v <- if (is.finite(sb1)) sb1 else 0
            sb2_v <- if (is.finite(sb2)) sb2 else 0

            if (sb0_v > 0 || sb1_v > 0 || sb2_v > 0) {
                B <- 2000
                set.seed(123)
                sim_b0 <- rnorm(B, b0, sb0_v)
                sim_b1 <- rnorm(B, b1, sb1_v)
                sim_b2 <- rnorm(B, b2, sb2_v)
                results <- sapply(1:B, function(i) calc_lin(sim_b0[i], sim_b1[i], sim_b2[i], z, p, zbar))
                ic <- t(apply(results, 1, quantile, probs = c(0.025, 0.975), na.rm = TRUE))
            } else if (is.finite(sb0) || is.finite(sb1) || is.finite(sb2)) {
                ic <- cbind(point, point)
            }

            list(
                ok = TRUE,
                Y0_z = point[1], Y1_z = point[2], DT_z = point[3],
                RT_z = point[4], FAE_z = point[5], FAP_pop = point[6], Ypop = point[7],
                ic = ic
            )
        })

        # ---- Log-linear model reactive ----
        log_vals <- reactive({
            b0 <- suppressWarnings(as.numeric(input$b0_log))
            b1 <- suppressWarnings(as.numeric(input$b1_log))
            b2 <- suppressWarnings(as.numeric(input$b2_log))
            sb0 <- suppressWarnings(as.numeric(input$se_b0_log))
            sb1 <- suppressWarnings(as.numeric(input$se_b1_log))
            sb2 <- suppressWarnings(as.numeric(input$se_b2_log))
            z <- suppressWarnings(as.numeric(input$z_log))
            p <- suppressWarnings(as.numeric(input$p_log))
            zbar <- suppressWarnings(as.numeric(input$zbar_log))

            ok <- is.finite(b0) && is.finite(b1) && is.finite(b2) &&
                is.finite(z) && is.finite(p) && is.finite(zbar)

            if (!ok) {
                return(list(ok = FALSE))
            }

            calc_log <- function(b0, b1, b2, z, p, zbar) {
                Y0_z <- exp(b0 + b2 * z)
                Y1_z <- exp(b0 + b1 + b2 * z)
                DT_z <- Y1_z - Y0_z
                RT <- exp(b1)
                FAE <- if (is.finite(RT) && RT != 0) (RT - 1) / RT else NaN
                FAP_pop <- (p * (RT - 1)) / (p * (RT - 1) + 1)
                Y0_bar <- exp(b0 + b2 * zbar)
                Ypop <- Y0_bar * ((1 - p) + p * RT)
                c(Y0_z, Y1_z, DT_z, RT, FAE, FAP_pop, Ypop)
            }

            point <- calc_log(b0, b1, b2, z, p, zbar)

            # Bootstrap / Monte Carlo
            ic <- matrix(NA, nrow = 7, ncol = 2)

            sb0_v <- if (is.finite(sb0)) sb0 else 0
            sb1_v <- if (is.finite(sb1)) sb1 else 0
            sb2_v <- if (is.finite(sb2)) sb2 else 0

            if (sb0_v > 0 || sb1_v > 0 || sb2_v > 0) {
                B <- 2000
                set.seed(123)
                sim_b0 <- rnorm(B, b0, sb0_v)
                sim_b1 <- rnorm(B, b1, sb1_v)
                sim_b2 <- rnorm(B, b2, sb2_v)
                results <- sapply(1:B, function(i) calc_log(sim_b0[i], sim_b1[i], sim_b2[i], z, p, zbar))
                ic <- t(apply(results, 1, quantile, probs = c(0.025, 0.975), na.rm = TRUE))
            } else if (is.finite(sb0) || is.finite(sb1) || is.finite(sb2)) {
                ic <- cbind(point, point)
            }

            list(
                ok = TRUE,
                Y0_z = point[1], Y1_z = point[2], DT_z = point[3],
                RT = point[4], FAE = point[5], FAP_pop = point[6], Ypop = point[7],
                ic = ic
            )
        })

        # ---- Render linear table ----
        output$tab_lin <- renderTable({
            tr <- get_translations(lang(), "ecologicos_multivariable")
            v <- lin_vals()

            df <- data.frame(
                Medida = c(
                    tr$res_tnexp %||% "TnExp (Z=z)",
                    tr$res_texp %||% "TExp (Z=z)",
                    tr$res_dt %||% "DT (Z=z)",
                    tr$res_rt %||% "RT (Z=z)",
                    tr$res_fae %||% "FAE (%)",
                    tr$res_fap %||% "FAP (%)",
                    tr$res_tasa_pop %||% "Tasa Pob."
                ),
                Valor = if (v$ok) {
                    c(
                        fmt(v$Y0_z),
                        fmt(v$Y1_z),
                        fmt(v$DT_z),
                        fmt(v$RT_z),
                        fmt_pct(v$FAE_z),
                        fmt_pct(v$FAP_pop),
                        fmt(v$Ypop)
                    )
                } else {
                    rep("", 7)
                },
                IC_95 = if (v$ok) {
                    c(
                        fmt_ic(v$ic[1, 1], v$ic[1, 2]),
                        fmt_ic(v$ic[2, 1], v$ic[2, 2]),
                        fmt_ic(v$ic[3, 1], v$ic[3, 2]),
                        fmt_ic(v$ic[4, 1], v$ic[4, 2]),
                        fmt_ic(v$ic[5, 1], v$ic[5, 2], is_pct = TRUE),
                        fmt_ic(v$ic[6, 1], v$ic[6, 2], is_pct = TRUE),
                        fmt_ic(v$ic[7, 1], v$ic[7, 2])
                    )
                } else {
                    rep("", 7)
                },
                stringsAsFactors = FALSE
            )

            colnames(df) <- c(tr$measure %||% "Medida", tr$value %||% "Valor", tr$col_ic %||% "IC 95%")
            df
        })

        # ---- Render log-linear table ----
        output$tab_log <- renderTable({
            tr <- get_translations(lang(), "ecologicos_multivariable")
            v <- log_vals()

            df <- data.frame(
                Medida = c(
                    tr$res_tnexp %||% "TnExp (Z=z)",
                    tr$res_texp %||% "TExp (Z=z)",
                    tr$res_dt %||% "DT (Z=z)",
                    tr$res_rt_adj %||% "RT ajustada",
                    tr$res_fae %||% "FAE (%)",
                    tr$res_fap %||% "FAP (%)",
                    tr$res_tasa_pop %||% "Tasa Pob."
                ),
                Valor = if (v$ok) {
                    c(
                        fmt(v$Y0_z),
                        fmt(v$Y1_z),
                        fmt(v$DT_z),
                        fmt(v$RT),
                        fmt_pct(v$FAE),
                        fmt_pct(v$FAP_pop),
                        fmt(v$Ypop)
                    )
                } else {
                    rep("", 7)
                },
                IC_95 = if (v$ok) {
                    c(
                        fmt_ic(v$ic[1, 1], v$ic[1, 2]),
                        fmt_ic(v$ic[2, 1], v$ic[2, 2]),
                        fmt_ic(v$ic[3, 1], v$ic[3, 2]),
                        fmt_ic(v$ic[4, 1], v$ic[4, 2]),
                        fmt_ic(v$ic[5, 1], v$ic[5, 2], is_pct = TRUE),
                        fmt_ic(v$ic[6, 1], v$ic[6, 2], is_pct = TRUE),
                        fmt_ic(v$ic[7, 1], v$ic[7, 2])
                    )
                } else {
                    rep("", 7)
                },
                stringsAsFactors = FALSE
            )

            colnames(df) <- c(tr$measure %||% "Medida", tr$value %||% "Valor", tr$col_ic %||% "IC 95%")
            df
        })

        # ---- Glosario ----
        output$tab_glosario_view <- renderTable({
            tr <- get_translations(lang(), "ecologicos_multivariable")
            data.frame(
                Termino = tr$glosario_terms,
                Definicion = tr$glosario_defs,
                stringsAsFactors = FALSE
            )
        })

        # ---- Fórmulas ----
        output$formulas_ui <- renderUI({
            tr <- get_translations(lang(), "ecologicos_multivariable")
            HTML(tr$formulas_html)
        })

        # ---- Print view ----
        output$print_view <- renderUI({
            tr <- get_translations(lang(), "ecologicos_multivariable")
            v_lin <- lin_vals()
            v_log <- log_vals()

            lin_text <- if (v_lin$ok) {
                paste0(
                    "TnExp = ", fmt(v_lin$Y0_z), " ", fmt_ic(v_lin$ic[1, 1], v_lin$ic[1, 2]), "\n",
                    "TExp = ", fmt(v_lin$Y1_z), " ", fmt_ic(v_lin$ic[2, 1], v_lin$ic[2, 2]), "\n",
                    "DT = ", fmt(v_lin$DT_z), " ", fmt_ic(v_lin$ic[3, 1], v_lin$ic[3, 2]), "\n",
                    "RT = ", fmt(v_lin$RT_z), " ", fmt_ic(v_lin$ic[4, 1], v_lin$ic[4, 2]), "\n",
                    "FAE = ", fmt_pct(v_lin$FAE_z), " ", fmt_ic(v_lin$ic[5, 1], v_lin$ic[5, 2], is_pct = TRUE), "\n",
                    "FAP = ", fmt_pct(v_lin$FAP_pop), " ", fmt_ic(v_lin$ic[6, 1], v_lin$ic[6, 2], is_pct = TRUE), "\n",
                    "Tasa Pob. = ", fmt(v_lin$Ypop), " ", fmt_ic(v_lin$ic[7, 1], v_lin$ic[7, 2])
                )
            } else {
                ""
            }

            log_text <- if (v_log$ok) {
                paste0(
                    "TnExp = ", fmt(v_log$Y0_z), " ", fmt_ic(v_log$ic[1, 1], v_log$ic[1, 2]), "\n",
                    "TExp = ", fmt(v_log$Y1_z), " ", fmt_ic(v_log$ic[2, 1], v_log$ic[2, 2]), "\n",
                    "DT = ", fmt(v_log$DT_z), " ", fmt_ic(v_log$ic[3, 1], v_log$ic[3, 2]), "\n",
                    "RT adj. = ", fmt(v_log$RT), " ", fmt_ic(v_log$ic[4, 1], v_log$ic[4, 2]), "\n",
                    "FAE = ", fmt_pct(v_log$FAE), " ", fmt_ic(v_log$ic[5, 1], v_log$ic[5, 2], is_pct = TRUE), "\n",
                    "FAP = ", fmt_pct(v_log$FAP_pop), " ", fmt_ic(v_log$ic[6, 1], v_log$ic[6, 2], is_pct = TRUE), "\n",
                    "Tasa Pob. = ", fmt(v_log$Ypop), " ", fmt_ic(v_log$ic[7, 1], v_log$ic[7, 2])
                )
            } else {
                ""
            }

            tagList(
                div(
                    class = "printable-section",
                    h3(tr$title_lin %||% "Modelo lineal"),
                    tags$pre(lin_text)
                ),
                div(
                    class = "printable-section",
                    h3(tr$title_log %||% "Modelo loglineal"),
                    tags$pre(log_text)
                )
            )
        })

        outputOptions(output, "print_view", suspendWhenHidden = FALSE)
    })
}
