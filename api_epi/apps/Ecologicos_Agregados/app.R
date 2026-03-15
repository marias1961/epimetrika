library(shiny)

# ---------- Module UI ----------
ecologicos_agregados_UI <- function(id) {
    ns <- NS(id)
    tagList(
        sidebarLayout(
            sidebarPanel(
                h4(id = ns("h_settings"), "Configuración"),
                sliderInput(ns("conf"), "Nivel de confianza", min = 0.80, max = 0.99, value = 0.95, step = 0.01),
                numericInput(ns("base"), "Base de tasas (por ... persona-tiempo)", value = 100000, min = 1, step = 1),
                selectInput(ns("n_groups"), "Número de grupos (áreas/estratos)", choices = 2:10, selected = 3),
                checkboxInput(ns("wls"), "Ponderar OLS por PT_total (WLS)", value = FALSE),
                hr(),
                checkboxInput(ns("has_indiv"), "Dispongo de casos por exposición dentro de cada grupo (opcional)", value = TRUE),
                hr(),
                actionButton(ns("load_example"), "Cargar ejemplo (3 grupos)"),
                helpText(id = ns("help_main"), "Por grupo: PT_e, PT_0 y casos totales. Pe = PT_e/PT_total.")
            ),
            mainPanel(
                tabsetPanel(
                    tabPanel(
                        title = span(id = ns("tab_data"), "1) Datos por grupo"),
                        br(),
                        uiOutput(ns("groups_ui")),
                        hr(),
                        h4(id = ns("h_summary"), "Resumen por grupo (para análisis ecológico)"),
                        tableOutput(ns("tab_group")),
                        hr(),
                        uiOutput(ns("note_consistency"))
                    ),
                    tabPanel(
                        title = span(id = ns("tab_eco"), "2) Análisis ecológico (regresión)"),
                        br(),
                        h4(id = ns("title_add"), "Modelo aditivo (OLS): tasa_total = B0 + B1·Pe"),
                        helpText(id = ns("help_add"), "IR=(B0+B1)/B0. Si IR≥1: FAE=(IR−1)/IR y FAP=(B1·p)/(B0+B1·p). Si IR<1: FPE=1−IR y FPP=p(1−IR)/(1−p·IR)."),
                        tableOutput(ns("tab_eco_add")),
                        hr(),
                        h4(id = ns("title_mul"), "Modelo exponencial (Poisson log-lineal): log(tasa_total) = B0 + B1·Pe"),
                        helpText(id = ns("help_mul"), "IR=exp(B1). Si IR≥1: FAE=(IR−1)/IR y FAP=1−exp(−B1·p). Si IR<1: FPE=1−IR y FPP=p(1−IR)/(1−p·IR)."),
                        tableOutput(ns("tab_eco_mul")),
                        hr(),
                        h4(id = ns("title_plot"), "Gráfico: tasa total vs Pe"),
                        plotOutput(ns("plot_eco"), height = 320)
                    ),
                    tabPanel(
                        title = span(id = ns("tab_individual"), "3) Comparación con nivel individual (opcional)"),
                        br(),
                        conditionalPanel(
                            condition = sprintf("input['%s'] == true", ns("has_indiv")),
                            h4(id = ns("title_indiv_grp"), "Dentro de cada grupo: tasas por exposición"),
                            tableOutput(ns("tab_indiv_by_group")),
                            hr(),
                            h4(id = ns("title_indiv_glob"), "Efecto global individual: crudo y ajustado por grupo (SMR; estándar=expuestos)"),
                            tableOutput(ns("tab_indiv_global")),
                            hr(),
                            h4(id = ns("title_compare"), "Comparación: IR ecológica vs IR individual"),
                            tableOutput(ns("tab_compare"))
                        ),
                        conditionalPanel(
                            condition = sprintf("input['%s'] == false", ns("has_indiv")),
                            helpText(id = ns("help_indiv"), "Activa la casilla de 'casos por exposición' para comparar con el análisis individual.")
                        )
                    ),
                    tabPanel(
                        title = span(id = ns("tab_help"), "4) Ayuda"),
                        br(),
                        uiOutput(ns("help_content"))
                    ),
                    tabPanel(
                        title = span(id = ns("tab_glossary"), "5) Glosario"),
                        br(),
                        tableOutput(ns("tab_glosario"))
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
ecologicos_agregados_Server <- function(id, lang) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        observeEvent(lang(), {
            tr <- get_translations(lang(), "ecologicos_agregados")

            updateSliderInput(session, "conf", label = tr$lbl_conf)
            updateNumericInput(session, "base", label = tr$lbl_base)
            updateSelectInput(session, "n_groups", label = tr$lbl_n_groups)
            updateCheckboxInput(session, "wls", label = tr$lbl_wls)
            updateCheckboxInput(session, "has_indiv", label = tr$lbl_has_indiv)
            updateActionButton(session, "load_example", label = tr$btn_load_example)

            ids_map <- list(
                "h_settings" = "h_settings",
                "help_main" = "help_main",
                "tab_data" = "tab_data",
                "h_summary" = "h_summary",
                "tab_eco" = "tab_eco",
                "title_add" = "title_add",
                "help_add" = "help_add",
                "title_mul" = "title_mul",
                "help_mul" = "help_mul",
                "title_plot" = "title_plot",
                "tab_individual" = "tab_individual",
                "title_indiv_grp" = "title_indiv_grp",
                "title_indiv_glob" = "title_indiv_glob",
                "title_compare" = "title_compare",
                "help_indiv" = "help_indiv",
                "tab_help" = "tab_help",
                "tab_glossary" = "tab_glossary"
            )
            update_static_ui(session, ids_map, tr, ns)
        })

        # --- UI dinámica para grupos ---
        output$groups_ui <- renderUI({
            n <- as.integer(input$n_groups)
            req(n)
            tr <- get_translations(lang(), "ecologicos_agregados")

            lapply(seq_len(n), function(i) {
                wellPanel(
                    fluidRow(
                        column(3, textInput(ns(paste0("gname_", i)), paste0(tr$lbl_grp, " ", i, " ", tr$lbl_name), value = paste0(tr$lbl_grp, " ", i))),
                        column(3, numericInput(ns(paste0("pte_", i)), tr$lbl_pte, value = 0, min = 0, step = 1)),
                        column(3, numericInput(ns(paste0("pt0_", i)), tr$lbl_pt0, value = 0, min = 0, step = 1)),
                        column(3, numericInput(ns(paste0("casesTot_", i)), tr$lbl_cases_tot, value = 0, min = 0, step = 1))
                    ),
                    conditionalPanel(
                        condition = sprintf("input['%s'] == true", ns("has_indiv")),
                        fluidRow(
                            column(6, numericInput(ns(paste0("casesE_", i)), tr$lbl_cases_e, value = 0, min = 0, step = 1)),
                            column(6, numericInput(ns(paste0("casesU_", i)), tr$lbl_cases_u, value = 0, min = 0, step = 1))
                        ),
                        helpText(tr$help_rec_tot)
                    )
                )
            })
        })

        # --- Botón: cargar ejemplo ---
        observeEvent(input$load_example, {
            updateSelectInput(session, "n_groups", selected = 3)
            ex <- list(
                pte = c(7000, 10000, 13000),
                pt0 = c(13000, 10000, 7000),
                tot = c(33, 30, 27),
                ce  = c(20, 20, 20),
                cu  = c(13, 10, 7)
            )
            for (i in 1:3) {
                updateTextInput(session, paste0("gname_", i), value = paste0("Grupo ", i))
                updateNumericInput(session, paste0("pte_", i), value = ex$pte[i])
                updateNumericInput(session, paste0("pt0_", i), value = ex$pt0[i])
                updateNumericInput(session, paste0("casesTot_", i), value = ex$tot[i])
                updateNumericInput(session, paste0("casesE_", i), value = ex$ce[i])
                updateNumericInput(session, paste0("casesU_", i), value = ex$cu[i])
            }
        })

        # --- Utilidades Internas ---
        safe_num <- function(x) {
            if (is.null(x) || length(x) == 0) {
                return(NA_real_)
            }
            res <- suppressWarnings(as.numeric(x))
            if (length(res) == 0) {
                return(NA_real_)
            }
            res
        }
        fmt_num <- function(x, k = 3) {
            if (is.null(x) || length(x) == 0 || is.na(x)) {
                return("---")
            }
            sprintf(paste0("%.", k, "f"), as.numeric(x))
        }
        fmt_pct <- function(x) {
            if (is.null(x) || length(x) == 0 || is.na(x)) {
                return("---")
            }
            sprintf("%.1f%%", 100 * as.numeric(x))
        }

        groups_df <- reactive({
            n <- as.integer(input$n_groups)
            req(n)
            df <- data.frame(group = character(n), PT_e = numeric(n), PT_0 = numeric(n), PT = numeric(n), cases = numeric(n), Pe = numeric(n), rate = numeric(n), stringsAsFactors = FALSE)
            for (i in seq_len(n)) {
                gname <- input[[paste0("gname_", i)]]
                pte <- safe_num(input[[paste0("pte_", i)]])
                pt0 <- safe_num(input[[paste0("pt0_", i)]])
                tot <- safe_num(input[[paste0("casesTot_", i)]])
                if (isTRUE(input$has_indiv)) {
                    ce <- safe_num(input[[paste0("casesE_", i)]])
                    cu <- safe_num(input[[paste0("casesU_", i)]])
                    if (!any(is.na(c(ce, cu)))) tot <- ce + cu
                }
                PTtot <- pte + pt0
                df$group[i] <- if (!is.null(gname) && nzchar(gname)) gname else paste0("Grupo ", i)
                df$PT_e[i] <- pte
                df$PT_0[i] <- pt0
                df$PT[i] <- PTtot
                df$cases[i] <- tot
                df$Pe[i] <- if (is.finite(PTtot) && PTtot > 0) pte / PTtot else NA
                df$rate[i] <- if (is.finite(PTtot) && PTtot > 0 && is.finite(tot)) tot / PTtot else NA
            }
            df
        })

        p_pop <- reactive({
            df <- groups_df()
            ok <- is.finite(df$PT) & df$PT > 0 & is.finite(df$Pe)
            d <- df[ok, , drop = FALSE]
            if (nrow(d) == 0) {
                return(NA_real_)
            }
            sum(d$Pe * d$PT) / sum(d$PT)
        })

        output$note_consistency <- renderUI({
            if (!isTRUE(input$has_indiv)) {
                return(NULL)
            }
            tr <- get_translations(lang(), "ecologicos_agregados")
            n <- as.integer(input$n_groups)
            req(n)
            msgs <- c()
            for (i in seq_len(n)) {
                tot_in <- safe_num(input[[paste0("casesTot_", i)]])
                ce <- safe_num(input[[paste0("casesE_", i)]])
                cu <- safe_num(input[[paste0("casesU_", i)]])
                if (!any(is.na(c(tot_in, ce, cu))) && abs((ce + cu) - tot_in) > 0.0001) {
                    msgs <- c(msgs, paste0(tr$lbl_grp, " ", i, ": ", tr$help_rec_tot))
                }
            }
            if (length(msgs) == 0) {
                return(NULL)
            }
            tagList(lapply(msgs, helpText))
        })

        output$tab_group <- renderTable({
            df <- groups_df()
            base <- as.numeric(input$base)
            tr <- get_translations(lang(), "ecologicos_agregados")
            msg_min <- if (!is.null(tr$val_min_grp)) tr$val_min_grp else "Introduce al menos 2 grupos / Enter at least 2 groups"
            msg_pt <- if (!is.null(tr$val_pt_pos)) tr$val_pt_pos else "PT_total debe ser > 0 / PT_total must be > 0"

            shiny::validate(
                shiny::need(nrow(df) >= 2, msg_min),
                shiny::need(all(is.finite(df$PT) & df$PT > 0), msg_pt)
            )
            df_view <- df
            df_view$Pe <- round(100 * df_view$Pe, 1)
            df_view$rate <- round(df_view$rate * base, 1)
            colnames(df_view) <- c(tr$col_grp, tr$col_pte, tr$col_pt0, tr$col_pt_tot, tr$col_cases, tr$col_pe_pct, tr$col_rate_base)
            df_view
        })

        # --- Ajuste ecológico: OLS aditivo ---
        eco_add <- reactive({
            df <- groups_df()
            base <- as.numeric(input$base)
            conf <- as.numeric(input$conf)
            z <- qnorm(1 - (1 - conf) / 2)
            ok <- is.finite(df$PT) & df$PT > 0 & is.finite(df$cases) & is.finite(df$Pe)
            d <- df[ok, , drop = FALSE]
            if (nrow(d) < 2) {
                return(NULL)
            }
            Y <- d$rate * base
            X <- d$Pe
            w <- if (isTRUE(input$wls)) d$PT else NULL
            fit <- tryCatch(
                {
                    if (is.null(w)) lm(Y ~ X) else lm(Y ~ X, weights = w)
                },
                error = function(e) NULL
            )
            if (is.null(fit)) {
                return(NULL)
            }
            co <- coef(fit)
            vc <- tryCatch(vcov(fit), error = function(e) NULL)
            if (is.null(vc) || length(co) < 2) {
                return(NULL)
            }
            B0 <- co[1]
            B1 <- co[2]
            IR <- if (B0 != 0) (B0 + B1) / B0 else NA
            ciIR <- c(NA, NA)
            if (!is.na(IR) && B0 != 0) {
                grad <- c(-B1 / (B0^2), 1 / B0)
                varIR <- as.numeric(t(grad) %*% vc %*% grad)
                if (is.finite(varIR) && varIR >= 0) ciIR <- c(IR - z * sqrt(varIR), IR + z * sqrt(varIR))
            }

            p <- p_pop()

            # FAE / FAP
            fae <- NA
            ciFAE <- c(NA, NA)
            fap <- NA
            ciFAP <- c(NA, NA)
            if (!is.na(IR)) {
                if (IR >= 1) {
                    fae <- (IR - 1) / IR
                    fap <- (B1 * p) / (B0 + B1 * p)
                    if (!any(is.na(ciIR))) {
                        ciFAE <- (ciIR - 1) / ciIR
                        # Aprox FAP IC via Delta or just direct from B1/B0?
                        # Simplifying: if IR >= 1, we use IR bounds for FAE.
                        # FAP = p(IR-1)/(1 + p(IR-1))
                        ciFAP <- (p * (ciIR - 1)) / (1 + p * (ciIR - 1))
                    }
                } else {
                    # Preventive
                    fae <- 1 - IR # FPE
                    fap <- p * (1 - IR) / (1 - p * IR) # FPP
                    if (!any(is.na(ciIR))) {
                        ciFAE <- 1 - rev(ciIR)
                        ciFAP <- p * (1 - rev(ciIR)) / (1 - p * rev(ciIR))
                    }
                }
            }

            sfit <- summary(fit)
            r2 <- sfit$r.squared
            r2adj <- sfit$adj.r.squared

            # B1 stats
            b1_stats <- sfit$coefficients[2, ]
            seB1 <- b1_stats["Std. Error"]
            pValB1 <- b1_stats["Pr(>|t|)"]

            list(
                n = nrow(d), B0 = B0, B1 = B1, IR = IR, ciIR = ciIR,
                fae = fae, ciFAE = ciFAE, fap = fap, ciFAP = ciFAP,
                p = p, fit = fit, r2 = r2, r2adj = r2adj, seB1 = seB1, pValB1 = pValB1
            )
        })

        output$tab_eco_add <- renderTable({
            res <- eco_add()
            tr <- get_translations(lang(), "ecologicos_agregados")
            msg_min <- if (!is.null(tr$val_min_grp)) tr$val_min_grp else "Introduce al menos 2 grupos / Enter at least 2 groups"
            shiny::validate(shiny::need(!is.null(res), msg_min))

            is_prev <- !is.na(res$IR) && res$IR < 1
            lbl_fae <- if (is_prev) tr$lbl_fpe %||% "FPE (%)" else tr$lbl_fae %||% "FAE (%)"
            lbl_fap <- if (is_prev) tr$lbl_fpp %||% "FPP (%)" else tr$lbl_fap %||% "FAP (%)"

            data.frame(
                Measure = c("n", "B0", "B1", tr$lbl_se, tr$lbl_p_val, "IR", "IC IR", lbl_fae, "IC FAE/FPE", lbl_fap, "IC FAP/FPP", "p", tr$lbl_r2, tr$lbl_r2_adj),
                Value = c(
                    res$n,
                    fmt_num(res$B0, 2),
                    fmt_num(res$B1, 2),
                    fmt_num(res$seB1, 3),
                    fmt_num(res$pValB1, 4),
                    fmt_num(res$IR, 3),
                    paste0(fmt_num(res$ciIR[1], 3), " ; ", fmt_num(res$ciIR[2], 3)),
                    fmt_pct(res$fae),
                    paste0(fmt_pct(res$ciFAE[1]), " ; ", fmt_pct(res$ciFAE[2])),
                    fmt_pct(res$fap),
                    paste0(fmt_pct(res$ciFAP[1]), " ; ", fmt_pct(res$ciFAP[2])),
                    fmt_num(res$p, 3),
                    fmt_num(res$r2, 3),
                    fmt_num(res$r2adj, 3)
                )
            )
        })

        # --- Ajuste ecológico: Poisson ---
        eco_mul <- reactive({
            df <- groups_df()
            conf <- as.numeric(input$conf)
            z <- qnorm(1 - (1 - conf) / 2)
            ok <- is.finite(df$PT) & df$PT > 0 & is.finite(df$cases) & is.finite(df$Pe)
            d <- df[ok, , drop = FALSE]
            if (nrow(d) < 2) {
                return(NULL)
            }
            d$cases_int <- round(d$cases)
            fit <- tryCatch(glm(cases_int ~ Pe, family = poisson(link = "log"), offset = log(PT), data = d), error = function(e) NULL)
            if (is.null(fit)) {
                return(NULL)
            }
            co <- coef(fit)
            vc <- tryCatch(vcov(fit), error = function(e) NULL)
            if (is.null(vc) || length(co) < 2) {
                return(NULL)
            }
            B1 <- co[2]
            seB1 <- sqrt(vc[2, 2])
            IR <- exp(B1)
            ciIR <- exp(c(B1 - z * seB1, B1 + z * seB1))

            p <- p_pop()

            # FAE / FAP
            fae <- NA
            ciFAE <- c(NA, NA)
            fap <- NA
            ciFAP <- c(NA, NA)
            if (!is.na(IR)) {
                if (IR >= 1) {
                    fae <- (IR - 1) / IR
                    fap <- 1 - exp(-B1 * p)
                    if (!any(is.na(ciIR))) {
                        ciFAE <- (ciIR - 1) / ciIR
                        ciFAP <- 1 - exp(-c(B1 - z * seB1, B1 + z * seB1) * p)
                    }
                } else {
                    fae <- 1 - IR
                    fap <- p * (1 - IR) / (1 - p * IR)
                    if (!any(is.na(ciIR))) {
                        ciFAE <- 1 - rev(ciIR)
                        ciFAP <- p * (1 - rev(ciIR)) / (1 - p * rev(ciIR))
                    }
                }
            }

            # Poisson Diagnostics
            sfit <- summary(fit)
            resid_dev <- sfit$deviance
            null_dev <- sfit$null.deviance
            aic <- sfit$aic
            phi <- sfit$deviance / sfit$df.residual

            # B1 stats
            b1_stats <- sfit$coefficients[2, ]
            seB1 <- b1_stats["Std. Error"]
            pValB1 <- b1_stats["Pr(>|z|)"]

            list(
                n = nrow(d), B1 = B1, IR = IR, ciIR = ciIR,
                fae = fae, ciFAE = ciFAE, fap = fap, ciFAP = ciFAP,
                fit = fit, resid_dev = resid_dev, null_dev = null_dev,
                aic = aic, phi = phi, seB1 = seB1, pValB1 = pValB1
            )
        })

        output$tab_eco_mul <- renderTable({
            res <- eco_mul()
            tr <- get_translations(lang(), "ecologicos_agregados")
            msg_min <- if (!is.null(tr$val_min_grp)) tr$val_min_grp else "Introduce al menos 2 grupos / Enter at least 2 groups"
            shiny::validate(shiny::need(!is.null(res), msg_min))

            is_prev <- !is.na(res$IR) && res$IR < 1
            lbl_fae <- if (is_prev) tr$lbl_fpe %||% "FPE (%)" else tr$lbl_fae %||% "FAE (%)"
            lbl_fap <- if (is_prev) tr$lbl_fpp %||% "FPP (%)" else tr$lbl_fap %||% "FAP (%)"

            data.frame(
                Measure = c("n", "B1", tr$lbl_se, tr$lbl_p_val, "IR", "IC IR", lbl_fae, "IC FAE/FPE", lbl_fap, "IC FAP/FPP", tr$lbl_resid_dev, tr$lbl_null_dev, tr$lbl_aic, tr$lbl_phi),
                Value = c(
                    res$n,
                    fmt_num(res$B1, 3),
                    fmt_num(res$seB1, 3),
                    fmt_num(res$pValB1, 4),
                    fmt_num(res$IR, 3),
                    paste0(fmt_num(res$ciIR[1], 3), " ; ", fmt_num(res$ciIR[2], 3)),
                    fmt_pct(res$fae),
                    paste0(fmt_pct(res$ciFAE[1]), " ; ", fmt_pct(res$ciFAE[2])),
                    fmt_pct(res$fap),
                    paste0(fmt_pct(res$ciFAP[1]), " ; ", fmt_pct(res$ciFAP[2])),
                    fmt_num(res$resid_dev, 2),
                    fmt_num(res$null_dev, 2),
                    fmt_num(res$aic, 2),
                    fmt_num(res$phi, 3)
                )
            )
        })

        output$plot_eco <- renderPlot({
            df <- groups_df()
            base <- as.numeric(input$base)
            ok <- is.finite(df$PT) & df$PT > 0 & is.finite(df$cases) & is.finite(df$Pe)
            d <- df[ok, , drop = FALSE]
            shiny::validate(shiny::need(nrow(d) >= 2, "Introduce al menos 2 grupos / Enter at least 2 groups"))

            tr <- get_translations(lang(), "ecologicos_agregados")
            par(mar = c(5, 5, 4, 12)) # Wider right margin for legend

            plot(d$Pe, d$rate * base,
                xlab = "Pe (Proporción expuestos)",
                ylab = paste0("Tasa por ", base),
                main = tr$title_plot,
                pch = 19, col = "#2c3e50", cex = 1.2
            )

            # Model lines
            a <- eco_add()
            m <- eco_mul()

            if (!is.null(a)) abline(coef(a$fit)[1], coef(a$fit)[2], lwd = 3, col = "#e74c3c")

            if (!is.null(m)) {
                xs <- seq(0, 1, length.out = 100)
                # Prediction. Offset log(PT) means log(mu) = B0 + B1*X + log(PT)
                # so mu = exp(B0 + B1*X) * PT. For rate (mu/PT), we use PT=1.
                pred_df <- data.frame(Pe = xs, PT = 1)
                ys <- predict(m$fit, newdata = pred_df, type = "response") * base
                lines(xs, ys, lwd = 3, lty = 2, col = "#3498db")
            }

            # Legend out of box
            legend("topright",
                inset = c(-0.5, 0), xpd = TRUE,
                legend = c("Modelo Aditivo (OLS/WLS)", "Modelo Exponencial (Poisson)"),
                col = c("#e74c3c", "#3498db"),
                lty = c(1, 2), lwd = 3, bty = "n", cex = 0.9
            )
        })

        # --- Individual Comparison ---
        indiv_calc <- reactive({
            if (!isTRUE(input$has_indiv)) {
                return(NULL)
            }
            n <- as.integer(input$n_groups)
            df <- groups_df()

            # Recopilar casos E y U
            df$casesE <- sapply(1:n, function(i) safe_num(input[[paste0("casesE_", i)]]))
            df$casesU <- sapply(1:n, function(i) safe_num(input[[paste0("casesU_", i)]]))

            # Tasas por grupo
            df$rateE <- df$casesE / df$PT_e
            df$rateU <- df$casesU / df$PT_0
            df$IR_indiv <- df$rateE / df$rateU

            # Global crudo
            sumCE <- sum(df$casesE, na.rm = TRUE)
            sumCU <- sum(df$casesU, na.rm = TRUE)
            sumPTE <- sum(df$PT_e, na.rm = TRUE)
            sumPT0 <- sum(df$PT_0, na.rm = TRUE)

            IR_crude <- (sumCE / sumPTE) / (sumCU / sumPT0)

            # SMR (ajustado por grupo)
            # E = sum( rateU_i * PT_e_i )
            E_total <- sum(df$rateU * df$PT_e, na.rm = TRUE)
            O_total <- sumCE
            SMR <- if (E_total > 0) O_total / E_total else NA

            list(df = df, IR_crude = IR_crude, SMR = SMR)
        })

        output$tab_indiv_by_group <- renderTable({
            res <- indiv_calc()
            req(res)
            tr <- get_translations(lang(), "ecologicos_agregados")
            base <- as.numeric(input$base)

            df_v <- res$df[, c("group", "casesE", "PT_e", "casesU", "PT_0", "rateE", "rateU", "IR_indiv")]
            df_v$rateE <- df_v$rateE * base
            df_v$rateU <- df_v$rateU * base

            colnames(df_v) <- c(tr$col_grp, tr$lbl_cases_e, tr$col_pte, tr$lbl_cases_u, tr$col_pt0, "Rate E", "Rate U", "IR Indiv")
            df_v
        })

        output$tab_indiv_global <- renderTable({
            res <- indiv_calc()
            req(res)
            tr <- get_translations(lang(), "ecologicos_agregados")
            data.frame(
                Medida = c("IR Individual Cruda", "SMR (IR ajustada)"),
                Valor = c(fmt_num(res$IR_crude, 3), fmt_num(res$SMR, 3))
            )
        })

        output$tab_compare <- renderTable({
            a <- eco_add()
            m <- eco_mul()
            i <- indiv_calc()
            req(a, m, i)
            data.frame(
                Model = c("Ecológico Aditivo (OLS)", "Ecológico Poisson", "Individual Crudo", "Individual Ajustado (SMR)"),
                IR_Value = c(fmt_num(a$IR, 3), fmt_num(m$IR, 3), fmt_num(i$IR_crude, 3), fmt_num(i$SMR, 3))
            )
        })

        # --- Glosario ---
        output$tab_glosario <- renderTable({
            tr <- get_translations(lang(), "ecologicos_agregados")
            glosario_df <- data.frame(Termino = tr$glosario_terms, Definicion = tr$glosario_defs)
            colnames(glosario_df) <- c(tr$col_term, tr$col_def)
            glosario_df
        })

        # --- Ayuda ---
        output$help_content <- renderUI({
            tr <- get_translations(lang(), "ecologicos_agregados")
            HTML(tr$help_html)
        })

        # --- Print View ---
        output$print_view <- renderUI({
            tr <- get_translations(lang(), "ecologicos_agregados")
            a <- eco_add()
            m <- eco_mul()
            i <- indiv_calc()

            sections <- list(
                h2(tr$tab_eco),

                # Additive
                h3(tr$title_add),
                if (!is.null(a)) {
                    tagList(
                        tags$ul(
                            tags$li(paste0("B0 = ", fmt_num(a$B0, 2))),
                            tags$li(paste0("B1 = ", fmt_num(a$B1, 2), " (SE=", fmt_num(a$seB1, 3), ", p=", fmt_num(a$pValB1, 4), ")")),
                            tags$li(paste0("IR = ", fmt_num(a$IR, 3), " [", fmt_num(a$ciIR[1], 3), " - ", fmt_num(a$ciIR[2], 3), "]")),
                            tags$li(paste0(tr$lbl_r2, " = ", fmt_num(a$r2, 3), " (", tr$lbl_r2_adj, " = ", fmt_num(a$r2adj, 3), ")"))
                        )
                    )
                } else {
                    p("N/A")
                },

                # Multiplicative
                h3(tr$title_mul),
                if (!is.null(m)) {
                    tagList(
                        tags$ul(
                            tags$li(paste0("B1 = ", fmt_num(m$B1, 3), " (SE=", fmt_num(m$seB1, 3), ", p=", fmt_num(m$pValB1, 4), ")")),
                            tags$li(paste0("IR = ", fmt_num(m$IR, 3), " [", fmt_num(m$ciIR[1], 3), " - ", fmt_num(m$ciIR[2], 3), "]")),
                            tags$li(paste0(tr$lbl_phi, " = ", fmt_num(m$phi, 3))),
                            tags$li(paste0(tr$lbl_aic, " = ", fmt_num(m$aic, 2), " (", tr$lbl_resid_dev, " = ", fmt_num(m$resid_dev, 2), ")"))
                        )
                    )
                } else {
                    p("N/A")
                }
            )

            if (!is.null(i)) {
                sections <- c(sections, list(
                    h3(tr$title_indiv_glob),
                    tags$ul(
                        tags$li(paste0("IR Crude = ", fmt_num(i$IR_crude, 3))),
                        tags$li(paste0("SMR (IR adjusted) = ", fmt_num(i$SMR, 3)))
                    )
                ))
            }

            tagList(div(class = "printable-section", sections))
        })

        outputOptions(output, "print_view", suspendWhenHidden = FALSE)
    })
}
