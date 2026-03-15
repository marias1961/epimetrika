library(shiny)

# ==============================================================================
# Funciones helper para el módulo de Tamaño Muestral
# ==============================================================================

# Ajuste por pérdidas
tm_adj <- function(n, loss_pct) {
    if (is.null(n) || is.na(n)) {
        return(NA)
    }
    if (is.infinite(n)) {
        return(Inf)
    }
    ceiling(n / (1 - loss_pct / 100))
}

# Asignación por ratio expuestos/no-expuestos
tm_alloc_ratio <- function(N_total, ratio_exp_noexp) {
    r <- ratio_exp_noexp
    if (is.infinite(N_total) || N_total <= 0) {
        return(list(n_noexp = Inf, n_exp = Inf, N_total = Inf))
    }
    if (is.na(r) || r <= 0) {
        return(list(n_noexp = Inf, n_exp = Inf, N_total = Inf))
    }
    n_noexp <- ceiling(N_total / (1 + r))
    n_exp <- ceiling(N_total - n_noexp)
    list(n_noexp = n_noexp, n_exp = n_exp, N_total = n_noexp + n_exp)
}

# Interpretación HTML
tm_interpretar_html <- function(alpha, power, n_net, n_tot, type_n, extra = "", loss_pct = 0, tr = NULL) {
    # Default translations if tr is NULL (Spanish fallback but now more robust)
    if (is.null(tr)) {
        tr <- list(
            interpretation = "Interpretación:",
            recruit_with_losses = "Considerando {loss}% de pérdidas, reclutar:",
            note = "Nota: {extra}",
            alfa_power_phrase = "Con Error Alfa del {alpha}%{power}:<br>",
            total_participants_phrase = "Se requieren <b>{n}</b> participantes ({type}).<br><br>",
            power_phrase = " y Potencia del <b>{power}%</b>"
        )
    }

    pow_str <- ""
    if (!is.null(power) && !is.na(power)) {
        pow_val <- round(power * 100, 1)
        pow_template <- if (!is.null(tr$power_phrase)) tr$power_phrase else " y Potencia del <b>{power}%</b>"
        pow_str <- gsub("{power}", pow_val, pow_template, fixed = TRUE)
    }

    # Interpretation header
    header <- paste0("<div class='interpretation-box'><b>", tr$interpretation, "</b><br>")

    # Alfa and Power line
    alfa_val <- round(alpha * 100, 2)
    alfa_power_template <- if (!is.null(tr$alfa_power_phrase)) tr$alfa_power_phrase else "Con Error Alfa del {alpha}%{power}:<br>"
    alfa_power_line <- gsub("{alpha}", alfa_val, alfa_power_template, fixed = TRUE)
    alfa_power_line <- gsub("{power}", pow_str, alfa_power_line, fixed = TRUE)

    # Net requirement
    total_participants_template <- if (!is.null(tr$total_participants_phrase)) tr$total_participants_phrase else "Se requieren <b>{n}</b> participantes ({type}).<br><br>"
    net_line <- gsub("{n}", n_net, total_participants_template, fixed = TRUE)
    net_line <- gsub("{type}", type_n, net_line, fixed = TRUE)

    # Losses line
    loss_text_template <- if (!is.null(tr$recruit_with_losses)) tr$recruit_with_losses else "Considerando {loss}% de pérdidas, reclutar:"
    loss_text <- gsub("{loss}", loss_pct, loss_text_template, fixed = TRUE)
    loss_line <- paste0(loss_text, "<br><span style='font-size:1.5em; color:#d32f2f; font-weight:bold;'>", n_tot, "</span>")

    # Note
    note_line <- ""
    if (extra != "") {
        note_text_template <- if (!is.null(tr$note)) tr$note else "Nota: {extra}"
        note_text <- gsub("{extra}", extra, note_text_template, fixed = TRUE)
        note_line <- paste0("<br><br><i>", note_text, "</i>")
    }

    HTML(paste0(header, alfa_power_line, net_line, loss_line, note_line, "</div>"))
}

# Corrección de continuidad (Casos/Controles, Cohortes)
tm_apply_cc <- function(n_uncorrected, k, p1, p2) {
    if (is.infinite(n_uncorrected) || n_uncorrected <= 0) {
        return(n_uncorrected)
    }
    num <- 2 * (k + 1)
    den <- n_uncorrected * k * abs(p1 - p2)
    if (den == 0) {
        return(Inf)
    }
    term <- 1 + sqrt(1 + num / den)
    return((n_uncorrected / 4) * term^2)
}

# Varianza del AUC (Hanley & McNeil) para ROC
tm_v_auc_comp <- function(A, k) {
    Q1 <- A / (2 - A)
    Q2 <- 2 * A^2 / (1 + A)
    v <- ((Q1 - A^2) + k * (Q2 - A^2)) / k
    max(0, v)
}
