library(shiny)

# Función para generar el gráfico de Fagan
# Modified to accept 'tr' (translation list) instead of 'lang' for better decoupling
fagan.plot <- function(probs.pre.test, LR, test.result = "+", tr) {
  t <- tr

  stato <- ifelse(test.result == "+", t$disease, t$no_disease)
  logits <- function(p) log(p / (1 - p))
  logits.pre <- logits(probs.pre.test)
  logits.post <- log(LR) + logits.pre
  probs.post.test <- exp(logits.post) / (1 + exp(logits.post))
  compl.logit.pre <- logits(1 - probs.pre.test)

  # Secuencia ajustada de LR.vec con más valores pequeños
  LR.vec <- c(
    0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2,
    0.5, 1, 2, 5, 10, 20, 50, 100
  )

  prob.vec <- c(
    0.001, 0.002, 0.003, 0.005, 0.007, 0.01, 0.02,
    0.03, 0.05, 0.07, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7,
    0.8, 0.9, 0.93, 0.95, 0.97, 0.98, 0.99, 0.993, 0.995,
    0.997, 0.998, 0.999
  )

  # Ajustamos el gráfico para que las escalas estén más separadas
  plot(0, 0,
    type = "n", ylim = range(logits(prob.vec)), axes = FALSE,
    xlab = "", ylab = "", xlim = c(-1, 1)
  )

  # Ejes (Mismo código, solo cambia texto)
  axis(2, rev(logits(prob.vec)), 100 * prob.vec, pos = -1, las = 1, cex.axis = 1.2, tick = FALSE)
  axis(2, rev(logits(prob.vec)), 100 * prob.vec, pos = -1, tck = 0.07, labels = FALSE)

  axis(4, logits(prob.vec), 100 * prob.vec, pos = 1, las = 1, cex.axis = 1.2, tick = FALSE)
  axis(4, logits(prob.vec), 100 * prob.vec, pos = 1, tck = 0.07, labels = FALSE)

  axis(2, log(LR.vec[1:10]) / 2, LR.vec[1:10], pos = 0, las = 1, cex.axis = 1.2, tick = FALSE)
  axis(2, log(LR.vec[1:10]) / 2, LR.vec[1:10], pos = 0, tck = 0.07, labels = FALSE)

  axis(4, log(LR.vec[10:16]) / 2, LR.vec[10:16], pos = 0, las = 1, cex.axis = 1.2, tick = FALSE)
  axis(4, log(LR.vec[10:16]) / 2, LR.vec[10:16], pos = 0, tck = 0.07, labels = FALSE)

  # Etiquetas y líneas de la gráfica
  text(0, 4.5, t$x_label, cex = 2)
  segments(-1, compl.logit.pre, 1, logits.post, lwd = 1.5, col = 2)

  mtext(side = 2, text = t$y_left, line = 2, cex = 1.5)
  mtext(side = 4, text = t$y_right, line = 2, cex = 1.5, las = 3)
  title(main = t$title, cex.main = 1.5)

  final_prob <- ifelse(test.result == "+", round(100 * probs.post.test, 2), round(100 * (1 - probs.post.test), 2))

  text(0, -6.3, paste(
    t$txt_pre, round(100 * probs.pre.test, 2), "% \n",
    t$txt_lr, "=", round(LR, 2), "\n",
    t$txt_post, stato, "=", final_prob, "%"
  ),
  cex = 1
  )
}

# --- Modulo UI ---
fagan_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("title_ui")),

    # Organiza los controles en una fila
    fluidRow(
      column(
        4,
        sliderInput(ns("probs.pre.test"), "Probabilidad pretest de enfermedad:",
          min = 0, max = 1, value = 0.5, step = 0.001
        ),

        # Control deslizante con valores más pequeños y visualmente más pequeño
        sliderInput(ns("LR"), "Cociente de probabilidad (Likelihood Ratio):",
          min = 0.001, max = 100, value = 1, step = 0.001, width = "150px",
          animate = TRUE
        ),
        radioButtons(ns("test.result"), "Resultado de la prueba:",
          choices = c("Positivo" = "+", "Negativo" = "-"), selected = "+"
        )
      )
    ),

    # La zona de la gráfica ocupa el resto del espacio disponible
    fluidRow(
      column(12, plotOutput(ns("faganPlot"), height = "600px"))
    ),
    # Print Template (hidden on screen via CSS)
    div(
      class = "print-template",
      uiOutput(ns("print_view"))
    )
  )
}

# --- Modulo Server ---
fagan_Server <- function(id, lang) {
  moduleServer(id, function(input, output, session) {
    output$title_ui <- renderUI({
      tr <- get_translations(lang(), "nomograma_fagan")
      h3(tr$title)
    })

    # Observe Language Change for Inputs
    observeEvent(lang(), {
      tr <- get_translations(lang(), "nomograma_fagan")

      updateSliderInput(session, "probs.pre.test", label = tr$lbl_pre)
      updateSliderInput(session, "LR", label = tr$lbl_lr)

      # Update Radio Buttons (keeping selection)
      current_sel <- input$test.result
      choices_new <- c("+" = "+", "-" = "-")
      names(choices_new) <- c(tr$res_pos, tr$res_neg)
      updateRadioButtons(session, "test.result",
        label = tr$lbl_res,
        choices = choices_new, selected = current_sel
      )
    })

    output$faganPlot <- renderPlot({
      tr <- get_translations(lang(), "nomograma_fagan")
      fagan.plot(input$probs.pre.test, input$LR, input$test.result, tr)
    })

    # ========= Print View Generator =========
    output$print_view <- renderUI({
      ns <- session$ns
      tr <- get_translations(lang(), "nomograma_fagan")

      # Compute post-test probability
      pre <- input$probs.pre.test
      lr <- input$LR
      test_res <- input$test.result
      logits_pre <- log(pre / (1 - pre))
      logits_post <- log(lr) + logits_pre
      post <- exp(logits_post) / (1 + exp(logits_post))
      stato <- ifelse(test_res == "+", tr$disease, tr$no_disease)
      final_prob <- ifelse(test_res == "+", round(100 * post, 2), round(100 * (1 - post), 2))

      tagList(
        div(
          class = "printable-section",
          h3(tr$title)
        ),
        div(
          class = "printable-section",
          h4(tr$lbl_pre),
          p(paste0(round(100 * pre, 2), "%"))
        ),
        div(
          class = "printable-section",
          h4(tr$lbl_lr),
          p(round(lr, 4))
        ),
        div(
          class = "printable-section",
          h4(tr$lbl_res),
          p(ifelse(test_res == "+", tr$res_pos, tr$res_neg))
        ),
        div(
          class = "printable-section",
          h4(paste(tr$txt_post, stato)),
          p(tags$b(paste0(final_prob, "%")))
        )
      )
    })

    # Force Rendering of Hidden Print Outputs
    outputOptions(output, "print_view", suspendWhenHidden = FALSE)
  })
}
