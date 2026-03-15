library(shiny)
library(bslib)
library(shinythemes)
library(ggplot2)
library(DT)
library(epiR)
library(httr)
library(jsonlite)

# Source i18n utils (must be early for modules)
if (file.exists("R/i18n_utils.R")) source("R/i18n_utils.R")

# Source all 14 modules
source("apps/Calculadora de OR_RR confusion_interaccion/app.R")
source("apps/Calculo_de_FAP_en_estudios_multivariable/app.R")
source("apps/Calculadora_confusion_interaccion/app.R")
source("apps/Cohortes_emperajados_Shiny/app.R")
source("apps/cohortes_politomico_Shiny/app.R")
source("apps/Cohortes_persona_tiempo_politomica_Shiny/app.R")
source("apps/Estudios Casos_Controles emparejdos_Shiny/app.R")
source("apps/calculadora_confusion_interaccion_graficos_dic/app.R")
source("apps/Casos_controles_estrat_Shiny/app.R")
source("apps/Cohorte_estrat_persona_tiempo_Shiny/app.R")
source("apps/Trasversal estratificado/app.R")
source("apps/cH_estrat_shiny/app.R")
source("apps/Cohortes_1/app.R")
source("apps/Cohortes_2/app.R")
source("apps/Cohortes_3/app.R")
source("apps/Casos_Controles_1/app.R")
source("apps/Casos_Controles_2/app.R")
source("apps/Estudios_Transversales/app.R")
source("apps/Otros_Calculos_NNT/app.R")
# source("apps/Estudios_Ecologicos/app.R")
source("apps/Ecologicos_Observacionales/app.R")
source("apps/Ecolologico_Shny_2/app.R")
source("apps/Ecologicos_Agregados/app.R")
source("apps/Ecologicos_Multivariable/app.R")

# Define UI
ui <- page_navbar(
  title = "",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  fillable = FALSE,
  fluid = TRUE,

  # Include Custom CSS, Flag Icons and External Scripts
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css?v=1.3"),
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/gh/lipis/flag-icons@7.2.3/css/flag-icons.min.css"),

    # Chat Widget (RAG Assistant - n8n integration)
    tags$script(src = "chat-widget.js?v=1.3"),

    # Script de UI e Impresión extraído a un archivo JS independiente
    tags$script(src = "print-script.js"),
    tags$style(HTML("
      /* Page Break Visualization in Preview */
      .page-break-marker {
        border-top: 2px dashed #e74c3c;
        margin: 20px 0;
        position: relative;
        text-align: center;
        color: #e74c3c;
        font-size: 0.8rem;
        pointer-events: none; /* Let clicks pass through */
      }
      .page-break-marker::after {
        content: 'Salto de Página Estimado / Page Break';
        background: #fffbe6;
        padding: 0 10px;
        position: relative;
        top: -10px;
      }
      @media print {
        .page-break-marker { display: none; } /* Hide marker in actual print */
        .printable-section { break-inside: avoid; } /* Avoid breaking inside sections */
      }
    "))
  ),
  sidebar = sidebar(
    div(
      class = "custom-lang-selector",
      radioButtons("lang",
        label = uiOutput("lbl_lang_selection", inline = TRUE),
        choiceNames = list(
          HTML("<div class='lang-item'><span class='lang-flag fi fi-es'></span> <span class='lang-text'>Español</span></div>"),
          HTML("<div class='lang-item'><span class='lang-flag fi fi-gb'></span> <span class='lang-text'>English</span></div>"),
          HTML("<div class='lang-item'><span class='lang-flag fi fi-pt'></span> <span class='lang-text'>Português</span></div>"),
          HTML("<div class='lang-item'><span class='lang-flag fi fi-cn'></span> <span class='lang-text'>简体中文</span></div>")
        ),
        choiceValues = c("es", "en", "pt", "zh"),
        selected = "es"
      )
    ),
    hr(),
    div(class = "theme-label", uiOutput("lbl_theme", inline = TRUE)),
    uiOutput("theme_toggle_ui"),
    hr(),
    div(
      class = "print-section",
      div(class = "theme-label", uiOutput("lbl_print", inline = TRUE)),
      actionButton("print_results",
        label = HTML("<span style='font-size: 1.5rem;'>🖨️</span> <span id='print_btn_text'>Imprimir</span>"),
        class = "btn print-btn disabled",
        disabled = "disabled",
        style = "width: 100%; padding: 10px; margin-top: 5px;"
      )
    )
  ),
  nav_panel(
    title = span(id = "home_tab_label", "Inicio"),
    uiOutput("home_content"),
    icon = icon("home")
  ),
    nav_panel(
      title = span(id = "sub_ec2", "Ensayos Clínicos 2 (Datos agregados)"),
      ensayos_clinicos_2_UI("ec2_module")
    ),
    nav_panel(
      title = span(id = "sub_nnt_pt", "NNT en ECAs Personas-Tiempo"),
      nnt_ec_pt_UI("nnt_pt_module")
    )
  ),
  nav_menu(
    title = span(id = "menu_ci", "Confusión e Interacción"),
    nav_panel(
      title = span(id = "sub_or_rr", "OR/RR Confusión Interacción"),
      or_rr_UI("or_rr_module")
    ),
    nav_panel(
      title = span(id = "sub_ci_complex", "Confusión e Interacción (Complejo)"),
      confusion_interaccion_UI("ci_complex_module")
    ),
    nav_panel(
      title = span(id = "sub_ci_graph", "Confusión Interacción (Gráficos)"),
      conf_inter_graphics_UI("conf_inter_gr_module")
    )
  ),
    nav_panel(
      title = span(id = "sub_diag1", "Pruebas Diagnósticas 1 (Tabla 2x2)"),
      pruebas_diagnosticas_1_UI("diag_1_module")
    ),
    nav_panel(
      title = span(id = "sub_diag2", "Pruebas Diagnósticas 2 (Datos agregados)"),
      pruebas_diagnosticas_2_UI("diag_2_module")
    )
  ),
  nav_menu(
    title = span(id = "menu_coh", "Estudios de Cohortes"),
    nav_panel(
      title = span(id = "sub_fap", "FAP Multivariable"),
      fap_multivariable_UI("fap_module")
    ),
    nav_panel(
      title = span(id = "sub_coh_emp", "Cohortes Emparejados"),
      cohortes_emparejados_UI("cohortes_emp_module")
    ),
    nav_panel(
      title = span(id = "sub_coh1", "Cohortes 1 (Tabla 2x2)"),
      cohortes_1_UI("coh_1_module")
    ),
    nav_panel(
      title = span(id = "sub_coh2", "Cohortes 2 (Datos agregados)"),
      cohortes_2_UI("coh_2_module")
    ),
    nav_panel(
      title = span(id = "sub_coh3", "Cohortes 3 (Población-tiempo)"),
      cohortes_3_UI("coh_3_module")
    ),
    nav_panel(
      title = span(id = "sub_coh_poly", "Riesgo Cohorte Politomica"),
      cohort_polytomous_risk_UI("cohort_poly_risk_module")
    ),
    nav_panel(
      title = span(id = "sub_coh_trend", "Tendencias Cohortes (Persona-Tiempo)"),
      cohortes_politomica_UI("cohortes_pol_mod_pt")
    ),
    nav_panel(
      title = span(id = "sub_coh_strat_pt", "Cohorte Estrat. Persona-Tiempo"),
      cohorte_estrat_pt_UI("cohorte_pt_module")
    ),
    nav_panel(
      title = span(id = "sub_coh_strat_cnt", "Cohorte Estratificada (Conteo)"),
      cohort_strat_count_UI("cohort_strat_cnt_module")
    )
  ),
  nav_menu(
    title = span(id = "menu_cc", "Casos y Controles"),
    nav_panel(
      title = span(id = "sub_cc1", "Casos y Controles 1 (Tabla 2x2)"),
      casos_controles_1_UI("cc_1_module")
    ),
    nav_panel(
      title = span(id = "sub_cc2", "Casos y Controles 2 (Datos agregados)"),
      casos_controles_2_UI("cc_2_module")
    ),
    nav_panel(
      title = span(id = "sub_cc_emp", "Casos y Controles Emparejados"),
      cc_emparejados_UI("cc_emp_module")
    ),
    nav_panel(
      title = span(id = "sub_cc_strat", "Casos y Controles Estratificados"),
      casos_controles_estrat_UI("cc_estrat_module")
    )
  ),
  nav_menu(
    title = span(id = "menu_trans", "Estudios Transversales"),
    nav_panel(
      title = span(id = "sub_trans_gen", "Estudios Transversales (General)"),
      estudios_transversales_UI("transversal_ui")
    ),
    nav_panel(
      title = span(id = "sub_trans_strat", "Transversal Estratificado"),
      cross_sectional_UI("cross_sec_module")
    )
  ),
  nav_menu(
    title = span(id = "menu_ecol", "Estudios Ecológicos"),
    nav_panel(
      title = span(id = "sub_ecol_obs", "Ecológicos observacionales (Tasas)"),
      ecologicos_observacionales_UI("ecol_obs_module")
    ),
    nav_panel(
      title = span(id = "sub_ecol_other", "Otros modelos ecológicos"),
      ecologico_UI("ecol_other_module")
    ),
    nav_panel(
      title = span(id = "sub_ecol_agg", "Ecológicos agregados"),
      ecologicos_agregados_UI("ecol_agg_module")
    ),
    nav_panel(
      title = span(id = "sub_ecol_mult", "Ecológicos multivariable"),
      ecologicos_multivariable_UI("ecol_mult_module")
    )
  ),
  nav_menu(
    title = span(id = "menu_other", "Otros"),
    nav_panel(
      title = span(id = "sub_nnt", "Otros Cálculos (NNT)"),
      otros_calculos_nnt_UI("nnt_module")
    )
  ),
    nav_panel(
      title = span(id = "sub_kappa", "Concordancia Kappa"),
      kappa_UI("kappa_module")
    ),
    nav_panel(
      title = span(id = "sub_cci_multi", "CCI Múltiples Observadores"),
      cci_multi_UI("cci_multi_module")
    )
  )
)

server <- function(input, output, session) {
  lang <- reactive({
    input$lang
  })

  # Proxy para el asistente Metodix (RAG)
  observeEvent(input$metodix_query, {
    req_data <- input$metodix_query

    # Endpoint de n8n (Producción)
    url <- "https://n8n.cienciasinseso.com/webhook/chat"

    tryCatch(
      {
        resp <- httr::POST(
          url = url,
          body = req_data,
          encode = "json",
          httr::timeout(30)
        )

        if (httr::status_code(resp) == 200) {
          content <- httr::content(resp, as = "parsed", simplifyVector = TRUE)
          # Búsqueda robusta del campo de respuesta
          answer <- content$output
          if (is.null(answer)) answer <- content$text
          if (is.null(answer)) answer <- content$answer
          if (is.null(answer)) answer <- content$response
          if (is.null(answer)) answer <- content$message
          if (is.null(answer)) answer <- "No output from assistant"

          session$sendCustomMessage("metodix_response", list(
            success = TRUE,
            output = answer,
            sources = content$sources
          ))
        } else {
          session$sendCustomMessage("metodix_response", list(success = FALSE, error = paste("HTTP", httr::status_code(resp))))
        }
      },
      error = function(e) {
        session$sendCustomMessage("metodix_response", list(success = FALSE, error = e$message))
      }
    )
  })

  # Home Tab Title and Navigation Translations

  nav_tr <- list(
    es = list(
      home = "Inicio",
      lbl_lang_selection = "Seleccione Idioma",
      lbl_theme = "Tema",
      lbl_print = "Imprimir resultados",
      print_btn = "Imprimir",
      print_title = "Calcupedev - Imprimir resultados",
      print_header = "Calculadora epidemiológica",
      print_preview = "Vista Previa de Impresión",
      menu_stat = "Calculadoras estadísticas",
      menu_stat_placeholder = "Intervalos de confianza para la mediana",
      sub_kappa = "Concordancia Kappa",
      sub_cci_multi = "CCI Múltiples Observadores",
      stat_coming_soon = "Próximamente",
      menu_ec = "Ensayos Clínicos",
      sub_ec1 = "Ensayos Clínicos 1 (Tabla 2x2)",
      sub_ec2 = "Ensayos Clínicos 2 (Datos agregados)",
      sub_nnt_pt = "NNT en ECAs Personas-Tiempo",
      menu_ci = "Confusión e Interacción",
      sub_or_rr = "OR/RR Confusión Interacción",
      sub_ci_complex = "Confusión e Interacción (Complejo)",
      sub_ci_graph = "Confusión Interacción (Gráficos)",
      menu_diag = "Pruebas Diagnósticas",
      sub_fagan = "Nomograma de Fagan",
      sub_diag1 = "Pruebas Diagnósticas 1 (Tabla 2x2)",
      sub_diag2 = "Pruebas Diagnósticas 2 (Datos agregados)",
      menu_coh = "Estudios de Cohortes",
      sub_fap = "FAP Multivariable",
      sub_coh_emp = "Cohortes Emparejados",
      sub_coh1 = "Cohortes 1 (Tabla 2x2)",
      sub_coh2 = "Cohortes 2 (Datos agregados)",
      sub_coh3 = "Cohortes 3 (Población-tiempo)",
      sub_coh_poly = "Riesgo Cohorte Politomica",
      sub_coh_trend = "Tendencias Cohortes (Persona-Tiempo)",
      sub_coh_strat_pt = "Cohorte Estrat. Persona-Tiempo",
      sub_coh_strat_cnt = "Cohorte Estratificada (Conteo)",
      menu_cc = "Casos y Controles",
      sub_cc1 = "Casos y Controles 1 (Tabla 2x2)",
      sub_cc2 = "Casos y Controles 2 (Datos agregados)",
      sub_cc_emp = "Casos y Controles Emparejados",
      sub_cc_strat = "Casos y Controles Estratificados",
      menu_trans = "Estudios Transversales",
      sub_trans_gen = "Estudios Transversales (General)",
      sub_trans_strat = "Transversal Estratificado",
      menu_ecol = "Estudios Ecológicos",
      sub_ecol_obs = "Ecológicos observacionales (Tasas)",
      sub_ecol_other = "Otros modelos ecológicos",
      sub_ecol_agg = "Ecológicos agregados",
      sub_ecol_mult = "Ecológicos multivariable",
      menu_other = "Otros",
      sub_nnt = "Otros Cálculos (NNT)",
      menu_sample = "Tamaño Muestral",
      sub_sample_placeholder = "Próximamente",
      tip_to_dark = "Cambiar a modo oscuro",
      tip_to_light = "Cambiar a modo claro",
      print_btn_close = "Cerrar",
      print_btn_print = "Imprimir"
    ),
    en = list(
      home = "Home",
      lbl_lang_selection = "Select Language",
      lbl_theme = "Theme",
      menu_stat = "Statistical Calculators",
      menu_stat_placeholder = "Confidence Intervals for the Median",
      sub_kappa = "Kappa Concordance",
      sub_cci_multi = "ICC Multiple Observers",
      stat_coming_soon = "Coming Soon",
      lbl_print = "Print Results",
      print_btn = "Print",
      print_title = "Calcupedev - Print Results",
      print_header = "Epidemiological Calculator",
      print_preview = "Print Preview",
      menu_ec = "Clinical Trials",
      sub_ec1 = "Clinical Trials 1 (2x2 Table)",
      sub_ec2 = "Clinical Trials 2 (Aggregated Data)",
      sub_nnt_pt = "NNT in Person-Time RCTs",
      menu_ci = "Confounding and Interaction",
      sub_or_rr = "OR/RR Confounding Interaction",
      sub_ci_complex = "Confounding and Interaction (Complex)",
      sub_ci_graph = "Confounding Interaction (Graphics)",
      menu_diag = "Diagnostic Tests",
      sub_fagan = "Fagan Nomogram",
      sub_diag1 = "Diagnostic Tests 1 (2x2 Table)",
      sub_diag2 = "Diagnostic Tests 2 (Aggregated Data)",
      menu_coh = "Cohort Studies",
      sub_fap = "Multivariate PAF",
      sub_coh_emp = "Matched Cohorts",
      sub_coh1 = "Cohorts 1 (2x2 Table)",
      sub_coh2 = "Cohorts 2 (Aggregated Data)",
      sub_coh3 = "Cohorts 3 (Person-time)",
      sub_coh_poly = "Polytomous Cohort Risk",
      sub_coh_trend = "Cohort Trends (Person-Time)",
      sub_coh_strat_pt = "Stratified Person-Time Cohort",
      sub_coh_strat_cnt = "Stratified Cohort (Count)",
      menu_cc = "Case-Control",
      sub_cc1 = "Case-Control 1 (2x2 Table)",
      sub_cc2 = "Case-Control 2 (Aggregated Data)",
      sub_cc_emp = "Matched Case-Control",
      sub_cc_strat = "Stratified Case-Control",
      menu_trans = "Cross-sectional Studies",
      sub_trans_gen = "Cross-sectional (General)",
      sub_trans_strat = "Stratified Cross-sectional",
      menu_ecol = "Ecological Studies",
      sub_ecol_obs = "Ecological Observational (Rates)",
      sub_ecol_other = "Other ecological models",
      sub_ecol_agg = "Aggregated ecological",
      sub_ecol_mult = "Multivariable ecological",
      menu_other = "Other",
      sub_nnt = "Other Counts (NNT)",
      menu_sample = "Sample Size",
      sub_sample_placeholder = "Coming Soon",
      tip_to_dark = "Switch to Dark Mode",
      tip_to_light = "Switch to Light Mode",
      print_btn_close = "Close",
      print_btn_print = "Print"
    ),
    pt = list(
      home = "Início",
      lbl_lang_selection = "Selecione o Idioma",
      lbl_theme = "Tema",
      menu_stat = "Calculadoras estatísticas",
      menu_stat_placeholder = "Intervalos de confiança para a mediana",
      sub_kappa = "Concordância Kappa",
      sub_cci_multi = "ICC Múltiplas Observações",
      stat_coming_soon = "Em breve",
      lbl_print = "Imprimir resultados",
      print_btn = "Imprimir",
      print_title = "Calcupedev - Imprimir resultados",
      print_header = "Calculadora epidemiológica",
      print_preview = "Visualização de impressão",
      menu_ec = "Ensaios Clínicos",
      sub_ec1 = "Ensaios Clínicos 1 (Tabela 2x2)",
      sub_ec2 = "Ensaios Clínicos 2 (Dados agregados)",
      sub_nnt_pt = "NNT em ECAs Pessoa-Tempo",
      menu_ci = "Confusão e Interação",
      sub_or_rr = "OR/RR Confusão Interação",
      sub_ci_complex = "Confusão e Interação (Complexo)",
      sub_ci_graph = "Confusão Interação (Gráficos)",
      menu_diag = "Testes Diagnósticos",
      sub_fagan = "Nomograma de Fagan",
      sub_diag1 = "Testes Diagnósticos 1 (Tabela 2x2)",
      sub_diag2 = "Testes Diagnósticos 2 (Dados agregados)",
      menu_coh = "Estudos de Coorte",
      sub_fap = "FAP Multivariada",
      sub_coh_emp = "Coortes Emparelhadas",
      sub_coh1 = "Coortes 1 (Tabela 2x2)",
      sub_coh2 = "Coortes 2 (Dados agregados)",
      sub_coh3 = "Coortes 3 (Pessoa-tempo)",
      sub_coh_poly = "Risco de Coorte Politômica",
      sub_coh_trend = "Tendências de Coorte (Pessoa-Tempo)",
      sub_coh_strat_pt = "Coorte Estrat. Pessoa-Tempo",
      sub_coh_strat_cnt = "Coorte Estratificada (Contagem)",
      menu_cc = "Casos e Controles",
      sub_cc1 = "Casos e Controles 1 (Tabela 2x2)",
      sub_cc2 = "Casos e Controles 2 (Dados agregados)",
      sub_cc_emp = "Casos e Controles Emparelhados",
      sub_cc_strat = "Casos e Controles Estratificados",
      menu_trans = "Estudos Transversais",
      sub_trans_gen = "Estudos Transversais (Geral)",
      sub_trans_strat = "Transversal Estratificado",
      menu_ecol = "Estudos Ecológicos",
      sub_ecol_obs = "Ecológicos observacionais (Taxas)",
      sub_ecol_other = "Outros modelos ecológicos",
      sub_ecol_agg = "Ecológicos agregados",
      sub_ecol_mult = "Ecológicos multivariados",
      menu_other = "Outros",
      sub_nnt = "Outros Cálculos (NNT)",
      menu_sample = "Tamanho da Amostra",
      sub_sample_placeholder = "Em breve",
      tip_to_dark = "Mudar para modo escuro",
      tip_to_light = "Mudar para modo claro",
      print_btn_close = "Fechar",
      print_btn_print = "Imprimir"
    ),
    zh = list(
      home = "首页",
      lbl_lang_selection = "选择语言",
      lbl_theme = "主题",
      menu_stat = "统计计算器",
      menu_stat_placeholder = "中位数置信区间",
      sub_kappa = "Kappa 一致性",
      sub_cci_multi = "组内相关系数 (多重观察者)",
      stat_coming_soon = "即将推出",
      lbl_print = "打印结果",
      print_btn = "打印",
      print_title = "Calcupedev - 打印结果",
      print_header = "流行病学计算器",
      print_preview = "打印预览",
      menu_ec = "临床试验",
      sub_ec1 = "临床试验 1 (2x2 表)",
      sub_ec2 = "临床试验 2 (汇总数据)",
      sub_nnt_pt = "人年 RCT 中的 NNT",
      menu_ci = "混杂与交互作用",
      sub_or_rr = "OR/RR 混杂交互作用",
      sub_ci_complex = "混杂与交互作用 (复杂型)",
      sub_ci_graph = "混杂交互作用 (图表)",
      menu_diag = "诊断试验",
      sub_fagan = "Fagan 诺谟图",
      sub_diag1 = "诊断试验 1 (2x2 表)",
      sub_diag2 = "诊断试验 2 (汇总数据)",
      menu_coh = "队列研究",
      sub_fap = "多变量 FAP",
      sub_coh_emp = "配对队列研究",
      sub_coh1 = "队列研究 1 (2x2 表)",
      sub_coh2 = "队列研究 2 (汇总数据)",
      sub_coh3 = "队列研究 3 (人年)",
      sub_coh_poly = "多分类队列风险",
      sub_coh_trend = "队列趋势 (人年)",
      sub_coh_strat_pt = "分层人年队列",
      sub_coh_strat_cnt = "分层队列 (计数)",
      menu_cc = "病例对照研究",
      sub_cc1 = "病例对照研究 1 (2x2 表)",
      sub_cc2 = "病例对照研究 2 (汇总数据)",
      sub_cc_emp = "配对病例对照研究",
      sub_cc_strat = "分层病例对照研究",
      menu_trans = "横断面研究",
      sub_trans_gen = "横断面研究 (通用)",
      sub_trans_strat = "分层横断面研究",
      menu_ecol = "生态学研究",
      sub_ecol_obs = "生态学观察 (率)",
      sub_ecol_other = "其他生态模型",
      sub_ecol_agg = "汇总生态学",
      sub_ecol_mult = "多变量生态学",
      menu_other = "其他",
      sub_nnt = "其他计算 (NNT)",
      menu_sample = "样本量",
      sub_sample_placeholder = "即将推出",
      tip_to_dark = "切换到深色模式",
      tip_to_light = "切换到浅色模式",
      print_btn_close = "关闭",
      print_btn_print = "打印"
    )
  )

  output$lbl_lang_selection <- renderUI({
    nav_tr[[lang()]]$lbl_lang_selection
  })

  output$lbl_theme <- renderUI({
    nav_tr[[lang()]]$lbl_theme
  })

  output$lbl_print <- renderUI({
    nav_tr[[lang()]]$lbl_print
  })

  output$home_tab_label <- NULL # Removed

  observeEvent(input$lang, {
    t <- nav_tr[[lang()]]
    session$sendCustomMessage("updateNavTitles", t)
    # Also update print button text and title
    session$sendCustomMessage("updatePrintButton", t$print_btn)
    session$sendCustomMessage("updatePrintTitle", t$print_title)
    session$sendCustomMessage("updatePrintHeader", t$print_header)
    session$sendCustomMessage("updatePrintPreviewTitle", t$print_preview)
    session$sendCustomMessage("updatePrintBtnClose", t$print_btn_close)
    session$sendCustomMessage("updatePrintBtnPrint", t$print_btn_print)
    # Sync language with chat widget
    session$sendCustomMessage("updateChatLanguage", lang())
  })

  # Custom Theme Toggle
  current_theme <- reactiveVal("light")

  output$theme_toggle_ui <- renderUI({
    theme <- current_theme()
    t <- nav_tr[[lang()]]

    if (theme == "light") {
      btn_label <- HTML("<span style='font-size: 1.8rem;'>☀️</span> <span style='font-size: 1.5rem; margin: 0 8px; color: #2c3e50;'>→</span> <span style='font-size: 1.8rem;'>🌙</span>")
      btn_title <- t$tip_to_dark
    } else {
      btn_label <- HTML("<span style='font-size: 1.8rem;'>🌙</span> <span style='font-size: 1.5rem; margin: 0 8px; color: #ecf0f1;'>→</span> <span style='font-size: 1.8rem;'>☀️</span>")
      btn_title <- t$tip_to_light
    }

    actionButton("toggle_theme",
      label = btn_label, title = btn_title,
      class = "btn", style = "width: 100%; padding: 10px; background-color: transparent; border: none; cursor: pointer;"
    )
  })

  observeEvent(input$toggle_theme, {
    new_theme <- if (current_theme() == "light") "dark" else "light"
    current_theme(new_theme)
    # Send JS to update the document theme attribute
    session$sendCustomMessage("setTheme", new_theme)
  })

  output$home_content <- renderUI({
    if (lang() == "es") {
      div(
        class = "home-container",
        div(
          class = "home-welcome-text",
          HTML("Bienvenido/a a <strong>Calcupedev</strong>, la herramienta de cálculo epidemiológico en pediatría.")
        ),
        div(class = "home-logo-text", "EVIDENCIAS EN PEDIATRÍA"),
        div(
          class = "home-footer",
          p("Calcupedev v11. Notas de versión."),
          p("Autor: Eduardo Ortega Páez. Pediatra. Comité de Pediatría Basada en la Evidencia. AEP."),
          p("Con el aval del Comité de Pediatría Basada en la Evidencia de la AEP y de la AEPAP."),
          p("Para comunicar cualquier tipo de error o sugerencia le agradeceríamos que escribiera a gatedu@gmail.com"),
          p("Si utiliza la calculadora para publicar un trabajo científico, rogamos sea citada como: \"Calcupedev. Herramienta de cálculo epidemiológico en pediatría. E. Ortega Páez. Comité de Pediatría Basada en la Evidencia de la AEP. 2019."),
          p("El autor no se responsabiliza del uso que pueda darse a la información derivada de esta aplicación. Se ha esforzado en que la información detallada en esta página sea lo más precisa y actualizada posible, pero dado los cambios constantes de la epidemiología podría ser que no sea completa o contenga algún error, por ello invitamos al usuario que los utilice con sentido crítico y compare los resultados.")
        ),
        div(
          class = "home-links",
          tags$a(href = "https://www.evidenciasenpediatria.es", "EVIDENCIAS EN PEDIATRÍA", target = "_blank", style = "color: white; text-decoration: underline; margin-right: 5px;"), " | ",
          actionLink("link_legal_es", "NOTA LEGAL", style = "color: white; text-decoration: underline; text-transform: uppercase; margin: 0 5px;"), " | ",
          span(id = "metodix-trigger", "Metodix, tu asistente inteligente", style = "cursor: pointer; text-decoration: underline;")
        )
      )
    } else if (lang() == "pt") {
      div(
        class = "home-container",
        div(
          class = "home-welcome-text",
          HTML("Bem-vindo(a) ao <strong>Calcupedev</strong>, a ferramenta de cálculo epidemiológico em pediatria.")
        ),
        div(class = "home-logo-text", "EVIDENCIAS EN PEDIATRÍA"),
        div(
          class = "home-footer",
          p("Calcupedev v11. Notas de versão."),
          p("Autor: Eduardo Ortega Páez. Pediatra. Comitê de Pediatria Baseada em Evidências. AEP."),
          p("Com o aval do Comitê de Pediatria Baseada em Evidências da AEP e da AEPAP."),
          p("Para comunicar qualquer tipo de erro ou sugestão, por favor escreva para gatedu@gmail.com"),
          p("Se utilizar a calculadora para publicar um trabalho científico, solicitamos que seja citada como: \"Calcupedev. Ferramenta de cálculo epidemiológico em pediatria. E. Ortega Páez. Comitê de Pediatria Baseada em Evidências da AEP. 2019."),
          p("O autor não se responsabiliza pelo uso que possa ser dado às informações derivadas desta aplicação. Esforçou-se para que a informação detalhada nesta página seja o mais precisa e atualizada possível, mas dadas as constantes mudanças na epidemiologia, pode ser que não esteja completa ou contenha algum erro, por isso convidamos o usuário a utilizá-la com sentido crítico e comparar os resultados.")
        ),
        div(
          class = "home-links",
          tags$a(href = "https://www.evidenciasenpediatria.es", "EVIDENCIAS EN PEDIATRÍA", target = "_blank", style = "color: white; text-decoration: underline; margin-right: 5px;"), " | ",
          actionLink("link_legal_pt", "NOTA LEGAL", style = "color: white; text-decoration: underline; text-transform: uppercase; margin: 0 5px;"), " | ",
          span(id = "metodix-trigger-pt", "Metodix, seu assistente inteligente", style = "cursor: pointer; text-decoration: underline;")
        )
      )
    } else if (lang() == "zh") {
      div(
        class = "home-container",
        div(
          class = "home-welcome-text",
          HTML("欢迎使用 <strong>Calcupedev</strong>，儿科流行病学计算工具。")
        ),
        div(class = "home-logo-text", "EVIDENCIAS EN PEDIATRÍA"),
        div(
          class = "home-footer",
          p("Calcupedev v11. 版本说明。"),
          p("作者：Eduardo Ortega Páez。儿科医生。AEP 循证儿科学委员会。"),
          p("得到 AEP 和 AEPAP 循证儿科学委员会的认可。"),
          p("如需报告任何错误或建议，请发送至 gatedu@gmail.com"),
          p("如果您在发表科研论文时使用此计算器，请引用为：\"Calcupedev. Tool for epidemiological calculation in pediatrics. E. Ortega Páez. Evidence-Based Pediatrics Committee of the AEP. 2019."),
          p("作者不对本应用程序衍生信息的使用负责。页内信息已尽力做到最准确和最新，但鉴于流行病学的不断变化，信息可能不完整或包含误差，因此邀请用户批判性地使用并对比结果。")
        ),
        div(
          class = "home-links",
          tags$a(href = "https://www.evidenciasenpediatria.es", "EVIDENCIAS EN PEDIATRÍA", target = "_blank", style = "color: white; text-decoration: underline; margin-right: 5px;"), " | ",
          actionLink("link_legal_zh", "法律声明", style = "color: white; text-decoration: underline; text-transform: uppercase; margin: 0 5px;"), " | ",
          span(id = "metodix-trigger-zh", "Metodix, 您的智能助手", style = "cursor: pointer; text-decoration: underline;")
        )
      )
    } else {
      div(
        class = "home-container",
        div(
          class = "home-welcome-text",
          HTML("Welcome to <strong>Calcupedev</strong>, the epidemiological calculation tool in pediatrics.")
        ),
        div(class = "home-logo-text", "EVIDENCIAS EN PEDIATRÍA"),
        div(
          class = "home-footer",
          p("Calcupedev v11. Release Notes."),
          p("Author: Eduardo Ortega Páez. Pediatrician. Evidence-Based Pediatrics Committee. AEP."),
          p("Endorsed by the Evidence-Based Pediatrics Committee of the AEP and AEPAP."),
          p("To report any errors or suggestions, please write to gatedu@gmail.com"),
          p("If you use this calculator to publish a scientific paper, please cite it as: \"Calcupedev. Tool for epidemiological calculation in pediatrics. E. Ortega Páez. Evidence-Based Pediatrics Committee of the AEP. 2019."),
          p("The author is not responsible for the use that may be made of the information derived from this application. Efforts have been made to ensure that the information detailed on this page is as accurate and up-to-date as possible, but given the constant changes in epidemiology, it may not be complete or may contain errors, so we invite the user to use it with critical sense and compare the results.")
        ),
        div(
          class = "home-links",
          tags$a(href = "https://www.evidenciasenpediatria.es", "EVIDENCIAS EN PEDIATRÍA", target = "_blank", style = "color: white; text-decoration: underline; margin-right: 5px;"), " | ",
          actionLink("link_legal_en", "LEGAL NOTICE", style = "color: white; text-decoration: underline; text-transform: uppercase; margin: 0 5px;"), " | ",
          span(id = "metodix-trigger-en", "Metodix, your intelligent assistant", style = "cursor: pointer; text-decoration: underline;")
        )
      )
    }
  })

  observeEvent(input$link_legal_es, {
    showModal(modalDialog(
      title = "Nota legal",
      "El autor no se responsabiliza del uso que pueda darse a la información derivada de esta aplicación. Se ha esforzado en que la información detallada en esta página sea lo más precisa y actualizada posible, pero dado los cambios constantes de la epidemiología podría ser que no sea completa o contenga algún error, por ello invitamos al usuario que los utilice con sentido crítico y compare los resultados.",
      easyClose = TRUE,
      footer = modalButton("Cerrar")
    ))
  })

  observeEvent(input$link_legal_pt, {
    showModal(modalDialog(
      title = "Nota legal",
      "O autor não se responsabiliza pelo uso que possa ser dado às informações derivadas desta aplicação. Esforçou-se para que a informação detalhada nesta página seja o mais precisa e atualizada possível, mas dadas as constantes mudanças na epidemiologia, pode ser que não esteja completa ou contenha algum erro, por isso convidamos o usuário a utilizá-la com sentido crítico e comparar os resultados.",
      easyClose = TRUE,
      footer = modalButton("Fechar")
    ))
  })

  observeEvent(input$link_legal_zh, {
    showModal(modalDialog(
      title = "法律声明",
      "作者不对本应用程序衍生信息的使用负责。页内信息已尽力做到最准确和最新，但鉴于流行病学的不断变化，信息可能不完整或包含误差，因此邀请用户批判性地使用并对比结果。",
      easyClose = TRUE,
      footer = modalButton("关闭")
    ))
  })

  observeEvent(input$link_legal_en, {
    showModal(modalDialog(
      title = "Legal notice",
      "The author is not responsible for the use that may be made of the information derived from this application. Efforts have been made to ensure that the information detailed on this page is as accurate and up-to-date as possible, but given the constant changes in epidemiology, it may not be complete or may contain errors, so we invite the user to use it with critical sense and compare the results.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  # All 14 module servers
  or_rr_Server("or_rr_module", lang)
  fap_multivariable_Server("fap_module", lang)
  confusion_interaccion_Server("ci_complex_module", lang)
  cohortes_emparejados_Server("cohortes_emp_module", lang)
  cohort_polytomous_risk_Server("cohort_poly_risk_module", lang)
  cohortes_politomica_Server("cohortes_pol_mod_pt", lang)
  cc_emparejados_Server("cc_emp_module", lang)
  conf_inter_graphics_Server("conf_inter_gr_module", lang)
  casos_controles_estrat_Server("cc_estrat_module", lang)
  cohorte_estrat_pt_Server("cohorte_pt_module", lang)
  cross_sectional_Server("cross_sec_module", lang)
  cohort_strat_count_Server("cohort_strat_cnt_module", lang)
  cohortes_1_Server("coh_1_module", lang)
  cohortes_2_Server("coh_2_module", lang)
  cohortes_3_Server("coh_3_module", lang)
  casos_controles_1_Server("cc_1_module", lang)
  casos_controles_2_Server("cc_2_module", lang)
  estudios_transversales_Server("transversal_ui", lang)
  ecologicos_observacionales_Server("ecol_obs_module", lang)
  ecologico_Server("ecol_other_module", lang)
  ecologicos_agregados_Server("ecol_agg_module", lang)
  ecologicos_multivariable_Server("ecol_mult_module", lang)

  output$menu_nnt_pt_title_ui <- NULL # Removed
  output$menu_mediana_title_ui <- NULL # Removed

  otros_calculos_nnt_Server("nnt_module", lang)
}

shinyApp(ui, server)
