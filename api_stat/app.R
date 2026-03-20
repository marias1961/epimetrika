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
source("apps/Kappa/app.R")
source("apps/CCI_multiples_obs/app.R")
source("apps/IC_Mediana/app.R")

# Define UI
ui <- page_navbar(
  title = "",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  fillable = FALSE,
  fluid = TRUE,

 header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css?v=1.3"),
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/gh/lipis/flag-icons@7.2.3/css/flag-icons.min.css"),
    tags$script(src = "print-script.js?v=1.6"),

    # --- FORZAR MODO OSCURO POR DEFECTO ---
    tags$script(HTML("
      document.documentElement.setAttribute('data-bs-theme', 'dark');
    ")),
    
    # --- SCRIPT RECEPTOR GLOBAL (COMUNICACIÓN CON REACT) ---
    tags$script(HTML("
      $(document).ready(function() {
        window.addEventListener('message', function(event) {
          // Ignorar mensajes que no tengan nuestra estructura estandarizada
          if (!event.data || !event.data.action) return;

          // 1. Cambio de Idioma
          if (event.data.action === 'CHANGE_LANG') {
            Shiny.setInputValue('global_lang', event.data.payload);
          }
          
          // 2. Cambio de Tema
          if (event.data.action === 'CHANGE_THEME') {
            document.documentElement.setAttribute('data-bs-theme', event.data.payload);
            Shiny.setInputValue('global_theme', event.data.payload);
          }

          // 3. Petición de Impresión
          if (event.data.action === 'PRINT_REQUEST') {
            // Llama al sistema de impresión (si tienes una función específica en print-script.js úsala aquí, si no window.print)
            if (typeof window.prepareAndPrint === 'function') {
               window.prepareAndPrint(); 
            } else {
               window.print();
            }
          }
        });

        // Comportamiento de los menús desplegables
        $('.navbar .dropdown').on('mouseleave', function() {
          $(this).removeClass('open show');
          $(this).children('.dropdown-menu').removeClass('show');
          $(this).children('.dropdown-toggle').attr('aria-expanded', 'false');
        });
      });
    ")),
    
    tags$style(HTML("
      .page-break-marker {
        border-top: 2px dashed #e74c3c;
        margin: 20px 0;
        position: relative;
        text-align: center;
        color: #e74c3c;
        font-size: 0.8rem;
        pointer-events: none;
      }
      .page-break-marker::after {
        content: 'Salto de Página Estimado / Page Break';
        background: #fffbe6;
        padding: 0 10px;
        position: relative;
        top: -10px;
      }
      @media print {
        .page-break-marker { display: none; }
        .printable-section { break-inside: avoid; }
      }
    "))
  ),
  
  nav_panel(
    title = span(id = "home_tab_label", "Inicio"),
    uiOutput("home_content"),
    icon = icon("home")
  ),
  nav_panel(
    title = span(id = "menu_stat_placeholder", "Intervalos de confianza para la mediana"),
    ic_mediana_UI("ic_med_module")
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

server <- function(input, output, session) {
  
  # Capturamos dinámicamente el idioma proveniente del JS (URL o postMessage)
  lang <- reactive({
    # Escucha la variable global inyectada desde React. Si es nula (al arrancar), asume 'es'.
    l <- if (is.null(input$global_lang)) "es" else input$global_lang
    message(paste("DEBUG: lang reactive evaluated, value =", l))
    l
  })

  nav_tr <- list(
    es = list(
      home = "Inicio",
      home_welcome = "Bienvenido/a a <strong>EpiMétrika</strong>, el portal de apps de Epidemiología y Estadística.",
      home_version = "EpiMétrika v11. Notas de versión.",
      home_author = "Autor: Eduardo Ortega Páez. Pediatra. Comité de Pediatría Basada en la Evidencia. AEP.",
      home_endorsed = "Con el aval del Comité de Pediatría Basada en la Evidencia de la AEP y de la AEPAP.",
      home_contact = "Para comunicar cualquier tipo de error o sugerencia le agradeceríamos que escribiera a gatedu@gmail.com",
      home_citation = "Si utiliza la calculadora para publicar un trabajo científico, rogamos sea citada como: \"EpiMétrika. Portal de apps de Epidemiología y Estadística. E. Ortega Páez. Comité de Pediatría Basada en la Evidencia de la AEP. 2019.\"",
      home_disclaimer = "El autor no se responsabiliza del uso que pueda darse a la información derivada de esta aplicación. Se ha esforzado en que la información detallada en esta página sea lo más precisa y actualizada posible, pero dado los cambios constantes de la epidemiología podría ser que no sea completa o contenga algún error, por ello invitamos al usuario que los utilice con sentido crítico y compare los resultados.",
      home_legal_link = "NOTA LEGAL",
      print_title = "Calcupedev - Imprimir resultados",
      print_header = "Calculadora epidemiológica",
      print_preview = "Vista Previa de Impresión",
      menu_stat_placeholder = "Intervalos de confianza para la mediana",
      sub_kappa = "Concordancia Kappa",
      sub_cci_multi = "CCI Múltiples Observadores",
      print_btn_close = "Cerrar",
      print_btn_print = "Imprimir"
    ),
    en = list(
      home = "Home",
      home_welcome = "Welcome to <strong>EpiMétrika</strong>, the portal of apps for Epidemiology and Statistics.",
      home_version = "EpiMétrika v11. Release Notes.",
      home_author = "Author: Eduardo Ortega Páez. Pediatrician. Evidence-Based Pediatrics Committee. AEP.",
      home_endorsed = "Endorsed by the Evidence-Based Pediatrics Committee of the AEP and AEPAP.",
      home_contact = "To report any errors or suggestions, please write to gatedu@gmail.com",
      home_citation = "If you use this calculator to publish a scientific paper, please cite it as: \"EpiMétrika. Portal of apps for Epidemiology and Statistics. E. Ortega Páez. Evidence-Based Pediatrics Committee of the AEP. 2019.\"",
      home_disclaimer = "The author is not responsible for the use that may be made of the information derived from this application. Efforts have been made to ensure that the information detailed on this page is as accurate and up-to-date as possible, but given the constant changes in epidemiology, it may not be complete or may contain errors, so we invite the user to use it with critical sense and compare the results.",
      home_legal_link = "LEGAL NOTICE",
      print_title = "EpiMétrika - Print Results",
      print_header = "Epidemiological Calculator",
      print_preview = "Print Preview",
      menu_stat_placeholder = "Confidence Intervals for the Median",
      sub_kappa = "Kappa Concordance",
      sub_cci_multi = "ICC Multiple Observers",
      print_btn_close = "Close",
      print_btn_print = "Print"
    ),
    pt = list(
      home = "Início",
      home_welcome = "Bem-vindo(a) ao <strong>EpiMétrika</strong>, o portal de aplicativos de Epidemiologia e Estatística.",
      home_version = "EpiMétrika v11. Notas de versão.",
      home_author = "Autor: Eduardo Ortega Páez. Pediatra. Comitê de Pediatria Baseada em Evidências. AEP.",
      home_endorsed = "Com o aval do Comitê de Pediatria Baseada em Evidências da AEP e da AEPAP.",
      home_contact = "Para comunicar qualquer tipo de erro ou sugestão, por favor escreva para gatedu@gmail.com",
      home_citation = "Se utilizar a calculadora para publicar um trabajo científico, solicitamos que seja citada como: \"EpiMétrika. Portal de aplicativos de Epidemiologia e Estatística. E. Ortega Páez. Comitê de Pediatria Baseada em Evidências da AEP. 2019.\"",
      home_disclaimer = "O autor não se responsabiliza pelo uso que possa ser dado às informações derivadas desta aplicação. Esforçou-se para que a informação detalhada nesta página seja o mais precisa e atualizada possível, mas dadas as constantes mudanças na epidemiologia, pode ser que não esteja completa ou contenha algum erro, por isso convidamos o usuário a utilizá-la com sentido crítico e comparar os resultados.",
      home_legal_link = "NOTA LEGAL",
      print_title = "EpiMétrika - Imprimir resultados",
      print_header = "Calculadora epidemiológica",
      print_preview = "Visualização de impressão",
      menu_stat_placeholder = "Intervalos de confiança para a mediana",
      sub_kappa = "Concordância Kappa",
      sub_cci_multi = "ICC Múltiplas Observações",
      print_btn_close = "Fechar",
      print_btn_print = "Imprimir"
    ),
    zh = list(
      home = "首页",
      home_welcome = "欢迎使用 <strong>EpiMetric</strong>，流行病学与统计应用门户。",
      home_version = "EpiMétrika v11. 版本说明。",
      home_author = "作者：Eduardo Ortega Páez。儿科医生。AEP 循证儿科学委员会。",
      home_endorsed = "得到 AEP 和 AEPAP 循证儿科学委员会的认可。",
      home_contact = "如需报告任何错误或建议，请发送至 gatedu@gmail.com",
      home_citation = "如果您在发表科研论文时使用此计算器，请引用为：\"EpiMétrika. Tool for epidemiological calculation in pediatrics. E. Ortega Páez. Evidence-Based Pediatrics Committee of the AEP. 2019.\"",
      home_disclaimer = "作者不对本应用程序衍生信息的使用负责。页内信息已尽力做到最准确和最新，但鉴于流行病学的不断变化，信息可能不完整或包含误差，因此邀请用户批判性地使用并对比结果。",
      home_legal_link = "法律声明",
      print_title = "EpiMétrika - 打印结果",
      print_header = "流行病学计算器",
      print_preview = "打印预览",
      menu_stat_placeholder = "中位数置信区间",
      sub_kappa = "Kappa 一致性",
      sub_cci_multi = "组内相关系数 (多重观察者)",
      print_btn_close = "关闭",
      print_btn_print = "打印"
    )
  )

  # Actualizar títulos de navegación e idioma de JS dinámicamente
  observeEvent(lang(), {
    t <- nav_tr[[lang()]]
    if (is.null(t)) t <- nav_tr$es

    session$sendCustomMessage("updateNavTitles", t)
    session$sendCustomMessage("updatePrintTitle", t$print_title)
    session$sendCustomMessage("updatePrintHeader", t$print_header)
    session$sendCustomMessage("updatePrintPreviewTitle", t$print_preview)
    session$sendCustomMessage("updatePrintBtnClose", t$print_btn_close)
    session$sendCustomMessage("updatePrintBtnPrint", t$print_btn_print)
  }, ignoreInit = FALSE)

  # Plantilla Unificada de Home
  output$home_content <- renderUI({
    t <- nav_tr[[lang()]]
    legal_link_id <- paste0("link_legal_", lang())
    
    div(
      class = "home-container",
      div(class = "home-welcome-text", HTML(t$home_welcome)),
      div(class = "home-logo-text", "EVIDENCIAS EN PEDIATRÍA"),
      div(
        class = "home-footer",
        p(t$home_version),
        p(t$home_author),
        p(t$home_endorsed),
        p(t$home_contact),
        p(t$home_citation),
        p(t$home_disclaimer)
      ),
      div(
        class = "home-links",
        tags$a(href = "https://www.evidenciasenpediatria.es", "EVIDENCIAS EN PEDIATRÍA", target = "_blank", style = "color: inherit; text-decoration: underline; margin-right: 5px;"), " | ",
        actionLink(legal_link_id, t$home_legal_link, style = "color: inherit; text-decoration: underline; text-transform: uppercase; margin: 0 5px;")
      )
    )
  })

  # Modales Legales
  observeEvent(input$link_legal_es, { showModal(modalDialog(title = "Nota legal", nav_tr$es$home_disclaimer, easyClose = TRUE, footer = modalButton("Cerrar"))) })
  observeEvent(input$link_legal_pt, { showModal(modalDialog(title = "Nota legal", nav_tr$pt$home_disclaimer, easyClose = TRUE, footer = modalButton("Fechar"))) })
  observeEvent(input$link_legal_zh, { showModal(modalDialog(title = "法律声明", nav_tr$zh$home_disclaimer, easyClose = TRUE, footer = modalButton("关闭"))) })
  observeEvent(input$link_legal_en, { showModal(modalDialog(title = "Legal notice", nav_tr$en$home_disclaimer, easyClose = TRUE, footer = modalButton("Close"))) })

  # Módulos hijos (se re-evaluarán cuando el reactivo 'lang' emita un nuevo valor)
  ic_mediana_Server("ic_med_module", lang)
  kappa_Server("kappa_module", lang)
  cci_multi_Server("cci_multi_module", lang)
}

shinyApp(ui, server)
