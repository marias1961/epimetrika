import { useState, useEffect, useRef } from 'react';
import './App.css';
import AssistantWidget from './components/AssistantWidget';

// ─── Traducciones del portal ────────────────────────────────────────────────
const i18n = {
  es: {
    tagline: 'Portal de apps de Epidemiología y Estadística',
    subtitle: 'Selecciona un módulo para comenzar',
    card_epi_title: 'Calculadoras Epidemiológicas',
    card_epi_desc: 'Estudios de cohortes, casos y controles, OR/RR, ecológicos, transversales y más.',
    card_stat_title: 'Calculadoras Estadísticas',
    card_stat_desc: 'Nomograma de Fagan, pruebas diagnósticas, concordancia Kappa, ICC y más.',
    card_sample_title: 'Tamaño Muestral',
    card_sample_desc: 'Calculadoras de potencia y tamaño muestral para distintos diseños de estudio.',
    btn_open: 'Abrir módulo',
    back: '← Volver',
    theme_dark: 'Oscuro',
    theme_light: 'Claro',
    footer: 'EpiMétrika · 2026',
    loading: 'Cargando módulo…',
    assistant_hint: 'Puedes consultar tus dudas a Metodix',
    legal_note: 'Nota legal',
    legal_text: 'El autor no se responsabiliza del uso que pueda darse a la información derivada de esta aplicación. Se ha esforzado en que la información detallada en esta página sea lo más precisa y actualizada posible, pero dado los cambios constantes de la epidemiología podría ser que no sea completa o contenga algún error, por ello invitamos al usuario que los utilice con sentido crítico y compare los resultados.',
    welcome: 'Bienvenida',
    welcome_title: 'Mensaje de bienvenida',
    welcome_text_1: 'Bienvenido/a a',
    welcome_text_2: ', el portal de apps de Epidemiología y Estadística.',
    welcome_logo_text: 'EVIDENCIAS EN PEDIATRÍA',
    welcome_v: 'EpiMétrika v11. Notas de versión.',
    welcome_author: 'Autor: Eduardo Ortega Páez. Pediatra. Comité de Pediatría Basada en la Evidencia. AEP.',
    welcome_endorsement: 'Con el aval del Comité de Pediatría Basada en la Evidencia de la AEP y de la AEPAP.',
    welcome_contact: 'Para comunicar cualquier tipo de error o sugerencia le agradeceríamos que escribiera a gatedu@gmail.com',
    welcome_citation: 'Si utiliza la calculadora para publicar un trabajo científico, rogamos sea citada como: "EpiMétrika. Portal de apps de Epidemiología y Estadística. E. Ortega Páez. Comité de Pediatría Basada en la Evidencia de la AEP. 2019."',
    flag: '🇪🇸',
    print: 'Imprimir',
  },
  en: {
    tagline: 'Epidemiology and Statistics Apps Portal',
    subtitle: 'Select a module to get started',
    card_epi_title: 'Epidemiological Calculators',
    card_epi_desc: 'Cohort studies, case-control, OR/RR, ecological, cross-sectional and more.',
    card_stat_title: 'Statistical Calculators',
    card_stat_desc: 'Fagan nomogram, diagnostic tests, Kappa concordance, ICC and more.',
    card_sample_title: 'Sample Size',
    card_sample_desc: 'Power and sample size calculators for different study designs.',
    btn_open: 'Open module',
    back: '← Back',
    theme_dark: 'Dark',
    theme_light: 'Light',
    footer: 'EpiMétrika · 2026',
    loading: 'Loading module…',
    assistant_hint: 'You can consult your doubts with Metodix',
    legal_note: 'Legal note',
    legal_text: 'The author is not responsible for the use that may be given to the information derived from this application. Efforts have been made to ensure that the detailed information on this page is as accurate and up-to-date as possible, but given the constant changes in epidemiology, it may not be complete or may contain errors. Therefore, we invite the user to use it with a critical sense and compare the results.',
    welcome: 'Welcome',
    welcome_title: 'Welcome message',
    welcome_text_1: 'Welcome to',
    welcome_text_2: ', the portal of apps for Epidemiology and Statistics.',
    welcome_logo_text: 'EVIDENCIAS EN PEDIATRÍA',
    welcome_v: 'EpiMétrika v11. Release Notes.',
    welcome_author: 'Author: Eduardo Ortega Páez. Pediatrician. Evidence-Based Pediatrics Committee. AEP.',
    welcome_endorsement: 'Endorsed by the Evidence-Based Pediatrics Committee of the AEP and AEPAP.',
    welcome_contact: 'To report any errors or suggestions, please write to gatedu@gmail.com',
    welcome_citation: 'If you use this calculator to publish a scientific paper, please cite it as: "EpiMétrika. Portal of apps for Epidemiology and Statistics. E. Ortega Páez. Evidence-Based Pediatrics Committee of the AEP. 2019."',
    flag: '🇬🇧',
    print: 'Print',
  },
  pt: {
    tagline: 'Portal de aplicativos de Epidemiologia e Estatística',
    subtitle: 'Selecione um módulo para começar',
    card_epi_title: 'Calculadoras Epidemiológicas',
    card_epi_desc: 'Estudos de coorte, caso-controle, OR/RR, ecológicos, transversais e mais.',
    card_stat_title: 'Calculadoras Estatísticas',
    card_stat_desc: 'Nomograma de Fagan, testes diagnósticos, concordância Kappa, ICC e mais.',
    card_sample_title: 'Tamanho da Amostra',
    card_sample_desc: 'Cálculos de poder e tamanho da amostra para diferentes desenhos de estudio.',
    btn_open: 'Abrir módulo',
    back: '← Voltar',
    theme_dark: 'Escuro',
    theme_light: 'Claro',
    footer: 'EpiMétrika · 2026',
    loading: 'Carregando módulo…',
    assistant_hint: 'Você pode consultar suas dúvidas com o Metodix',
    legal_note: 'Nota legal',
    legal_text: 'O autor não se responsabiliza pelo uso que possa ser dado à informação derivada desta aplicação. Esforço foi feito para que a informação detalhada nesta página seja a mais precisa e atualizada possível, mas dadas as constantes mudanças da epidemiologia, pode ser que não esteja completa ou contenha algum erro. Por isso, convidamos o usuário a utilizá-la com senso crítico e a comparar os resultados.',
    welcome: 'Boas-vindas',
    welcome_title: 'Mensagem de boas-vindas',
    welcome_text_1: 'Bem-vindo(a) ao',
    welcome_text_2: ', o portal de aplicativos de Epidemiologia e Estatística.',
    welcome_logo_text: 'EVIDENCIAS EN PEDIATRÍA',
    welcome_v: 'EpiMétrika v11. Notas de versão.',
    welcome_author: 'Autor: Eduardo Ortega Páez. Pediatra. Comitê de Pediatria Baseada em Evidências. AEP.',
    welcome_endorsement: 'Com o aval do Comitê de Pediatria Baseada em Evidências da AEP e da AEPAP.',
    welcome_contact: 'Para comunicar qualquer tipo de erro ou sugestão, por favor escreva para gatedu@gmail.com',
    welcome_citation: 'Se utilizar a calculadora para publicar um trabalho científico, solicitamos que seja citada como: "EpiMétrika. Portal de aplicativos de Epidemiologia e Estatística. E. Ortega Páez. Comitê de Pediatria Baseada em Evidências da AEP. 2019."',
    flag: '🇧🇷',
    print: 'Imprimir',
  },
  zh: {
    tagline: '流行病学与统计应用门户',
    subtitle: '选择一个模块开始使用',
    card_epi_title: '流行病学计算器',
    card_epi_desc: '队列研究、病例对照、OR/RR、生态学、横断面研究等。',
    card_stat_title: '统计学计算器',
    card_stat_desc: 'Fagan 诺莫图、诊断试验、Kappa 一致性、ICC 等。',
    card_sample_title: '样本量计算',
    card_sample_desc: '不同研究设计的统计功效和样本量计算器。',
    btn_open: '打开模块',
    back: '← 返回',
    theme_dark: '深色',
    theme_light: '浅色',
    footer: 'EpiMétrika · 2026',
    loading: '正在加载模块…',
    assistant_hint: '您可以向 Metodix 咨询您的疑问',
    legal_note: '法律声明',
    legal_text: '作者对可能因使用本应用程序衍生的信息而造成的后果不承担任何责任。我们已努力确保本页面上的详细信息尽可能准确和最新，但鉴于流行病学的不断变化，信息可能不完整或包含误差。因此，我们建议用户以批判的态度使用并比较结果。',
    welcome: '欢迎',
    welcome_title: '欢迎信息',
    welcome_text_1: '欢迎使用',
    welcome_text_2: '，流行病学与统计应用门户。',
    welcome_logo_text: 'EVIDENCIAS EN PEDIATRÍA',
    welcome_v: 'EpiMétrika v11. 版本说明。',
    welcome_author: '作者：Eduardo Ortega Páez。儿科医生。AEP 循证儿科学委员会。',
    welcome_endorsement: '得到 AEP 和 AEPAP 循证儿科学委员会的认可。',
    welcome_contact: '如需报告任何错误或建议，请发送至 gatedu@gmail.com',
    welcome_citation: '如果您在发表科研论文时使用此计算器，请引用为："EpiMétrika. Portal of apps for Epidemiology and Statistics. E. Ortega Páez. Evidence-Based Pediatrics Committee of the AEP. 2019."',
    flag: '🇨🇳',
    print: '打印',
  },
};

const LANG_OPTS = [
  { value: 'es', label: '🇪🇸 Español' },
  { value: 'en', label: '🇬🇧 English' },
  { value: 'pt', label: '🇧🇷 Português' },
  { value: 'zh', label: '🇨🇳 中文' },
];

const MODULES = [
  {
    key: 'epi',
    path: '/api/epi/',
    icon: '🧬',
    color: 'card-epi',
    titleKey: 'card_epi_title',
    descKey: 'card_epi_desc',
  },
  {
    key: 'stat',
    path: '/api/stat/',
    icon: '📊',
    color: 'card-stat',
    titleKey: 'card_stat_title',
    descKey: 'card_stat_desc',
  },
  {
    key: 'sample',
    path: '/api/sample/',
    icon: '🔬',
    color: 'card-sample',
    titleKey: 'card_sample_title',
    descKey: 'card_sample_desc',
  },
];

export default function App() {
  const [lang, setLang] = useState('es');
  const [theme, setTheme] = useState('dark');
  const [activeModule, setActiveModule] = useState(null);
  const [iframeLoaded, setIframeLoaded] = useState(false);
  const [showLegalModal, setShowLegalModal] = useState(false);
  const [showWelcomeModal, setShowWelcomeModal] = useState(false);

  const t = i18n[lang];
  const assistantRef = useRef(null);
  const iframeRef = useRef(null);

  // ─── COMUNICACIÓN CON SHINY ──────────────────────────────────────────────────
  const sendToShiny = (action, payload) => {
    // Intentamos usar la referencia primero (más seguro y directo)
    let iframeWindow = iframeRef.current?.contentWindow;

    // Fallback: si la ref falla por alguna razón del ciclo de React, buscamos en el DOM
    if (!iframeWindow) {
      const domIframe = document.querySelector('.module-iframe');
      if (domIframe) iframeWindow = domIframe.contentWindow;
    }

    if (iframeWindow) {
      // Usamos el formato estandarizado que hemos definido para el listener global de R
      iframeWindow.postMessage({ action, payload }, '*');
    }
  };

  // ─── EFECTOS GLOBALES ────────────────────────────────────────────────────────
  // Cambio de Tema
  useEffect(() => {
    document.documentElement.setAttribute('data-theme', theme);
    document.documentElement.setAttribute('data-bs-theme', theme); // Para forzar Bootstrap si es necesario
    sendToShiny('CHANGE_THEME', theme);
  }, [theme]);

  // Cambio de Idioma
  useEffect(() => {
    sendToShiny('CHANGE_LANG', lang);
  }, [lang]);

  // Sincronización al cargar un nuevo iframe
  const handleIframeLoad = () => {
    setIframeLoaded(true);
    // Le inyectamos el estado actual para que no parpadee al idioma/tema por defecto
    sendToShiny('CHANGE_LANG', lang);
    sendToShiny('CHANGE_THEME', theme);
  };

  // ─── NAVEGACIÓN Y ACCIONES ───────────────────────────────────────────────────
  const openModule = (mod) => {
    setIframeLoaded(false);
    setActiveModule(mod);
  };

  const goHome = () => {
    setActiveModule(null);
    setIframeLoaded(false);
  };

  const handlePrint = () => {
    sendToShiny('PRINT_REQUEST', null);
  };

  const openAssistantExternally = (e) => {
    if (e) e.preventDefault();
    if (assistantRef.current?.open) {
      assistantRef.current.open();
    }
  };

  return (
    <div className="portal-root">
      {/* ── NAVBAR ─────────────────────────────────────────────────── */}
      <nav className="navbar">
        <div className="navbar-brand" onClick={goHome} style={{ cursor: 'pointer' }}>
          <span className="brand-icon">🩺</span>
          <span className="brand-text">EpiMétrika</span>
        </div>

        <div className="navbar-controls">
          <select
            className="lang-select"
            value={lang}
            onChange={(e) => setLang(e.target.value)}
            aria-label="Select language"
          >
            {LANG_OPTS.map((o) => (
              <option key={o.value} value={o.value}>{o.label}</option>
            ))}
          </select>

          <button
            className="theme-btn"
            onClick={() => setTheme(t => t === 'dark' ? 'light' : 'dark')}
            title={theme === 'dark' ? t.theme_light : t.theme_dark}
          >
            {theme === 'dark' ? '☀️' : '🌙'}
          </button>

          {/* Botón de impresión refactorizado */}
          <button
            className="print-btn"
            title={t.print}
            onClick={handlePrint}
            disabled={!activeModule}
            style={{
              background: 'transparent',
              border: 'none',
              fontSize: '1.2rem',
              cursor: activeModule ? 'pointer' : 'not-allowed',
              opacity: activeModule ? 1 : 0.5,
              padding: '0 5px',
              display: 'grid',
              placeItems: 'center'
            }}
          >
            🖨️
          </button>
        </div>
      </nav>

      {/* ── MAIN ────────────────────────────────────────────────────── */}
      <main className="main-content">
        {!activeModule ? (
          <div className="hub">
            <div className="hero">
              <h1 className="hero-title">EpiMétrika</h1>
              <p className="hero-tagline">{t.tagline}</p>
              <p className="hero-subtitle">{t.subtitle}</p>
            </div>

            <div className="cards-grid">
              {MODULES.map((mod) => (
                <button
                  key={mod.key}
                  className={`module-card ${mod.color}`}
                  onClick={() => openModule(mod)}
                >
                  <span className="card-icon">{mod.icon}</span>
                  <h2 className="card-title">{t[mod.titleKey]}</h2>
                  <p className="card-desc">{t[mod.descKey]}</p>
                  <span className="card-cta">{t.btn_open} →</span>
                </button>
              ))}
            </div>

            <div
              className="assistant-hint-bar"
              style={{
                marginTop: '40px',
                userSelect: 'none'
              }}
            >
              <div 
                className="assistant-hint-sector side-sector"
                onClick={() => setShowLegalModal(true)}
                style={{ cursor: 'pointer' }}
              >
                <p className="assistant-hint" style={{ pointerEvents: 'none' }}>
                  {t.legal_note}
                </p>
              </div>
              <div 
                className="assistant-hint-sector center-sector" 
                onClick={openAssistantExternally}
                style={{ cursor: 'pointer' }}
              >
                <p className="assistant-hint" style={{ pointerEvents: 'none', margin: 0 }}>
                  🤖 {t.assistant_hint}
                </p>
              </div>
              <div 
                className="assistant-hint-sector side-sector"
                onClick={() => setShowWelcomeModal(true)}
                style={{ cursor: 'pointer' }}
              >
                <p className="assistant-hint" style={{ pointerEvents: 'none' }}>
                  {t.welcome}
                </p>
              </div>
            </div>
          </div>
        ) : (
          <div className="iframe-wrap">
            <div className="iframe-topbar">
              <button className="back-btn" onClick={goHome}>
                {t.back}
              </button>
              <span className="active-module-label">
                {activeModule.icon} {t[activeModule.titleKey]}
              </span>
            </div>

            {!iframeLoaded && (
              <div className="iframe-loading">
                <div className="spinner" />
                <p>{t.loading}</p>
              </div>
            )}

            <iframe
              ref={iframeRef}
              key={activeModule.path}
              src={activeModule.path}
              title={t[activeModule.titleKey]}
              className={`module-iframe ${iframeLoaded ? 'visible' : ''}`}
              onLoad={handleIframeLoad}
              allow="fullscreen"
            />
          </div>
        )}
      </main>

      {/* ── FOOTER ──────────────────────────────────────────────────── */}
      {!activeModule && (
        <footer className="portal-footer">
          <p>{t.footer}</p>
        </footer>
      )}

      {/* ── LEGAL MODAL ──────────────────────────────────────────────── */}
      {showLegalModal && (
        <div className="legal-modal-overlay" onClick={() => setShowLegalModal(false)}>
          <div className="legal-modal-content" onClick={(e) => e.stopPropagation()}>
            <h3 className="legal-modal-title">{t.legal_note}</h3>
            <p className="legal-modal-text">{t.legal_text}</p>
          </div>
        </div>
      )}

      {/* ── WELCOME MODAL ────────────────────────────────────────────── */}
      {showWelcomeModal && (
        <div className="legal-modal-overlay" onClick={() => setShowWelcomeModal(false)}>
          <div className="legal-modal-content welcome-modal-content" onClick={(e) => e.stopPropagation()}>
            <h3 className="legal-modal-title" style={{ textAlign: 'center', color: 'var(--accent)', fontSize: '1.8rem' }}>
              {t.welcome_title}
            </h3>
            
            <div className="welcome-hero-text">
              {t.welcome_text_1} <strong>EpiMétrika</strong>{t.welcome_text_2}
            </div>

            <div className="welcome-logo-text">
              {t.welcome_logo_text}
            </div>

            <div className="welcome-footer-text">
              <p>{t.welcome_v}</p>
              <p>{t.welcome_author}</p>
              <p>{t.welcome_endorsement}</p>
              <p>{t.welcome_contact}</p>
              <p>{t.welcome_citation}</p>
            </div>
            
            <div style={{ textAlign: 'center', marginTop: '24px' }}>
              <button 
                className="back-btn" 
                onClick={() => setShowWelcomeModal(false)}
                style={{ fontSize: '1rem', padding: '10px 24px' }}
              >
                {lang === 'es' ? 'Cerrar' : lang === 'en' ? 'Close' : lang === 'pt' ? 'Fechar' : '关闭'}
              </button>
            </div>
          </div>
        </div>
      )}

      {/* ASISTENTE CON REFERENCIA */}
      <AssistantWidget lang={lang} ref={assistantRef} />

    </div>
  );
}
