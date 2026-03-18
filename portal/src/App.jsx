import { useState, useEffect } from 'react';
import './App.css';

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
    footer: 'EpiMétrika V2 · Evidencias en Pediatría · Eduardo Ortega Páez · AEP',
    loading: 'Cargando módulo…',
    assistant_hint: 'Asistente Metodix disponible dentro de cada módulo.',
    flag: '🇪🇸',
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
    footer: 'EpiMétrika V2 · Evidencias en Pediatría · Eduardo Ortega Páez · AEP',
    loading: 'Loading module…',
    assistant_hint: 'Metodix AI assistant available inside each module.',
    flag: '🇬🇧',
  },
  pt: {
    tagline: 'Portal de aplicativos de Epidemiologia e Estatística',
    subtitle: 'Selecione um módulo para começar',
    card_epi_title: 'Calculadoras Epidemiológicas',
    card_epi_desc: 'Estudos de coorte, caso-controle, OR/RR, ecológicos, transversais e mais.',
    card_stat_title: 'Calculadoras Estatísticas',
    card_stat_desc: 'Nomograma de Fagan, testes diagnósticos, concordância Kappa, ICC e mais.',
    card_sample_title: 'Tamanho da Amostra',
    card_sample_desc: 'Cálculos de poder e tamanho da amostra para diferentes desenhos de estudo.',
    btn_open: 'Abrir módulo',
    back: '← Voltar',
    theme_dark: 'Escuro',
    theme_light: 'Claro',
    footer: 'EpiMétrika V2 · Evidencias en Pediatría · Eduardo Ortega Páez · AEP',
    loading: 'Carregando módulo…',
    assistant_hint: 'Assistente Metodix IA disponível dentro de cada módulo.',
    flag: '🇧🇷',
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
    footer: 'EpiMétrika V2 · Evidencias en Pediatría · Eduardo Ortega Páez · AEP',
    loading: '正在加载模块…',
    assistant_hint: 'Metodix AI 智能助手在每个模块中均可使用。',
    flag: '🇨🇳',
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
  const t = i18n[lang];

  useEffect(() => {
    document.documentElement.setAttribute('data-theme', theme);
    // Send theme update to iframe if it exists
    const iframe = document.querySelector('.module-iframe');
    if (iframe && iframe.contentWindow) {
      iframe.contentWindow.postMessage({ type: 'theme', value: theme }, '*');
    }
  }, [theme]);

  useEffect(() => {
    // Send lang update to iframe if it exists
    const iframe = document.querySelector('.module-iframe');
    if (iframe && iframe.contentWindow) {
      iframe.contentWindow.postMessage({ type: 'lang', value: lang }, '*');
    }
  }, [lang]);

  // When iframe loads, send current state to initialize it
  const handleIframeLoad = () => {
    setIframeLoaded(true);
    const iframe = document.querySelector('.module-iframe');
    if (iframe && iframe.contentWindow) {
      iframe.contentWindow.postMessage({ type: 'theme', value: theme }, '*');
      iframe.contentWindow.postMessage({ type: 'lang', value: lang }, '*');
    }
  };

  const openModule = (mod) => {
    setIframeLoaded(false);
    setActiveModule(mod);
  };

  const goHome = () => setActiveModule(null);

  return (
    <div className="portal-root">
      {/* ── NAVBAR ─────────────────────────────────────────────────── */}
      <nav className="navbar">
        <div className="navbar-brand" onClick={goHome}>
          <span className="brand-icon">🩺</span>
          <span className="brand-text">EpiMétrika</span>
          <span className="brand-version">V2</span>
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

          <button
            className="print-btn"
            title={t.flag === '🇪🇸' ? 'Imprimir' : t.flag === '🇬🇧' ? 'Print' : t.flag === '🇧🇷' ? 'Imprimir' : '打印'}
            onClick={() => {
              const iframe = document.querySelector('.module-iframe');
              if (iframe && iframe.contentWindow) {
                iframe.contentWindow.postMessage({ type: 'print' }, '*');
              }
            }}
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
          /* ── HOME / HUB ─────────────────────────────── */
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

            <p className="assistant-hint">🤖 {t.assistant_hint}</p>
          </div>
        ) : (
          /* ── IFRAME MODULE ──────────────────────────── */
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
              key={activeModule.path}
              src={activeModule.path}
              title={t[activeModule.titleKey]}
              className={`module-iframe ${iframeLoaded ? 'visible' : ''}`}
              onLoad={() => setIframeLoaded(true)}
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
    </div>
  );
}
