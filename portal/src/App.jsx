import { useState } from 'react';
import reactLogo from './assets/react.svg';
import './App.css';

function App() {
  const [activeModule, setActiveModule] = useState(null);

  // Funciones para cargar submódulos
  const loadModule = (modulePath) => {
    setActiveModule(modulePath);
  };

  const goHome = () => {
    setActiveModule(null);
  };

  return (
    <div className="portal-container">
      {/* Navbar Superior */}
      <nav className="navbar">
        <div className="navbar-brand" onClick={goHome} style={{ cursor: 'pointer' }}>
          <h2>🌐 Calcupedev Portal</h2>
        </div>
        <div className="navbar-controls">
          <select className="lang-select">
            <option value="es">🇪🇸 Español</option>
            <option value="en">🇬🇧 English</option>
          </select>
          <button className="theme-toggle">🌓 Tema</button>
        </div>
      </nav>

      {/* Área Principal (Home o iFrame del módulo) */}
      <main className="main-content">
        {!activeModule ? (
          <div className="hub-menu">
            <h1>Bienvenido a Calcupedev V2</h1>
            <p>Selecciona una herramienta para comenzar. Todo se ejecuta de forma modular y aislada.</p>

            <div className="cards-grid">
              <div className="module-card" onClick={() => loadModule('/api/epi/')}>
                <h3>🦠 Epidemiología</h3>
                <p>Estudios de Cohortes, Casos y Controles, Riesgos Relativos.</p>
                <button>Abrir Módulo</button>
              </div>

              <div className="module-card" onClick={() => loadModule('/api/stat/')}>
                <h3>📊 Estadística</h3>
                <p>Nomograma de Fagan, Análisis de Varianza y Pruebas Diagnósticas.</p>
                <button>Abrir Módulo</button>
              </div>

              <div className="module-card disabled">
                <h3>🧪 Tamaño Muestral</h3>
                <p>(En desarrollo)</p>
                <button disabled>Próximamente</button>
              </div>
            </div>
          </div>
        ) : (
          <div className="iframe-container">
            <div className="iframe-header">
              <button className="back-btn" onClick={goHome}>⬅ Volver al hub</button>
              <span className="module-path">Módulo activo: {activeModule}</span>
            </div>
            <iframe
              src={activeModule}
              title="Calcupedev Module"
              className="module-iframe"
            />
          </div>
        )}
      </main>
    </div>
  );
}

export default App;
