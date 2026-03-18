/**
 * EpiMétrika RAG Chat Widget - METODIX
 * Floating draggable/resizable chat window for n8n-based AI assistant
 */

(function () {
    'use strict';

    // ============== CONFIGURATION ==============
    const CONFIG = {
        apiEndpoint: 'https://n8n.cienciasinseso.com/webhook/chat',
        enabled: true,
        language: 'es',
        maxHistory: 10
    };

    // ============== PERSISTENT SESSION ID ==============
    const getSessionId = () => {
        let sid = localStorage.getItem('calcupedev_metodix_sid');
        if (!sid) {
            sid = 'sid_' + Math.random().toString(36).substr(2, 9) + '_' + Date.now();
            localStorage.setItem('calcupedev_metodix_sid', sid);
        }
        return sid;
    };

    // ============== TRANSLATIONS ==============
    const i18n = {
        es: {
            title: 'Metodix',
            placeholder: 'Escribe tu pregunta...',
            send: 'Enviar',
            thinking: 'Pensando...',
            error: 'Error de conexión. Intenta de nuevo.',
            welcome: '¡Hola! Soy Metodix, tu asistente experto en metodología y MBE. ¿Qué duda te reconcome hoy?',
            disabled: 'Metodix está descansando en este momento.',
            minimize: 'Minimizar',
            close: 'Cerrar'
        },
        en: {
            title: 'Metodix',
            placeholder: 'Type your question...',
            send: 'Send',
            thinking: 'Thinking...',
            error: 'Connection error. Please try again.',
            welcome: 'Hello! I\'m Metodix, your expert assistant in methodology and EBM. What\'s on your mind today?',
            disabled: 'Metodix is resting at the moment.',
            minimize: 'Minimize',
            close: 'Close'
        },
        pt: {
            title: 'Metodix',
            placeholder: 'Digite sua pergunta...',
            send: 'Enviar',
            thinking: 'Pensando...',
            error: 'Erro de conexão. Tente novamente.',
            welcome: 'Olá! Sou Metodix, seu assistente especialista em metodologia e MBE. Qual é a sua dúvida hoje?',
            disabled: 'Metodix está descansando no momento.',
            minimize: 'Minimizar',
            close: 'Fechar'
        },
        zh: {
            title: 'Metodix',
            placeholder: '输入您的问题...',
            send: '发送',
            thinking: '思考中...',
            error: '连接错误。请重试。',
            welcome: '您好！我是 Metodix，您的方法学与循证医学专家助手。今天有什么疑问吗？',
            disabled: 'Metodix 目前正在休息。',
            minimize: '最小化',
            close: '关闭'
        }
    };

    // ============== STATE ==============
    let currentLang = CONFIG.language;
    let chatHistory = [];
    let isMinimized = true;
    let isDragging = false;
    let dragOffset = { x: 0, y: 0 };
    const sessionId = getSessionId();

    // ============== HELPER FUNCTIONS ==============
    function t(key) {
        return i18n[currentLang]?.[key] || i18n['es'][key] || key;
    }

    function createElementFromHTML(html) {
        const template = document.createElement('template');
        template.innerHTML = html.trim();
        return template.content.firstChild;
    }

    function getCurrentCalculator() {
        const activeTab = document.querySelector('.tab-pane.active');
        if (!activeTab) return 'home';
        const tabId = activeTab.id || '';
        const heading = activeTab.querySelector('h3, h4')?.textContent || '';
        return heading || tabId || 'unknown';
    }

    function formatMessage(content, isUser) {
        const div = document.createElement('div');
        div.className = `chat-message ${isUser ? 'user' : 'assistant'}`;
        if (!isUser) {
            let html = typeof content === 'string' ? content
                .replace(/\*\*(.*?)\*\*/g, '<strong>$1</strong>')
                .replace(/\n/g, '<br>') : content;
            div.innerHTML = html;
        } else {
            div.textContent = content;
        }
        return div;
    }

    function scrollToBottom() {
        const messages = document.getElementById('calcupedev-chat-messages');
        if (messages) messages.scrollTop = messages.scrollHeight;
    }

    // ============== API CALL ==============
    let responseResolver = null;

    // ============== API CALL (VIA SHINY PROXY) ==============
    async function sendToRAG(message) {
        if (!CONFIG.enabled) {
            return { answer: t('disabled'), error: true };
        }

        if (!window.Shiny) {
            console.warn("Metodix: Shiny not found, falling back to fetch (likely to fail CORS)");
            try {
                const response = await fetch(CONFIG.apiEndpoint, {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    mode: 'cors',
                    body: JSON.stringify({ question: message, chatInput: message, sessionId: sessionId })
                });
                const data = await response.json();
                return { answer: data.output || data.text || t('error') };
            } catch (e) { return { answer: t('error'), error: true }; }
        }

        console.log("Metodix: Sending message via Shiny Proxy", { question: message });

        return new Promise((resolve) => {
            // Guardar el resolver para cuando llegue la respuesta de R
            responseResolver = resolve;

            Shiny.setInputValue('metodix_query', {
                question: message,
                chatInput: message,
                sessionId: sessionId,
                language: currentLang,
                context: { calculator: getCurrentCalculator() }
            }, { priority: 'event' });

            // Timeout de seguridad por si falla la red
            setTimeout(() => {
                if (responseResolver === resolve) {
                    console.error("Metodix: Proxy timeout");
                    resolve({ answer: t('error'), error: true });
                    responseResolver = null;
                }
            }, 35000);
        });
    }

    if (window.Shiny) {
        Shiny.addCustomMessageHandler('metodix_response', function (data) {
            console.log("Metodix: Received proxy response:", data);
            if (responseResolver) {
                if (data.success) {
                    responseResolver({ answer: data.output, sources: data.sources || [] });
                } else {
                    responseResolver({ answer: data.error || t('error'), error: true });
                }
                responseResolver = null;
            }
        });
    }

    // ============== UI CREATION ==============
    function createChatWidget() {
        if (document.getElementById('calcupedev-chat-widget')) return;
        const widget = createElementFromHTML(`
      <div id="calcupedev-chat-widget" class="chat-widget minimized">
        <button id="calcupedev-chat-toggle" class="chat-toggle-btn" title="${t('title')}">
          <div class="metodix-avatar">M</div>
        </button>
        <div id="calcupedev-chat-window" class="chat-window">
          <div id="calcupedev-chat-header" class="chat-header">
            <span class="chat-title">Metodix</span>
            <div class="chat-header-buttons">
              <button id="calcupedev-chat-minimize" class="chat-btn-icon" title="${t('minimize')}">−</button>
            </div>
          </div>
          <div id="calcupedev-chat-messages" class="chat-messages">
            <div class="chat-message assistant">${t('welcome')}</div>
          </div>
          <div class="chat-input-area">
            <input type="text" id="calcupedev-chat-input" placeholder="${t('placeholder')}" autocomplete="off">
            <button id="calcupedev-chat-send" class="chat-send-btn" title="${t('send')}">➤</button>
          </div>
        </div>
      </div>
    `);
        document.body.appendChild(widget);
        setupEventListeners();
    }

    function updateChatUI() {
        const toggle = document.getElementById('calcupedev-chat-toggle');
        if (toggle) toggle.title = t('title');

        const titleElem = document.querySelector('.chat-title');
        if (titleElem) titleElem.textContent = t('title');

        const minimize = document.getElementById('calcupedev-chat-minimize');
        if (minimize) minimize.title = t('minimize');

        const input = document.getElementById('calcupedev-chat-input');
        if (input) input.placeholder = t('placeholder');

        const sendBtn = document.getElementById('calcupedev-chat-send');
        if (sendBtn) sendBtn.title = t('send');

        // Update welcome message
        const messagesContainer = document.getElementById('calcupedev-chat-messages');
        if (messagesContainer) {
            const userMessages = messagesContainer.querySelectorAll('.chat-message.user');
            // ONLY update if user hasn't typed anything yet
            if (userMessages.length === 0) {
                // Find all assistant messages that are just the welcome message
                // (usually there's only one at start)
                const assistantMessages = messagesContainer.querySelectorAll('.chat-message.assistant');
                assistantMessages.forEach(msg => {
                    // Check if it matches a welcome message from any language to be safe
                    const isAnyWelcome = Object.values(i18n).some(langObj => langObj.welcome === msg.textContent);
                    if (isAnyWelcome || assistantMessages.length === 1) {
                        msg.textContent = t('welcome');
                    }
                });
            }
        }
    }

    function setupEventListeners() {
        const widget = document.getElementById('calcupedev-chat-widget');
        const toggle = document.getElementById('calcupedev-chat-toggle');
        const minimize = document.getElementById('calcupedev-chat-minimize');
        const header = document.getElementById('calcupedev-chat-header');
        const input = document.getElementById('calcupedev-chat-input');
        const sendBtn = document.getElementById('calcupedev-chat-send');

        const openChat = () => {
            isMinimized = false;
            widget.classList.remove('minimized');
            input.focus();
            scrollToBottom();
        };

        toggle.addEventListener('click', () => {
            isMinimized = !isMinimized;
            widget.classList.toggle('minimized', isMinimized);
            if (!isMinimized) openChat();
        });

        // External triggers
        document.addEventListener('click', (e) => {
            if (e.target && (e.target.id === 'metodix-trigger' || e.target.id === 'metodix-trigger-en' || e.target.id === 'metodix-trigger-pt' || e.target.id === 'metodix-trigger-zh')) {
                e.preventDefault();
                openChat();
            }
        });

        minimize.addEventListener('click', () => {
            isMinimized = true;
            widget.classList.add('minimized');
        });

        async function handleSend() {
            const message = input.value.trim();
            if (!message) return;
            const messagesContainer = document.getElementById('calcupedev-chat-messages');
            messagesContainer.appendChild(formatMessage(message, true));
            input.value = '';
            scrollToBottom();

            const thinkingEl = formatMessage(t('thinking'), false);
            thinkingEl.classList.add('thinking');
            messagesContainer.appendChild(thinkingEl);
            scrollToBottom();

            const response = await sendToRAG(message);
            thinkingEl.remove();
            messagesContainer.appendChild(formatMessage(response.answer, false));
            scrollToBottom();
        }

        sendBtn.addEventListener('click', handleSend);
        input.addEventListener('keypress', (e) => { if (e.key === 'Enter') handleSend(); });

        // Simple drag
        header.addEventListener('mousedown', (e) => {
            if (e.target.closest('button')) return;
            isDragging = true;
            const rect = document.getElementById('calcupedev-chat-window').getBoundingClientRect();
            dragOffset.x = e.clientX - rect.left;
            dragOffset.y = e.clientY - rect.top;
        });

        document.addEventListener('mousemove', (e) => {
            if (!isDragging) return;
            const chatWindow = document.getElementById('calcupedev-chat-window');
            chatWindow.style.left = (e.clientX - dragOffset.x) + 'px';
            chatWindow.style.top = (e.clientY - dragOffset.y) + 'px';
            chatWindow.style.bottom = 'auto';
            chatWindow.style.right = 'auto';
        });

        document.addEventListener('mouseup', () => { isDragging = false; });
    }


    function registerShinyHandlers() {
        if (window.Shiny && window.Shiny.addCustomMessageHandler) {
            console.log("Metodix: Registering Shiny handlers");
            Shiny.addCustomMessageHandler('metodix_response', function (data) {
                console.log("Metodix: Received proxy response:", data);
                if (responseResolver) {
                    if (data.success) {
                        responseResolver({ answer: data.output, sources: data.sources || [] });
                    } else {
                        responseResolver({ answer: data.error || t('error'), error: true });
                    }
                    responseResolver = null;
                }
            });
            Shiny.addCustomMessageHandler('updateChatLanguage', (lang) => {
                if (i18n[lang]) {
                    currentLang = lang;
                    updateChatUI();
                }
            });
        } else {
            setTimeout(registerShinyHandlers, 500);
        }
    }

    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', () => {
            createChatWidget();
            registerShinyHandlers();
        });
    } else {
        createChatWidget();
        registerShinyHandlers();
    }
})();
