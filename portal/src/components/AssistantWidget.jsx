import { useState, useRef, useEffect, forwardRef, useImperativeHandle } from 'react';
import Draggable from 'react-draggable';
import './AssistantWidget.css';

// ─── Traducciones del Asistente ───────────────────────────────
const i18n_assistant = {
    es: {
        title: '🤖 Metodix',
        welcome: '¡Hola! Soy Metodix. ¿En qué te puedo ayudar hoy?',
        placeholder: 'Escribe tu pregunta...',
        typing: 'Metodix está escribiendo...'
    },
    en: {
        title: '🤖 Metodix',
        welcome: 'Hello! I am Metodix. How can I help you today?',
        placeholder: 'Type your question...',
        typing: 'Metodix is typing...'
    },
    pt: {
        title: '🤖 Metodix',
        welcome: 'Olá! Sou Metodix. Como posso ajudar hoje?',
        placeholder: 'Digite sua pergunta...',
        typing: 'Metodix está digitando...'
    },
    zh: {
        title: '🤖 Metodix',
        welcome: '你好！我是 Metodix。今天我能为您提供什么帮助？',
        placeholder: '输入您的问题...',
        typing: 'Metodix 正在输入...'
    }
};

// URL DE TU WEBHOOK DE N8N. Usamos siempre el proxy relativo de Nginx
const N8N_WEBHOOK_URL = "/api/metodix";

// VERSIÓN SEGURA: Usamos "function AssistantWidget" dentro del forwardRef
const AssistantWidget = forwardRef(function AssistantWidget({ lang = 'es' }, ref) {
    const [isOpen, setIsOpen] = useState(false);
    const [inputValue, setInputValue] = useState('');
    const [messages, setMessages] = useState([]);
    const [isLoading, setIsLoading] = useState(false);

    const nodeRef = useRef(null);
    const messagesEndRef = useRef(null);
    const inputRef = useRef(null);

    const t = i18n_assistant[lang] || i18n_assistant.es;

    // Conectamos la orden externa de "abrir" con el estado interno
    useImperativeHandle(ref, () => ({
        open: () => {
            setIsOpen(true);
        }
    }));

    // Auto-scroll al final cuando hay mensajes nuevos
    useEffect(() => {
        if (messagesEndRef.current) {
            messagesEndRef.current.scrollIntoView({ behavior: 'smooth' });
        }
    }, [messages, isOpen, isLoading]);

    // Foco automático en el input al terminar de cargar
    useEffect(() => {
        if (!isLoading && isOpen && inputRef.current) {
            inputRef.current.focus();
        }
    }, [isLoading, isOpen]);

    const toggleAssistant = () => setIsOpen(!isOpen);

    const handleSend = async () => {
        if (!inputValue.trim() || isLoading) return;

        const userText = inputValue;
        setInputValue('');
        setMessages((prev) => [...prev, { role: 'user', text: userText }]);
        setIsLoading(true);

        try {
            const response = await fetch(N8N_WEBHOOK_URL, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                },
                body: JSON.stringify({ question: userText })
            });

            if (!response.ok) throw new Error("Error en la red");

            const data = await response.json();
            const botText = data.output || data.text || data.response || "No se recibió respuesta válida.";

            setMessages((prev) => [...prev, { role: 'bot', text: botText }]);
        } catch (error) {
            console.error("Error conectando con n8n:", error);
            setMessages((prev) => [...prev, { role: 'bot', text: "⚠️ Hubo un error de conexión. Inténtalo de nuevo." }]);
        } finally {
            setIsLoading(false);
        }
    };

    const handleKeyDown = (e) => {
        if (e.key === 'Enter') {
            e.preventDefault();
            handleSend();
        }
    };

    return (
        <>
            {!isOpen && (
                <button className="assistant-fab" onClick={toggleAssistant} title="Ayuda / Help">
                    <span className="assistant-fab-icon">M</span>
                </button>
            )}

            {isOpen && (
                <Draggable nodeRef={nodeRef} handle=".assistant-header">
                    <div ref={nodeRef} className="assistant-window">

                        <div className="assistant-header">
                            <span className="assistant-title">{t.title}</span>
                            <button className="assistant-close-btn" onClick={toggleAssistant}>✖</button>
                        </div>

                        <div className="assistant-body">
                            <div className="assistant-message system-message">{t.welcome}</div>

                            {messages.map((msg, index) => (
                                <div key={index} className={`assistant-message ${msg.role === 'user' ? 'user-message' : 'system-message'}`}>
                                    {msg.text}
                                </div>
                            ))}

                            {isLoading && (
                                <div className="assistant-message system-message typing-indicator">
                                    <em>{t.typing}</em>
                                </div>
                            )}

                            <div ref={messagesEndRef} />
                        </div>

                        <div className="assistant-footer">
                            <input
                                ref={inputRef}
                                type="text"
                                placeholder={t.placeholder}
                                className="assistant-input"
                                value={inputValue}
                                onChange={(e) => setInputValue(e.target.value)}
                                onKeyDown={handleKeyDown}
                                disabled={isLoading}
                            />
                            <button
                                className="assistant-send-btn"
                                onClick={handleSend}
                                disabled={isLoading}
                            >
                                ➤
                            </button>
                        </div>

                    </div>
                </Draggable>
            )}
        </>
    );
});

export default AssistantWidget;