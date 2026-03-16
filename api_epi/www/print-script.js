// Listen for messages from the parent React Portal
// We use a global variable to store the latest lang/theme in case Shiny isn't ready yet
window._pendingLang = null;
window._pendingTheme = null;

window.addEventListener('message', function(event) {
    if (event.data && event.data.type) {
        if (event.data.type === 'lang') {
            console.log("Iframe: Received lang update:", event.data.value);
            if (window.Shiny && Shiny.setInputValue) {
                Shiny.setInputValue('lang', event.data.value, {priority: 'event'});
            } else {
                window._pendingLang = event.data.value;
                console.log("Iframe: Shiny not ready, stored pending lang:", event.data.value);
            }
        } else if (event.data.type === 'theme') {
            const theme = event.data.value;
            document.documentElement.setAttribute('data-bs-theme', theme);
            if (window.Shiny && Shiny.setInputValue) {
                Shiny.setInputValue('current_theme', theme, {priority: 'event'});
            } else {
                window._pendingTheme = theme;
            }
        } else if (event.data.type === 'print') {
            window.print();
        }
    }
});

// Main initialization when Shiny is connected
$(document).on('shiny:connected', function(event) {
    console.log("Iframe: Shiny connected!");
    
    // Process pending updates
    if (window._pendingLang) {
        Shiny.setInputValue('lang', window._pendingLang, {priority: 'event'});
        window._pendingLang = null;
    }
    if (window._pendingTheme) {
        Shiny.setInputValue('current_theme', window._pendingTheme, {priority: 'event'});
        window._pendingTheme = null;
    }

    // Register Custom Message Handlers
    Shiny.addCustomMessageHandler('updateNavTitles', function(message) {
        for (var id in message) {
            var el = document.getElementById(id);
            if (el) {
                el.textContent = message[id];
            }
        }
    });

    Shiny.addCustomMessageHandler('updatePrintButton', function (text) {
        var btnText = document.getElementById('print_btn_text');
        if (btnText) btnText.textContent = text;
    });

    Shiny.addCustomMessageHandler('setTheme', function (theme) {
        document.documentElement.setAttribute('data-bs-theme', theme);
    });

    Shiny.addCustomMessageHandler('updatePrintTitle', function (title) { window.printTitle = title; });
    Shiny.addCustomMessageHandler('updatePrintHeader', function (header) { window.printHeader = header; });
    Shiny.addCustomMessageHandler('updatePrintPreviewTitle', function (title) { window.printPreviewTitle = title; });
    Shiny.addCustomMessageHandler('updatePrintBtnClose', function (text) { window.printBtnClose = text; });
    Shiny.addCustomMessageHandler('updatePrintBtnPrint', function (text) { window.printBtnPrint = text; });
});

$(document).ready(function () {
    // Dropdown hover behavior (Desktop only)
    $('.dropdown-toggle').on('click focus', function (e) {
        if (window.matchMedia('(min-width: 768px)').matches) {
            e.preventDefault();
            e.stopPropagation();
            $(this).removeClass('show');
            $(this).next('.dropdown-menu').removeClass('show');
            $(this).blur();
        }
    });

    $('.dropdown-item').on('click', function () {
        $('.dropdown-toggle').removeClass('show').blur();
        $('.dropdown-menu').removeClass('show');
    });

    // Sidebar/Home tab state logic
    function checkHomeTab() {
        var homePane = document.querySelector('.tab-pane.active');
        if (homePane && (homePane.querySelector('#home_content') || homePane.querySelector('.home-container'))) {
            document.body.classList.add('home-active');
        } else {
            document.body.classList.remove('home-active');
        }
    }

    checkHomeTab();
    var tabObserver = new MutationObserver(checkHomeTab);
    var tabContent = document.querySelector('.tab-content');
    if (tabContent) tabObserver.observe(tabContent, { attributes: true, subtree: true, attributeFilter: ['class'] });

    $(document).on('shown.bs.tab', function () {
        checkHomeTab();
        updatePrintButtonState();
    });

    // Print button state
    function updatePrintButtonState() {
        var printBtn = document.getElementById('print_results');
        if (!printBtn) return;
        var isHome = document.body.classList.contains('home-active');
        if (isHome) {
            printBtn.disabled = true;
            printBtn.classList.add('disabled');
        } else {
            printBtn.disabled = false;
            printBtn.classList.remove('disabled');
        }
    }
    setTimeout(updatePrintButtonState, 500);

    // Robust Printing logic
    $(document).on('click', '#print_results', function () {
        if (this.disabled) return;
        
        var contentToPrint = null;
        var visibleTemplate = Array.from(document.querySelectorAll('.print-template')).find(el => {
            var container = el.parentElement;
            while (container && container !== document.body) {
                if (container.classList.contains('tab-pane')) return container.offsetWidth > 0;
                container = container.parentElement;
            }
            return false;
        });

        if (visibleTemplate) {
            contentToPrint = visibleTemplate.cloneNode(true);
            contentToPrint.style.display = 'block';
        } else {
            var visibleActive = Array.from(document.querySelectorAll('.tab-pane.active')).reverse().find(el => el.offsetWidth > 0);
            if (visibleActive) {
                var mainPanel = visibleActive.querySelector('.col-sm-8, .col-sm-9, .mainPanel, .main-panel') || visibleActive;
                contentToPrint = mainPanel.cloneNode(true);
            }
        }

        if (!contentToPrint) return;

        // Cleanup before print
        contentToPrint.querySelectorAll('.shiny-input-container, script, style, button, .sidebarPanel, .sidebar-panel, .utils-menu, .exclude-print').forEach(el => el.remove());
        contentToPrint.querySelectorAll('*').forEach(el => { if (el.style.display === 'none') el.remove(); });

        var winWidth = 900, winHeight = 700;
        var printWindow = window.open('', '_blank', `width=${winWidth},height=${winHeight}`);
        
        printWindow.document.write(`
            <!DOCTYPE html><html><head>
            <title>${window.printTitle || 'Print'}</title>
            <link rel="stylesheet" href="custom.css">
            <style>
                body { font-family: sans-serif; padding: 40px; color: #333; margin-top: 50px; }
                .print-controls { position: fixed; top: 0; left: 0; right: 0; background: #2c3e50; color: white; padding: 10px 20px; display: flex; justify-content: space-between; align-items: center; z-index: 1000; }
                .print-btn { background: #e74c3c; color: white; border: none; padding: 8px 16px; border-radius: 4px; cursor: pointer; }
                .close-btn { background: transparent; border: 1px solid white; color: white; padding: 8px 16px; border-radius: 4px; margin-right: 10px; cursor: pointer; }
                @media print { .print-controls { display: none !important; } .print-template { display: block !important; } }
            </style>
            </head><body>
            <div class="print-controls">
                <div><strong>${window.printPreviewTitle || 'Print Preview'}</strong></div>
                <div><button class="close-btn" onclick="window.close()">${window.printBtnClose || 'Close'}</button><button class="print-btn" onclick="window.print()">🖨️ ${window.printBtnPrint || 'Print'}</button></div>
            </div>
            <div class="print-template">${contentToPrint.innerHTML}</div>
            </body></html>
        `);
        printWindow.document.close();
    });
});