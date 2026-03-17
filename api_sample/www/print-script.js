$(document).ready(function () {
    // Prevent click on dropdown toggles (menus open only on hover) ONLY ON DESKTOP
    // On mobile (<768px), we need click to open the menu.
    $('.dropdown-toggle').on('click focus', function (e) {
        if (window.matchMedia('(min-width: 768px)').matches) {
            e.preventDefault();
            e.stopPropagation();
            // Only remove 'show' class, preserve 'active' for navigation
            $(this).removeClass('show');
            $(this).next('.dropdown-menu').removeClass('show');
            $(this).blur(); // Remove focus styling
        }
    });

    // Also handle mousedown to prevent focus before click
    $('.dropdown-toggle').on('mousedown', function (e) {
        e.preventDefault();
    });

    // Close dropdown when a calculator link is clicked
    $('.dropdown-item').on('click', function () {
        // Remove the 'show' class added by Bootstrap
        $('.dropdown-toggle').removeClass('show').blur();
        $('.dropdown-menu').removeClass('show');
    });

    // Close other dropdowns when hovering over a different one
    $('.nav-item.dropdown').on('mouseenter', function () {
        // Close all other open dropdowns
        $('.nav-item.dropdown').not(this).find('.dropdown-toggle').removeClass('show').blur();
        $('.nav-item.dropdown').not(this).find('.dropdown-menu').removeClass('show');
    });

    // Close all dropdowns when mouse leaves the navbar
    $('.navbar-nav').on('mouseleave', function () {
        $('.dropdown-toggle').removeClass('show').blur();
        $('.dropdown-menu').removeClass('show');
    });

    // Handler to batch update text content of elements by ID
    Shiny.addCustomMessageHandler('updateNavTitles', function (message) {
        // message is an object {id: 'New Title', ...}
        for (var id in message) {
            if (message.hasOwnProperty(id)) {
                $('#' + id).text(message[id]);
            }
        }
    });

    // Handler to update print button text
    Shiny.addCustomMessageHandler('updatePrintButton', function (text) {
        var btnText = document.getElementById('print_btn_text');
        if (btnText) {
            btnText.textContent = text;
        }
    });

    // Handler to set theme
    Shiny.addCustomMessageHandler('setTheme', function (theme) {
        document.documentElement.setAttribute('data-bs-theme', theme);
    });

    // Custom Handler for Theme Tooltip with Recursive Search and MutationObserver
    Shiny.addCustomMessageHandler('updateThemeTooltip', function (message) {
        var wrapper = document.getElementById('theme_mode');
        if (!wrapper) return;

        // Find the actual element holding the title (usually a button or input inside)
        // We search for any element with a title attribute, or fallback to the first button
        var target = wrapper.querySelector('[title]');
        if (!target) target = wrapper.querySelector('button');
        if (!target) target = wrapper; // fallback

        // Function to set attributes
        var setAttrs = function () {
            // If bslib uses data-original-title (Bootstrap 4) or title
            if (target.hasAttribute('data-original-title')) {
                target.setAttribute('data-original-title', message);
            }
            target.setAttribute('title', message);
            target.setAttribute('aria-label', message);
        };

        setAttrs();

        // Observer to persist changes
        if (window.themeObserver) window.themeObserver.disconnect();

        window.themeObserver = new MutationObserver(function (mutations) {
            mutations.forEach(function (mutation) {
                if (mutation.type === 'attributes' && (mutation.attributeName === 'title' || mutation.attributeName === 'aria-label' || mutation.attributeName === 'data-original-title')) {
                    var currentTitle = target.getAttribute('title');
                    if (currentTitle !== message) {
                        setAttrs();
                    }
                }
            });
        });

        window.themeObserver.observe(target, { attributes: true });
    });

    // Track active tab and add class to body for home tab styling
    function checkHomeTab() {
        var homePane = document.querySelector('.tab-pane.active');
        if (homePane && (homePane.querySelector('#home_content') || homePane.querySelector('.home-container'))) {
            document.body.classList.add('home-active');
        } else {
            document.body.classList.remove('home-active');
        }
    }

    // Check on page load
    checkHomeTab();

    // Check when tabs change (using MutationObserver)
    var tabObserver = new MutationObserver(function (mutations) {
        checkHomeTab();
    });

    var tabContent = document.querySelector('.tab-content');
    if (tabContent) {
        tabObserver.observe(tabContent, { attributes: true, subtree: true, attributeFilter: ['class'] });
    }

    // Also observe on nav clicks
    $(document).on('shown.bs.tab', function () {
        checkHomeTab();
        updatePrintButton();
    });

    // Force repaint on scroll
    $(window).on('scroll resize', function () {
        if (document.body.classList.contains('home-active')) {
            var homeContent = document.getElementById('home_content');
            if (homeContent) {
                homeContent.style.transform = 'translateZ(0)';
            }
        }
    });

    // Print button functionality
    var printTitle = 'Calcupedev - Imprimir resultados'; // Default Spanish
    var printHeader = 'Calculadora epidemiológica'; // Default Spanish
    var printPreviewTitle = 'Vista Previa de Impresión'; // Default Spanish
    var printBtnClose = 'Cerrar'; // Default Spanish
    var printBtnPrint = 'Imprimir'; // Default Spanish

    Shiny.addCustomMessageHandler('updatePrintTitle', function (title) {
        printTitle = title;
    });
    Shiny.addCustomMessageHandler('updatePrintHeader', function (header) {
        printHeader = header;
    });
    Shiny.addCustomMessageHandler('updatePrintPreviewTitle', function (title) {
        printPreviewTitle = title;
    });
    Shiny.addCustomMessageHandler('updatePrintBtnClose', function (text) {
        printBtnClose = text;
    });
    Shiny.addCustomMessageHandler('updatePrintBtnPrint', function (text) {
        printBtnPrint = text;
    });

    function updatePrintButton() {
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

    // Initial check
    setTimeout(updatePrintButton, 500);

    // Handle print button click - DIRECT PRINT (Simplified)
    $(document).on('click', '#print_results', function () {
        if (!this.disabled) {
            // 1. Identify Content Source - Robust Selection
            var contentToPrint = null;

            // Strategy A: Find the visible .print-template
            var allTemplates = document.querySelectorAll('.print-template');
            var visibleTemplate = Array.from(allTemplates).find(function (el) {
                // A template is visible if its parent hierarchy is visible
                // offsetParent is null if element or any ancestor is display:none
                // since .print-template itself is display:none (via CSS), 
                // we check its parent .tab-pane or container.
                var container = el.parentElement;
                while (container && container !== document.body) {
                    if (container.classList.contains('tab-pane')) {
                        return container.offsetWidth > 0 || container.offsetHeight > 0;
                    }
                    container = container.parentElement;
                }
                return false;
            });

            if (visibleTemplate) {
                contentToPrint = visibleTemplate.cloneNode(true);
                contentToPrint.style.display = 'block';
            } else {
                // Strategy B: Fallback to visible active tab
                var allActive = document.querySelectorAll('.tab-pane.active');
                var visibleActive = Array.from(allActive).reverse().find(el => el.offsetWidth > 0 || el.offsetHeight > 0);

                if (visibleActive) {
                    // Fallback to Main Panel inside visible tab
                    var mainPanel = visibleActive.querySelector('.col-sm-8, .col-sm-9, .mainPanel, .main-panel');
                    if (!mainPanel) mainPanel = visibleActive;
                    contentToPrint = mainPanel.cloneNode(true);
                }
            }

            if (!contentToPrint) return;

            // 2. Process Content: Cleanup
            // Remove interactive elements and inputs
            var toRemove = contentToPrint.querySelectorAll('.shiny-input-container, script, style, button, .sidebarPanel, .sidebar-panel, .utils-menu, .exclude-print');
            toRemove.forEach(function (el) { el.remove(); });

            // Remove elements explicitly hidden via inline style (often conditionals)
            // Note: This matches legacy cleanup logic
            var allElements = contentToPrint.querySelectorAll('*');
            allElements.forEach(function (el) {
                if (el.style.display === 'none') el.remove();
            });

            // 3. Open Print Window
            var winWidth = 900;
            var winHeight = 700;
            var winLeft = (screen.width - winWidth) / 2;
            var winTop = (screen.height - winHeight) / 2;

            var printWindow = window.open('', '_blank', 'width=' + winWidth + ',height=' + winHeight + ',left=' + winLeft + ',top=' + winTop);

            printWindow.document.write('<!DOCTYPE html><html><head>');
            printWindow.document.write('<title>' + printTitle + '</title>');
            // Inject stylesheets
            printWindow.document.write('<link rel="stylesheet" href="custom.css">');
            printWindow.document.write('<style>');
            printWindow.document.write('body { font-family: "Segoe UI", Tahoma, Geneva, Verdana, sans-serif; padding: 40px; color: #333; margin-top: 50px; }');
            printWindow.document.write('h1, h2, h3, h4 { color: #2c3e50; }');
            printWindow.document.write('.printable-section { margin-bottom: 20px; border-bottom: 1px solid #eee; padding-bottom: 10px; }');
            printWindow.document.write('table { border-collapse: collapse; width: 100%; margin: 15px 0; }');
            printWindow.document.write('th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }');
            printWindow.document.write('th { background-color: #f8f9fa; font-weight: 600; }');

            // CRITICAL: Force display of template in the new window (both screen and print)
            printWindow.document.write('.print-template { display: block !important; }');

            // Print Control Bar Styles
            printWindow.document.write('.print-controls { position: fixed; top: 0; left: 0; right: 0; background: #2c3e50; color: white; padding: 10px 20px; display: flex; justify-content: space-between; align-items: center; z-index: 1000; box-shadow: 0 2px 5px rgba(0,0,0,0.2); }');
            printWindow.document.write('.print-btn { background: #e74c3c; color: white; border: none; padding: 8px 16px; border-radius: 4px; cursor: pointer; font-size: 14px; font-weight: bold; }');
            printWindow.document.write('.print-btn:hover { background: #c0392b; }');
            printWindow.document.write('.close-btn { background: transparent; border: 1px solid rgba(255,255,255,0.5); color: white; padding: 8px 16px; border-radius: 4px; cursor: pointer; margin-right: 10px; }');
            printWindow.document.write('.close-btn:hover { background: rgba(255,255,255,0.1); }');

            printWindow.document.write('@media print { @page { margin: 0; size: auto; } body > :not(.print-template) { display: none !important; } .print-template { display: block !important; } body { margin: 0; padding: 2cm !important; } }');
            printWindow.document.write('</style>');

            // Inject Script for Buttons
            printWindow.document.write('<script>');
            printWindow.document.write('function doPrint() { window.print(); }');
            printWindow.document.write('function doClose() { window.close(); }');
            printWindow.document.write('</script>');

            printWindow.document.write('</head><body>');

            // Print Controls
            printWindow.document.write('<div id="print-controls" class="print-controls">');
            printWindow.document.write('<div><strong>' + printPreviewTitle + '</strong></div>');
            printWindow.document.write('<div><button class="close-btn" onclick="doClose()">' + printBtnClose + '</button><button class="print-btn" onclick="doPrint()">🖨️ ' + printBtnPrint + '</button></div>');
            printWindow.document.write('</div>');

            // App Header for Print - Add print-template class to ensure visibility
            printWindow.document.write('<div class="print-template" style="margin-bottom: 30px; border-bottom: 2px solid #2c3e50; padding-bottom: 15px; display: flex; justify-content: space-between; align-items: flex-end;">');
            printWindow.document.write('<div><h1 style="margin: 0; font-size: 24px; color: #2c3e50;">Calcupedev</h1><span style="color: #7f8c8d; font-size: 14px;">' + printHeader + '</span></div>');
            printWindow.document.write('<div style="color: #95a5a6; font-size: 12px;">' + new Date().toLocaleDateString() + '</div>');
            printWindow.document.write('</div>');

            // Content - Use outerHTML to preserve wrapper class if present
            if (contentToPrint.classList.contains('print-template')) {
                printWindow.document.write(contentToPrint.outerHTML);
            } else {
                // Wrap legacy content
                printWindow.document.write('<div class="print-template">');
                printWindow.document.write(contentToPrint.innerHTML);
                printWindow.document.write('</div>');
            }

            printWindow.document.write('</body></html>');
            printWindow.document.close();
        }
    });
});