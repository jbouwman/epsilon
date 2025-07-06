/**
 * Lisp and Functional Programming UI Enhancements
 * Inspired by technical documentation for functional languages
 */

document.addEventListener('DOMContentLoaded', function() {
    
    // Add technical metadata to document
    addTechnicalMetadata();
    
    // Enhance code blocks with Lisp-specific styling
    enhanceLispCodeBlocks();
    
    // Add grid toggle functionality
    addGridToggle();
    
    // Enhance tables with technical styling
    enhanceTables();
    
    // Add function signature highlighting
    highlightFunctionSignatures();
});

function addTechnicalMetadata() {
    // Add document metadata in technical style
    const lastModified = document.lastModified;
    const metaDiv = document.createElement('div');
    metaDiv.className = 'epsilon-doc-metadata';
    metaDiv.innerHTML = `
        <div class="epsilon-metadata">
            <span class="epsilon-doc-id">ε-DOC-${Date.now().toString(36).toUpperCase()}</span>
            <span class="epsilon-revision">REV: ${lastModified.substring(0, 10)}</span>
        </div>
    `;
    
    // Add CSS for metadata
    const style = document.createElement('style');
    style.textContent = `
        .epsilon-doc-metadata {
            position: fixed;
            top: 60px;
            right: 16px;
            font-family: 'IBM Plex Mono', monospace;
            font-size: 9px;
            color: #7b1fa2;
            z-index: 100;
            background: rgba(255, 255, 255, 0.95);
            padding: 4px 8px;
            border: 1px solid #cfd8dc;
            text-transform: uppercase;
            letter-spacing: 0.1em;
            border-left: 3px solid #7b1fa2;
        }
        
        .epsilon-metadata span {
            display: block;
            margin: 2px 0;
        }
        
        .epsilon-doc-id {
            color: #2e7d32;
            font-weight: 600;
        }
        
        @media print {
            .epsilon-doc-metadata {
                position: absolute;
                top: 0;
                right: 0;
                background: white;
            }
        }
        
        @media screen and (max-width: 768px) {
            .epsilon-doc-metadata {
                display: none;
            }
        }
    `;
    
    document.head.appendChild(style);
    document.body.appendChild(metaDiv);
}

function enhanceLispCodeBlocks() {
    const codeBlocks = document.querySelectorAll('pre code');
    
    codeBlocks.forEach((block, index) => {
        const pre = block.parentElement;
        const language = block.className.replace('language-', '').toLowerCase();
        
        // Detect Lisp-like languages
        const isLisp = language.includes('lisp') || 
                      language.includes('clojure') || 
                      language.includes('scheme') || 
                      language.includes('racket') ||
                      block.textContent.includes('(def') ||
                      block.textContent.includes('(lambda');
        
        // Add code block header with language-specific styling
        const header = document.createElement('div');
        header.className = 'epsilon-code-header';
        header.innerHTML = `
            <span class="epsilon-code-label">LISTING ${String(index + 1).padStart(3, '0')}</span>
            <span class="epsilon-code-lang ${isLisp ? 'lisp-lang' : ''}">${language.toUpperCase() || 'CODE'}</span>
            ${isLisp ? '<span class="epsilon-lisp-indicator">λ</span>' : ''}
        `;
        
        pre.insertBefore(header, block);
        pre.classList.add('epsilon-code-block');
        
        if (isLisp) {
            pre.classList.add('epsilon-lisp-block');
            highlightLispSyntax(block);
        }
    });
    
    // Add CSS for enhanced code blocks
    const style = document.createElement('style');
    style.textContent = `
        .epsilon-code-block {
            border-radius: 0 !important;
            position: relative;
            margin: 24px 0 !important;
            border-left: 4px solid #2e7d32 !important;
        }
        
        .epsilon-lisp-block {
            border-left-color: #2e7d32 !important;
            background: linear-gradient(to right, rgba(46, 125, 50, 0.03) 0%, #f8f9fa 4px) !important;
        }
        
        .epsilon-code-header {
            background: #37474f;
            color: #7b1fa2;
            font-family: 'IBM Plex Mono', monospace;
            font-size: 9px;
            text-transform: uppercase;
            letter-spacing: 0.1em;
            padding: 4px 12px;
            border-bottom: 1px solid #7b1fa2;
            display: flex;
            justify-content: space-between;
            align-items: center;
            font-weight: 600;
        }
        
        .epsilon-code-label {
            color: #cfd8dc;
        }
        
        .epsilon-code-lang {
            color: #7b1fa2;
        }
        
        .epsilon-code-lang.lisp-lang {
            color: #2e7d32;
        }
        
        .epsilon-lisp-indicator {
            color: #2e7d32;
            font-size: 12px;
            font-weight: bold;
            margin-left: 8px;
        }
    `;
    
    document.head.appendChild(style);
}

function highlightLispSyntax(codeBlock) {
    const text = codeBlock.textContent;
    
    // Basic Lisp syntax highlighting
    let highlighted = text
        // Highlight function definitions
        .replace(/(defun|defmacro|defclass|defmethod|defvar|defparameter)\s+([^\s\)]+)/g, 
                '<span class="lisp-definer">$1</span> <span class="lisp-name">$2</span>')
        // Highlight special forms
        .replace(/\b(let|let\*|lambda|if|when|unless|cond|case|do|loop|block|return-from)\b/g, 
                '<span class="lisp-special">$1</span>')
        // Highlight package names
        .replace(/([a-zA-Z-]+):([a-zA-Z-]+)/g, 
                '<span class="lisp-package">$1</span>:<span class="lisp-symbol">$2</span>');
    
    if (highlighted !== text) {
        codeBlock.innerHTML = highlighted;
    }
    
    // Add CSS for Lisp syntax highlighting
    const style = document.createElement('style');
    style.textContent = `
        .lisp-definer {
            color: #7b1fa2;
            font-weight: 600;
        }
        
        .lisp-name {
            color: #1976d2;
            font-weight: 500;
        }
        
        .lisp-special {
            color: #ff6f00;
            font-weight: 500;
        }
        
        .lisp-package {
            color: #7b1fa2;
            font-weight: 500;
        }
        
        .lisp-symbol {
            color: #2e7d32;
            font-weight: 500;
        }
    `;
    
    if (!document.getElementById('lisp-syntax-style')) {
        style.id = 'lisp-syntax-style';
        document.head.appendChild(style);
    }
}

function addGridToggle() {
    // Add grid toggle button
    const toggleButton = document.createElement('button');
    toggleButton.className = 'epsilon-grid-toggle';
    toggleButton.innerHTML = 'GRID';
    toggleButton.title = 'Toggle construction grid';
    
    let gridVisible = true;
    
    toggleButton.addEventListener('click', function() {
        gridVisible = !gridVisible;
        document.body.style.setProperty('--epsilon-grid-opacity', gridVisible ? '0.08' : '0');
        toggleButton.textContent = gridVisible ? 'GRID' : 'NO GRID';
        toggleButton.classList.toggle('inactive', !gridVisible);
    });
    
    // Add CSS for grid toggle
    const style = document.createElement('style');
    style.textContent = `
        .epsilon-grid-toggle {
            position: fixed;
            bottom: 24px;
            right: 24px;
            background: #37474f;
            color: #7b1fa2;
            border: 1px solid #7b1fa2;
            padding: 6px 12px;
            font-family: 'IBM Plex Mono', monospace;
            font-size: 9px;
            text-transform: uppercase;
            letter-spacing: 0.1em;
            cursor: pointer;
            z-index: 1000;
            transition: all 0.2s ease;
            font-weight: 600;
        }
        
        .epsilon-grid-toggle:hover {
            background: #7b1fa2;
            color: #37474f;
        }
        
        .epsilon-grid-toggle.inactive {
            opacity: 0.5;
        }
        
        body::before {
            opacity: var(--epsilon-grid-opacity, 0.08);
            transition: opacity 0.3s ease;
        }
        
        @media screen and (max-width: 768px) {
            .epsilon-grid-toggle {
                bottom: 16px;
                right: 16px;
                padding: 4px 8px;
                font-size: 8px;
            }
        }
        
        @media print {
            .epsilon-grid-toggle {
                display: none;
            }
        }
    `;
    
    document.head.appendChild(style);
    document.body.appendChild(toggleButton);
}

function enhanceTables() {
    const tables = document.querySelectorAll('table');
    
    tables.forEach((table, index) => {
        // Wrap table in container for technical styling
        const wrapper = document.createElement('div');
        wrapper.className = 'epsilon-table-wrapper';
        
        const header = document.createElement('div');
        header.className = 'epsilon-table-header';
        header.innerHTML = `
            <span class="epsilon-table-label">TABLE ${String(index + 1).padStart(2, '0')}</span>
            <span class="epsilon-table-rows">${table.rows.length} ROWS</span>
        `;
        
        table.parentNode.insertBefore(wrapper, table);
        wrapper.appendChild(header);
        wrapper.appendChild(table);
    });
    
    // Add CSS for enhanced tables
    const style = document.createElement('style');
    style.textContent = `
        .epsilon-table-wrapper {
            border: 2px solid #cfd8dc;
            margin: 24px 0;
            background: white;
        }
        
        .epsilon-table-header {
            background: #37474f;
            color: #7b1fa2;
            font-family: 'IBM Plex Mono', monospace;
            font-size: 9px;
            text-transform: uppercase;
            letter-spacing: 0.1em;
            padding: 6px 12px;
            display: flex;
            justify-content: space-between;
            font-weight: 600;
            border-bottom: 1px solid #7b1fa2;
        }
        
        .epsilon-table-label {
            color: #cfd8dc;
        }
        
        .epsilon-table-wrapper table {
            margin: 0 !important;
            border: none !important;
            width: 100%;
        }
    `;
    
    document.head.appendChild(style);
}

function highlightFunctionSignatures() {
    // Find and enhance function signatures in the documentation
    const headings = document.querySelectorAll('h3, h4');
    
    headings.forEach(heading => {
        const text = heading.textContent;
        
        // Look for function-like signatures
        if (text.includes('(') && text.includes(')')) {
            heading.classList.add('function-signature');
            
            // Extract function name and arguments
            const match = text.match(/([a-zA-Z-]+)\s*\(([^)]*)\)/);
            if (match) {
                const [full, funcName, args] = match;
                heading.innerHTML = text.replace(
                    full,
                    `<span class="func-name">${funcName}</span><span class="func-args">(${args})</span>`
                );
            }
        }
    });
    
    // Add CSS for function signatures
    const style = document.createElement('style');
    style.textContent = `
        .function-signature {
            background: rgba(46, 125, 50, 0.05);
            border-left: 3px solid #2e7d32;
            padding: 8px 16px;
            margin: 16px 0;
            font-family: 'IBM Plex Mono', monospace !important;
        }
        
        .func-name {
            color: #2e7d32;
            font-weight: 600;
        }
        
        .func-args {
            color: #1976d2;
            font-weight: 400;
        }
    `;
    
    document.head.appendChild(style);
}