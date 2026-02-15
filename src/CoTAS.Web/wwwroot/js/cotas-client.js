// CoTAS Web Client - SignalR terminal interface

const ROWS = 25;
const COLS = 80;

// Screen buffer: 25 rows of 80 chars
let screenBuffer = [];
let connection = null;
let currentProgram = null;
let isRunning = false;

// Initialize screen buffer
function initScreen() {
    screenBuffer = [];
    for (let r = 0; r < ROWS; r++) {
        screenBuffer.push(' '.repeat(COLS));
    }
}

// Render screen buffer to terminal div
function renderScreen() {
    const terminal = document.getElementById('terminal');
    terminal.innerHTML = '';
    for (let r = 0; r < ROWS; r++) {
        const row = document.createElement('span');
        row.className = 'terminal-row';
        row.textContent = screenBuffer[r];
        terminal.appendChild(row);
    }
}

// Write text at position in screen buffer
function writeToBuffer(text, row, col) {
    if (row < 1 || row > ROWS) return;
    let r = row - 1;
    let c = col - 1;
    if (c < 0) c = 0;

    let line = screenBuffer[r];
    while (line.length < COLS) line += ' ';

    let before = line.substring(0, c);
    let after = line.substring(c + text.length);
    screenBuffer[r] = (before + text + after).substring(0, COLS);
}

// Clear screen buffer
function clearScreen() {
    initScreen();
    renderScreen();
}

// Set running state
function setRunning(running, programName) {
    isRunning = running;
    currentProgram = running ? programName : null;

    const indicator = document.getElementById('status-indicator');
    const btnStop = document.getElementById('btn-stop');
    const programLabel = document.getElementById('current-program');

    if (running) {
        indicator.textContent = 'Running';
        indicator.className = 'badge bg-success running';
        btnStop.disabled = false;
        programLabel.textContent = programName || 'Running...';
    } else {
        indicator.textContent = 'Idle';
        indicator.className = 'badge bg-secondary';
        btnStop.disabled = true;
        if (!programName) programLabel.textContent = 'No program loaded';
    }
}

// Load program list from server
async function loadPrograms() {
    try {
        const programs = await connection.invoke('GetPrograms');
        const list = document.getElementById('program-list');
        list.innerHTML = '';

        programs.forEach(name => {
            const item = document.createElement('a');
            item.href = '#';
            item.className = 'list-group-item list-group-item-action';
            item.innerHTML = `<i class="fas fa-file-code me-1 text-muted"></i>${name}`;
            item.dataset.program = name;
            item.addEventListener('click', (e) => {
                e.preventDefault();
                runProgram(name);
            });
            list.appendChild(item);
        });

        document.getElementById('footer-status').textContent = `${programs.length} programs available`;
    } catch (err) {
        console.error('Failed to load programs:', err);
        appendLog('ERROR: Failed to load programs: ' + err.message);
    }
}

// Filter program list
function filterPrograms() {
    const filter = document.getElementById('program-filter').value.toLowerCase();
    const items = document.querySelectorAll('#program-list .list-group-item');
    items.forEach(item => {
        const name = item.dataset.program.toLowerCase();
        item.style.display = name.includes(filter) ? '' : 'none';
    });
}

// Run a program
async function runProgram(fileName) {
    // Highlight selected program
    document.querySelectorAll('#program-list .list-group-item').forEach(item => {
        item.classList.toggle('active', item.dataset.program === fileName);
    });

    clearScreen();
    setRunning(true, fileName);
    appendLog(`Starting ${fileName}...`);

    try {
        await connection.invoke('RunProgram', fileName);
    } catch (err) {
        appendLog(`ERROR invoking RunProgram: ${err.message}`);
        showErrorOnTerminal(`RunProgram failed: ${err.message}`);
        setRunning(false);
    }
}

// Stop current program
async function stopProgram() {
    try {
        await connection.invoke('StopProgram');
    } catch (err) {
        console.error('Failed to stop:', err);
    }
}

// Submit input value
async function submitInput(value) {
    try {
        await connection.invoke('SubmitInput', value);
    } catch (err) {
        console.error('Failed to submit input:', err);
    }
}

// Show input field at position
function showInputField(fieldName, row, col, size) {
    const terminal = document.getElementById('terminal');
    const overlay = document.getElementById('input-overlay');
    const input = document.getElementById('input-field');

    const charWidth = terminal.scrollWidth / COLS;
    const lineHeight = terminal.scrollHeight / ROWS;

    const r = Math.max(0, row - 1);
    const c = Math.max(0, col - 1);

    input.style.left = (4 + c * charWidth) + 'px';
    input.style.top = (4 + r * lineHeight) + 'px';
    input.style.width = Math.min(size, COLS - c) * charWidth + 'px';
    input.style.height = lineHeight + 'px';
    input.maxLength = size;
    input.value = '';
    input.placeholder = fieldName;

    overlay.className = '';
    overlay.style.position = 'absolute';
    overlay.style.zIndex = '10';

    terminal.style.position = 'relative';
    terminal.appendChild(overlay);

    input.focus();

    return new Promise((resolve) => {
        const handleSubmit = (e) => {
            if (e.key === 'Enter') {
                e.preventDefault();
                input.removeEventListener('keydown', handleSubmit);
                overlay.className = 'd-none';
                terminal.removeChild(overlay);
                resolve(input.value);
            }
        };
        input.addEventListener('keydown', handleSubmit);
    });
}

// Show ask dialog
function showAskDialog(prompt, defaultValue) {
    const terminal = document.getElementById('terminal');

    const overlay = document.createElement('div');
    overlay.className = 'ask-overlay';

    const dialog = document.createElement('div');
    dialog.className = 'ask-dialog';
    dialog.innerHTML = `
        <div class="mb-2">${prompt}</div>
        <button id="ask-yes">Yes (Y)</button>
        <button id="ask-no">No (N)</button>
    `;
    overlay.appendChild(dialog);

    terminal.style.position = 'relative';
    terminal.appendChild(overlay);

    return new Promise((resolve) => {
        dialog.querySelector('#ask-yes').addEventListener('click', () => {
            terminal.removeChild(overlay);
            resolve('Y');
        });
        dialog.querySelector('#ask-no').addEventListener('click', () => {
            terminal.removeChild(overlay);
            resolve('N');
        });

        const handleKey = (e) => {
            if (e.key.toUpperCase() === 'Y' || e.key === 'Enter') {
                document.removeEventListener('keydown', handleKey);
                terminal.removeChild(overlay);
                resolve('Y');
            } else if (e.key.toUpperCase() === 'N' || e.key === 'Escape') {
                document.removeEventListener('keydown', handleKey);
                terminal.removeChild(overlay);
                resolve('N');
            }
        };
        document.addEventListener('keydown', handleKey);
    });
}

// Show error message directly on the terminal screen
function showErrorOnTerminal(msg) {
    // Write error to first two rows of terminal in red
    const errText = msg.substring(0, COLS);
    writeToBuffer(errText, 1, 1);
    renderScreen();

    // Also make the first row red
    const terminal = document.getElementById('terminal');
    const firstRow = terminal.querySelector('.terminal-row');
    if (firstRow) firstRow.style.color = '#ff4444';
}

// Append to message log (always visible)
function appendLog(msg) {
    const log = document.getElementById('message-log');
    const entry = document.createElement('div');
    const isError = msg.includes('ERROR');
    entry.className = isError ? 'text-danger' : '';
    entry.textContent = `[${new Date().toLocaleTimeString()}] ${msg}`;
    log.appendChild(entry);
    log.scrollTop = log.scrollHeight;

    // Also update footer
    document.getElementById('footer-status').textContent = msg;
}

// Initialize SignalR connection
async function initConnection() {
    connection = new signalR.HubConnectionBuilder()
        .withUrl('/hub/interpreter')
        .withAutomaticReconnect()
        .configureLogging(signalR.LogLevel.Information)
        .build();

    // Server -> Client: screen updates
    connection.on('ClearScreen', () => {
        clearScreen();
    });

    connection.on('Say', (text, row, col) => {
        writeToBuffer(text, row, col);
        renderScreen();
    });

    connection.on('Message', (text) => {
        appendLog(text);
    });

    connection.on('RequestInput', async (fieldName, row, col, size) => {
        const value = await showInputField(fieldName, row, col, size);
        writeToBuffer(value, row, col);
        renderScreen();
        await submitInput(value);
    });

    connection.on('RequestAsk', async (prompt, defaultValue) => {
        const value = await showAskDialog(prompt, defaultValue);
        await submitInput(value);
    });

    connection.on('ProgramStarted', (fileName) => {
        setRunning(true, fileName);
        appendLog(`Program started: ${fileName}`);
    });

    connection.on('ProgramEnded', (fileName) => {
        setRunning(false, fileName);
        appendLog(`Program ended: ${fileName}`);
        document.getElementById('footer-status').textContent = `${fileName} completed`;
    });

    connection.on('ProgramStopped', (fileName) => {
        setRunning(false, fileName);
        appendLog(`Program stopped: ${fileName}`);
    });

    connection.on('Error', (message) => {
        appendLog(`ERROR: ${message}`);
        showErrorOnTerminal(message);
        setRunning(false);
    });

    connection.onreconnecting(() => {
        document.getElementById('status-indicator').textContent = 'Reconnecting...';
        document.getElementById('status-indicator').className = 'badge bg-warning';
    });

    connection.onreconnected(() => {
        document.getElementById('status-indicator').textContent = 'Connected';
        document.getElementById('status-indicator').className = 'badge bg-info';
        setTimeout(() => {
            if (!isRunning) {
                document.getElementById('status-indicator').textContent = 'Idle';
                document.getElementById('status-indicator').className = 'badge bg-secondary';
            }
        }, 2000);
    });

    try {
        await connection.start();
        appendLog('SignalR connected');
        await loadPrograms();
    } catch (err) {
        console.error('SignalR connection failed:', err);
        appendLog('ERROR: SignalR connection failed: ' + err.message);
        document.getElementById('footer-status').textContent = 'Connection failed - refresh to retry';
    }
}

// Wire up UI events
document.addEventListener('DOMContentLoaded', () => {
    initScreen();
    renderScreen();

    document.getElementById('btn-stop').addEventListener('click', stopProgram);
    document.getElementById('btn-clear').addEventListener('click', clearScreen);
    document.getElementById('btn-refresh').addEventListener('click', loadPrograms);
    document.getElementById('program-filter').addEventListener('input', filterPrograms);

    // Keyboard shortcuts for function keys
    document.addEventListener('keydown', (e) => {
        if (!isRunning) return;

        if (e.key.startsWith('F') && !e.key.startsWith('Fo')) {
            e.preventDefault();
            connection.invoke('SendKeyPress', e.key);
        } else if (e.key === 'Escape') {
            e.preventDefault();
            connection.invoke('SendKeyPress', 'Escape');
        }
    });

    initConnection();
});
