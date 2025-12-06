const boardEl = document.getElementById('board');
const sizeEl = document.getElementById('board-size');
const safeEl = document.getElementById('safe-moves');
const mustEl = document.getElementById('must-flag');
const floodEl = document.getElementById('flood');
const refreshBtn = document.getElementById('refresh');
const resetBtn = document.getElementById('reset');

let currentState = null;

refreshBtn.addEventListener('click', loadState);
resetBtn.addEventListener('click', resetBoard);
document.addEventListener('DOMContentLoaded', loadState);
document.addEventListener('contextmenu', (e) => {
  if (e.target.classList.contains('cell')) e.preventDefault();
});

async function loadState() {
  try {
    const res = await fetch('/state');
    const data = await res.json();
    currentState = data;
    renderBoard(data);
    renderChips(safeEl, data.safe_moves);
    renderChips(mustEl, data.must_flag);
    renderChips(floodEl, data.flood_from);
  } catch (err) {
    console.error('Failed to load state', err);
    sizeEl.textContent = 'Failed to fetch state';
    boardEl.innerHTML = '';
    currentState = null;
  }
}

function renderBoard(data) {
  const { board, cells } = data;
  sizeEl.textContent = `${board.width} × ${board.height}`;
  boardEl.style.gridTemplateColumns = `repeat(${board.width}, 64px)`;
  boardEl.innerHTML = '';

  const numberColors = {
    1: '#7ce7cb',
    2: '#f7c266',
    3: '#ff9a76',
    4: '#8db5ff'
  };

  cells.forEach(cell => {
    const el = document.createElement('div');
    el.className = 'cell';
    if (cell.opened) el.classList.add('opened');
    if (cell.flagged) el.classList.add('flagged');
    if (cell.mine) el.classList.add('mine');
    if (cell.safe_hint) el.classList.add('safe-hint');
    if (cell.must_flag_hint) el.classList.add('must-hint');

    if (cell.flagged) {
      el.textContent = '🚩';
    } else if (cell.mine) {
      el.textContent = '💣';
    } else if (cell.opened && typeof cell.clue === 'number' && cell.clue > 0) {
      el.textContent = cell.clue;
      el.style.color = numberColors[cell.clue] || '#d9e4ff';
    } else {
      el.textContent = '';
    }

    el.title = `(${cell.x},${cell.y})`;
    el.addEventListener('click', () => sendAction('open', cell.x, cell.y));
    el.addEventListener('auxclick', (e) => {
      if (e.button === 1) {
        e.preventDefault();
        sendAction('flag', cell.x, cell.y);
      }
    });
    el.addEventListener('contextmenu', (e) => {
      e.preventDefault();
      sendAction('flag', cell.x, cell.y);
    });
    boardEl.appendChild(el);
  });
}

function renderChips(container, coords) {
  container.innerHTML = '';
  if (!coords.length) {
    const empty = document.createElement('span');
    empty.className = 'chip';
    empty.textContent = '—';
    container.appendChild(empty);
    return;
  }
  coords.forEach(([x, y]) => {
    const chip = document.createElement('span');
    chip.className = 'chip';
    chip.textContent = `(${x},${y})`;
    container.appendChild(chip);
  });
}

async function sendAction(action, x, y) {
  try {
    const res = await fetch('/click', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ action, x, y })
    });
    if (!res.ok) throw new Error(`Action failed: ${res.status}`);
    const data = await res.json();
    currentState = data;
    renderBoard(data);
    renderChips(safeEl, data.safe_moves);
    renderChips(mustEl, data.must_flag);
    renderChips(floodEl, data.flood_from);
  } catch (err) {
    console.error(err);
  }
}

async function resetBoard() {
  try {
    const res = await fetch('/reset', { method: 'POST' });
    if (!res.ok) throw new Error(`Reset failed: ${res.status}`);
    const data = await res.json();
    currentState = data;
    renderBoard(data);
    renderChips(safeEl, data.safe_moves);
    renderChips(mustEl, data.must_flag);
    renderChips(floodEl, data.flood_from);
  } catch (err) {
    console.error(err);
  }
}
