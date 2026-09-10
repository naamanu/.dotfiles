local o = vim.o
local opt = vim.opt

-- Line numbers
o.number = true
o.relativenumber = true

-- Tabs & indentation (C, C++ and Rust use 4 via after/ftplugin/)
o.tabstop = 2
o.shiftwidth = 2
o.expandtab = true
o.autoindent = true
o.smartindent = true

-- Line wrapping
o.wrap = false

-- Search settings
o.ignorecase = true
o.smartcase = true
o.hlsearch = true
o.incsearch = true

-- Cursor line
o.cursorline = true

-- Appearance. `background` belongs to theme.lua (the shared light/dark
-- state); setting it here flashed dark before the theme loaded in light mode.
o.termguicolors = true
o.signcolumn = "yes"
o.colorcolumn = "100"
opt.fillchars:append({ eob = " " })
-- One border for every floating window (hover, diagnostics, signature help,
-- pickers); plugins that pass their own `border` still win.
o.winborder = "rounded"

-- Backspace
o.backspace = "indent,eol,start"

-- Clipboard
opt.clipboard:append("unnamedplus")

-- Split windows
o.splitright = true
o.splitbelow = true

-- Swap, backup and undo (undo history lives in stdpath("state")/undo)
o.swapfile = false
o.backup = false
o.undofile = true

-- Pick up files changed outside Neovim; autocmds.lua runs :checktime on focus.
o.autoread = true

-- Mouse
o.mouse = "a"

-- Performance
o.updatetime = 250
o.timeoutlen = 300

-- Completion
o.completeopt = "menu,menuone,noinsert"

-- Scrolling
o.scrolloff = 8
o.sidescrolloff = 8

-- Show invisible characters
o.list = true
opt.listchars = { tab = "» ", trail = "·", nbsp = "␣" }

-- File encoding
o.fileencoding = "utf-8"

-- Sessions: what <leader><tab>s / <leader><tab>o (keymaps.lua) persist.
-- No `options`/`blank`, so restoring a session does not replay stale settings.
o.sessionoptions = "buffers,curdir,folds,help,tabpages,winsize,globals,skiprtp"

-- Disable unused providers
vim.g.loaded_perl_provider = 0

-- Python provider: dedicated uv venv (created by setup.sh)
local python_venv = vim.fn.expand("~/.local/share/nvim/venv/bin/python")
if vim.fn.filereadable(python_venv) == 1 then
  vim.g.python3_host_prog = python_venv
end

-- Ruby provider: unused, disable to avoid warnings
vim.g.loaded_ruby_provider = 0
