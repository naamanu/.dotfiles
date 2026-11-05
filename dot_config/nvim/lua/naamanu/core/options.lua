local o = vim.o
local opt = vim.opt

-- Hybrid line numbers: relative everywhere, the real number on the cursor line.
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
-- Always one sign column, so text does not shift when git or diagnostic signs
-- appear.
o.signcolumn = "yes"
-- No command line until it is needed, and one statusline for the whole screen.
o.cmdheight = 0
o.laststatus = 3
-- Popup menu: at most 10 rows, slightly see-through.
o.pumheight = 10
o.pumblend = 10
-- Flash the matching bracket for 0.2s when one is typed.
o.showmatch = true
o.matchtime = 2
-- Window title "file.go [+] - nvim", so terminal tabs show the file (tmux
-- forwards it: set-titles in ~/.tmux.conf).
o.title = true
o.titlestring = "%t%( %M%) - nvim"
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

-- Completion (built-in, LSP-driven; core/lsp.lua): fuzzy matching, nothing
-- inserted until chosen, documentation in a popup beside the menu.
o.completeopt = "menuone,noselect,popup,fuzzy"
-- Show the menu while typing words too, not only after `.`/`::`: the LSP
-- (omnifunc, set on attach) first, then words from the current buffer.
o.autocomplete = true
o.complete = "o,."
opt.wildoptions:append("fuzzy")

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

-- Start on a plain empty buffer: no intro screen.
vim.opt.shortmess:append("I")

-- Splits (terminals, quickfix) open and close without scrolling the main
-- windows.
o.splitkeep = "screen"

-- Folds: treesitter supplies them (core/plugins.lua sets foldexpr where a
-- parser attaches). Level 99 so every file opens unfolded; zc/za/zM still
-- close them along the syntax tree. foldtext="" keeps the folded line's own
-- syntax highlighting instead of the "+-- N lines" summary.
o.foldlevel = 99
o.foldlevelstart = 99
o.foldtext = ""
opt.fillchars:append({ fold = " " })

-- :grep uses ripgrep and fills the quickfix list.
if vim.fn.executable("rg") == 1 then
  o.grepprg = "rg --vimgrep --smart-case"
  o.grepformat = "%f:%l:%c:%m"
end

-- Built-in ui2 (experimental in 0.12): messages and the cmdline render in
-- their own windows. Where each message goes (|ui-messages| kinds):
--   msg    short messages: a corner popup, at most 30% tall, gone after 5s
--   pager  long output (errors, :ls/:set listings, :verbose, :!cmd and its
--          output): scrollable, at most half the screen. Opening it moves
--          the cursor into it, so LSP progress (sent on every format-on-save)
--          goes to msg instead.
--   cmd    search count, completion/wildmenu lists, confirm prompts, and the
--          output of commands typed on the cmdline
-- Long output collapses to `[+N]`; g< reopens it in the pager. The centered
-- cmdline box itself is tiny-cmdline.nvim (core/plugins.lua).
local ui2_ok, ui2 = pcall(require, "vim._core.ui2")
if ui2_ok then
  local pager_kinds = {
    list_cmd = true,
    verbose = true,
    shell_cmd = true,
    shell_out = true,
    shell_err = true,
    shell_ret = true,
    emsg = true,
    echoerr = true,
    lua_error = true,
    rpc_error = true,
  }
  local targets = {
    typed_cmd = "cmd",
    search_count = "cmd",
    search_count_cmd = "cmd", -- see the msg_show wrapper below
    wildlist = "cmd",
    completion = "cmd",
    confirm = "cmd",
  }
  for kind in pairs(pager_kinds) do
    targets[kind] = "pager"
  end
  ui2.enable({
    msg = {
      target = "msg", -- every kind not listed in `targets`
      targets = targets,
      msg = { height = 0.3, timeout = 5000 },
      pager = { height = 0.5 },
      cmd = { height = 0.5 },
    },
  })
  -- ui2 looks up the trigger before the kind, so `typed_cmd = "cmd"` would
  -- also catch a typed :ls or :!cmd. Drop the trigger for the pager kinds so
  -- long output from a typed command still lands in the pager.
  local msgs = require("vim._core.ui2.messages")
  local msg_show = msgs.msg_show
  msgs.msg_show = function(kind, content, replace_last, history, append, id, trigger)
    if trigger == "typed_cmd" and pager_kinds[kind] then
      trigger = ""
    end
    -- With cmdheight=0 ui2 writes the search count into the hidden cmdline
    -- window, so it never shows. Send just the "[1/5]" part as a cmdline
    -- message instead.
    if kind == "search_count" and vim.o.cmdheight == 0 then
      local last = content[#content]
      local count = last and last[2]:match("W? %[>?%d*%??/>?%d*%?*%]")
      if count then
        kind, content = "search_count_cmd", { { last[1], vim.trim(count), last[3] } }
      end
    end
    return msg_show(kind, content, replace_last, history, append, id, trigger)
  end
end

-- Disable unused providers
vim.g.loaded_perl_provider = 0

-- Python provider: dedicated uv venv (created by setup.sh)
local python_venv = vim.fn.expand("~/.local/share/nvim/venv/bin/python")
if vim.fn.filereadable(python_venv) == 1 then
  vim.g.python3_host_prog = python_venv
end

-- Ruby provider: unused, disable to avoid warnings
vim.g.loaded_ruby_provider = 0
