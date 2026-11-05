-- Plugins: the few things stock Neovim cannot do, managed by the built-in
-- vim.pack (revisions pinned in nvim-pack-lock.json at the config root).
-- Update with :lua vim.pack.update(), review, then :write to confirm.
--
--   nvim-treesitter   parsers for python/ts/js/sql/ocaml/haskell/go (Neovim
--                     ships only c, lua, markdown, vim, vimdoc, query)
--   oil.nvim          directories as editable buffers
--   snacks.nvim       picker (files, grep, LSP, GitHub) and terminals
--   gitsigns.nvim     hunks in the sign column, stage/reset/blame
--   vim-tmux-navigator  C-h/j/k/l across Neovim windows and tmux panes
--   nvim-dap          debugging (Delve for Go); configured in core/dap.lua
--   tiny-cmdline.nvim the ui2 cmdline as a centered floating box
--   mini.icons        file/filetype/LSP-kind icons; also stands in for
--                     nvim-web-devicons, so plugins asking for it get these
--   lualine.nvim      the global statusline
--   incline.nvim      a file-name tag in each window's top-right corner
--   todo-comments.nvim  TODO/FIXME/HACK/... highlighted (needs plenary.nvim)
--   nvim-highlight-colors  color literals painted as their own background
--   mason.nvim        installs language servers and formatters (its bin dir
--                     goes first on PATH); mason-tool-installer keeps the list
--   nvim-lspconfig    default server configs (lsp/*.lua); ours override them
--                     from after/lsp/
--   conform.nvim      format on save (formatters, LSP fallback); configured
--                     in core/format.lua
--   mini.pairs        auto-closing brackets and quotes (insert + cmdline)
--   blink.cmp         completion menu (LSP, path, snippets, buffer, cmdline);
--                     pinned to 1.x, its prebuilt fuzzy matcher is fetched
--                     for the release tag

-- :TSUpdate on PackChanged: nvim-treesitter's parsers must be rebuilt whenever
-- the plugin is installed or updated (update() is what :TSUpdate runs; waited
-- on so the parsers exist before first use). Registered before vim.pack.add
-- so it also fires on the first install.
vim.api.nvim_create_autocmd("PackChanged", {
  group = vim.api.nvim_create_augroup("pack-hooks", { clear = true }),
  callback = function(ev)
    local name, kind = ev.data.spec.name, ev.data.kind
    if name == "nvim-treesitter" and (kind == "install" or kind == "update") then
      if not ev.data.active then
        vim.cmd.packadd("nvim-treesitter")
      end
      require("nvim-treesitter").update(nil, { summary = true }):wait(300000)
    end
  end,
})

-- oil replaces netrw as the directory browser.
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

vim.pack.add({
  { src = "https://github.com/nvim-treesitter/nvim-treesitter", version = "main" },
  "https://github.com/stevearc/oil.nvim",
  "https://github.com/folke/snacks.nvim",
  "https://github.com/lewis6991/gitsigns.nvim",
  "https://github.com/christoomey/vim-tmux-navigator",
  "https://github.com/mfussenegger/nvim-dap",
  "https://github.com/rachartier/tiny-cmdline.nvim",
  "https://github.com/nvim-mini/mini.icons",
  "https://github.com/nvim-mini/mini.pairs",
  "https://github.com/stevearc/conform.nvim",
  "https://github.com/nvim-lualine/lualine.nvim",
  "https://github.com/b0o/incline.nvim",
  "https://github.com/nvim-lua/plenary.nvim",
  "https://github.com/mason-org/mason.nvim",
  "https://github.com/WhoIsSethDaniel/mason-tool-installer.nvim",
  "https://github.com/neovim/nvim-lspconfig",
  "https://github.com/folke/todo-comments.nvim",
  "https://github.com/brenoprata10/nvim-highlight-colors",
  { src = "https://github.com/saghen/blink.cmp", version = vim.version.range("^1") },
}, { confirm = false })

-- --- Mason -------------------------------------------------------------------

-- Installed on start when missing, never auto-updated (:MasonToolsUpdate or
-- :Mason when you want newer versions). clangd, ocamllsp and hls are left to
-- their toolchains (Xcode, opam, ghcup/brew) so they match the compiler.
require("mason").setup()
require("mason-tool-installer").setup({
  ensure_installed = {
    -- language servers
    "lua-language-server",
    "vtsls",
    "eslint-lsp",
    "oxlint",
    "tailwindcss-language-server",
    "css-lsp",
    "rust-analyzer",
    "basedpyright",
    "ruff",
    "gopls",
    -- formatters
    "prettier",
    "prettierd",
    "oxfmt",
    "stylua",
    "shfmt",
    "sql-formatter",
  },
  run_on_start = true,
  auto_update = false,
  integrations = {
    ["mason-lspconfig"] = false,
    ["mason-null-ls"] = false,
    ["mason-nvim-dap"] = false,
  },
})

-- --- Icons ------------------------------------------------------------------

-- Set up before the plugins that draw icons (snacks picker, oil). The mock
-- answers require("nvim-web-devicons") with mini.icons, so plugins that only
-- know devicons get the same icons without installing it. Glyphs come from
-- Symbols Nerd Font Mono, Ghostty's fallback font.
require("mini.icons").setup()
MiniIcons.mock_nvim_web_devicons()

-- --- Pairs ------------------------------------------------------------------

-- Typing an opening bracket or quote inserts its pair; typing the closer
-- steps over it, <BS> deletes both, <CR> between a pair opens a line. On in
-- insert and command-line mode (mini.pairs' default leaves cmdline off).
require("mini.pairs").setup({
  modes = { insert = true, command = true, terminal = false },
})

-- --- Completion (blink.cmp) -------------------------------------------------

-- "enter" preset: <CR> accepts, <C-n>/<C-p> or <Up>/<Down> move, <C-Space>
-- opens the menu or toggles docs, <C-e> closes, <Tab>/<S-Tab> jump snippet
-- fields. Every language server gets blink's capabilities (core/lsp.lua).
-- Menu entries are highlighted by treesitter (LSP items) and carry the
-- mini.icons LSP-kind glyph and color, the same set as the rest of the UI.
local function kind_icon(ctx)
  local icon, hl = MiniIcons.get("lsp", ctx.kind)
  return icon, hl
end

require("blink.cmp").setup({
  keymap = { preset = "enter" },
  appearance = { nerd_font_variant = "mono" },
  completion = {
    -- `(` added after accepting a function or method.
    accept = { auto_brackets = { enabled = true } },
    documentation = { auto_show = true, auto_show_delay_ms = 200 },
    ghost_text = { enabled = false },
    menu = {
      draw = {
        treesitter = { "lsp" },
        components = {
          kind_icon = {
            text = function(ctx)
              return (kind_icon(ctx))
            end,
            highlight = function(ctx)
              return select(2, kind_icon(ctx))
            end,
          },
          kind = {
            highlight = function(ctx)
              return select(2, kind_icon(ctx))
            end,
          },
        },
      },
    },
  },
  -- : and / complete too, the menu opening as you type. Ghost text is off
  -- here as well (blink's cmdline default turns it on).
  cmdline = {
    completion = {
      menu = { auto_show = true },
      ghost_text = { enabled = false },
    },
  },
  fuzzy = { implementation = "prefer_rust_with_warning" },
})

local map = function(mode, lhs, rhs, desc)
  vim.keymap.set(mode, lhs, rhs, { desc = desc })
end

-- --- Treesitter -------------------------------------------------------------

-- The `main` branch: setup() takes only install_dir; parsers are installed
-- explicitly and highlighting starts per buffer via vim.treesitter.start.
local parsers = {
  "bash",
  "c",
  "cmake",
  "cpp",
  "css",
  "diff",
  "fish",
  "gitignore",
  "go",
  "gomod",
  "gosum",
  "gowork",
  "graphql",
  "haskell",
  "html",
  "http",
  "java",
  "javascript",
  "jsdoc",
  "json",
  "lua",
  "luadoc",
  "luap",
  "markdown",
  "markdown_inline",
  "ocaml",
  "ocaml_interface",
  "printf",
  "python",
  "query",
  "racket",
  "regex",
  "rust",
  "scss",
  "sql",
  "toml",
  "tsx",
  "typescript",
  "vim",
  "vimdoc",
  "xml",
  "yaml",
}

require("nvim-treesitter").setup({})
local installed = require("nvim-treesitter.config").get_installed("parsers")
-- A parser without its highlights query counts as missing: the queries are
-- symlinks into the plugin, and a moved plugin (e.g. the old lazy.nvim path)
-- leaves them dangling -- treesitter then attaches with nothing to highlight.
local missing = vim.tbl_filter(function(lang)
  return not vim.list_contains(installed, lang)
    or #vim.treesitter.query.get_files(lang, "highlights") == 0
end, parsers)
if #missing > 0 then
  require("nvim-treesitter").install(missing, { force = true })
end

-- Whenever a parser exists for the buffer's filetype (vim.treesitter.start
-- maps filetype -> language, e.g. sh -> bash): highlighting, folds and
-- treesitter indent. Indent only where the language ships an indents query;
-- without one the indentexpr would leave every new line at column 0.
vim.api.nvim_create_autocmd("FileType", {
  group = vim.api.nvim_create_augroup("treesitter-start", { clear = true }),
  callback = function(ev)
    if not pcall(vim.treesitter.start, ev.buf) then
      return
    end
    local lang = vim.treesitter.language.get_lang(ev.match) or ev.match
    -- Folds from the syntax tree (window-local; foldlevel 99 in options.lua).
    vim.wo[0][0].foldmethod = "expr"
    vim.wo[0][0].foldexpr = "v:lua.vim.treesitter.foldexpr()"
    if #vim.treesitter.query.get_files(lang, "indents") > 0 then
      vim.bo[ev.buf].indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
    end
  end,
})

-- --- tiny-cmdline -----------------------------------------------------------

-- The ui2 cmdline (options.lua: cmdheight=0, ui2) as a floating box,
-- centered left to right and 20% from the top. / and ? stay at the bottom,
-- so the search pattern and count read in the cmdline.
require("tiny-cmdline").setup({
  position = { x = "50%", y = "20%" },
})

-- --- Lualine ----------------------------------------------------------------

-- One global bar (laststatus=3), theme derived from the colorscheme.
--   a mode | b branch | c project root (when not the cwd), icon, path
--   x macro register, diagnostics, git diff | y progress | z line:column

-- Project root: nearest ancestor holding any of these.
local root_markers =
  { ".git", "lua", "package.json", "Makefile", "Cargo.toml", "go.mod", "pyproject.toml" }

local function project_root()
  return vim.fs.root(0, root_markers)
end

-- Root folder name, only when it is not the cwd (cwd is already in the title
-- and the shell prompt).
local function root_name()
  local root = project_root()
  if not root or vim.fs.normalize(root) == vim.fs.normalize(vim.fn.getcwd()) then
    return ""
  end
  return vim.fs.basename(root)
end

-- File path relative to the project root (or cwd), ~ for home, with
-- modified/readonly marks. Special buffers show their name or filetype.
local function pretty_path()
  local buf = vim.api.nvim_get_current_buf()
  local name = vim.api.nvim_buf_get_name(buf)
  if vim.bo[buf].buftype ~= "" or name == "" then
    return name ~= "" and vim.fs.basename((name:gsub("/+$", ""))) or "[No Name]"
  end
  local base = project_root() or vim.fn.getcwd()
  local path = vim.fs.relpath(base, name) or vim.fn.fnamemodify(name, ":~")
  if vim.bo[buf].modified then
    path = path .. " ●"
  end
  if vim.bo[buf].readonly or not vim.bo[buf].modifiable then
    path = path .. " "
  end
  return path
end

local function macro()
  local reg = vim.fn.reg_recording()
  return reg ~= "" and ("recording @" .. reg) or ""
end

-- Diff counts from gitsigns (already computed for the sign column).
local function git_diff()
  local d = vim.b.gitsigns_status_dict
  if d then
    return { added = d.added, modified = d.changed, removed = d.removed }
  end
end

require("lualine").setup({
  options = {
    theme = "auto",
    globalstatus = true,
    component_separators = "",
    section_separators = { left = "\u{e0bc}", right = "\u{e0ba}" }, -- the tabline's slants
    -- Redraw on cursor movement as well as the usual buffer/window events.
    refresh = {
      events = {
        "WinEnter",
        "BufEnter",
        "BufWritePost",
        "SessionLoadPost",
        "FileChangedShellPost",
        "VimResized",
        "Filetype",
        "CursorMoved",
        "CursorMovedI",
        "ModeChanged",
        "RecordingEnter",
      },
    },
  },
  sections = {
    lualine_a = { "mode" },
    lualine_b = { "branch" },
    lualine_c = {
      { root_name, color = { gui = "bold" } },
      { "filetype", icon_only = true, padding = { left = 1, right = 0 } },
      { pretty_path, padding = { left = 0, right = 1 } },
    },
    lualine_x = {
      { macro, color = "WarningMsg" },
      "diagnostics",
      { "diff", source = git_diff },
    },
    lualine_y = { "progress" },
    lualine_z = {
      function()
        local pos = vim.api.nvim_win_get_cursor(0)
        return ("%d:%d"):format(pos[1], pos[2] + 1)
      end,
    },
  },
})

-- RecordingLeave fires while reg_recording() still names the register, so
-- refresh just after it, or "recording @q" would linger until the next move.
vim.api.nvim_create_autocmd("RecordingLeave", {
  group = vim.api.nvim_create_augroup("lualine-macro", { clear = true }),
  callback = function()
    vim.defer_fn(function()
      require("lualine").refresh({ place = { "statusline" } })
    end, 50)
  end,
  desc = "Clear the macro indicator from lualine",
})

-- --- Incline ----------------------------------------------------------------

-- With one global statusline each split still needs to say which file it
-- holds: a floating tag (icon, name, "[+]" when unsaved) in every window's
-- top-right corner, hidden while the cursor is on that line.
require("incline").setup({
  window = {
    placement = { vertical = "top", horizontal = "right" },
    margin = { vertical = 0, horizontal = 1 },
    padding = 1,
  },
  hide = { cursorline = true },
  render = function(props)
    local name = vim.api.nvim_buf_get_name(props.buf)
    name = name ~= "" and vim.fs.basename((name:gsub("/+$", ""))) or "[No Name]"
    local icon, icon_hl = "", nil
    if _G.MiniIcons then
      icon, icon_hl = MiniIcons.get("file", name)
    end
    return {
      { icon, group = props.focused and icon_hl or "InclineNormalNC" },
      " ",
      { name, gui = props.focused and "bold" or nil },
      vim.bo[props.buf].modified and { " [+]", group = "InclineModified" } or "",
    }
  end,
})

-- --- Small highlights ---------------------------------------------------------

-- TODO, FIXME, HACK, WARN, PERF, NOTE, TEST in comments, with the defaults.
require("todo-comments").setup({})

-- Colors in the text painted as their own background: hex, rgb(), hsl(),
-- ANSI escapes and CSS var(--x) usages. Tailwind classes are left alone.
require("nvim-highlight-colors").setup({
  render = "background",
  enable_hex = true,
  enable_short_hex = true,
  enable_rgb = true,
  enable_hsl = true,
  enable_ansi = true,
  enable_var_usage = true,
  enable_named_colors = false,
  enable_tailwind = false,
})

-- --- Oil --------------------------------------------------------------------

-- Replaces netrw for directories (`nvim .`, :e dir, `-`), dotfiles shown.
require("oil").setup({
  default_file_explorer = true,
  columns = { "icon", "permissions", "size", "mtime" },
  view_options = { show_hidden = true },
  -- Renames/moves ask the LSP (workspace/willRenameFiles) to update imports;
  -- the buffers it edits are saved automatically. 3s: gopls can be slow.
  lsp_file_methods = { enabled = true, timeout_ms = 3000, autosave_changes = true },
  -- Always a centered float, with a live preview of the entry under the
  -- cursor on the right (preview_win.update_on_cursor_moved is on by default).
  float = { padding = 2, max_width = 0.85, max_height = 0.8, preview_split = "right" },
  keymaps = { q = { "actions.close", mode = "n" } }, -- also <C-c>
})

local function oil_float(dir)
  require("oil").open_float(dir, { preview = {} })
end
map("n", "-", function()
  oil_float()
end, "File explorer (parent directory)")

-- `nvim .`, `:e dir` and a bare `:Oil` put oil in a normal window. Move it into
-- the float, leaving the previous buffer (or an empty one) behind.
vim.api.nvim_create_autocmd("BufWinEnter", {
  group = vim.api.nvim_create_augroup("oil-always-float", { clear = true }),
  callback = function(ev)
    if vim.bo[ev.buf].filetype ~= "oil" then
      return
    end
    local win = vim.api.nvim_get_current_win()
    if vim.api.nvim_win_get_config(win).relative ~= "" then
      return -- already the float
    end
    vim.schedule(function()
      if not vim.api.nvim_win_is_valid(win) or vim.api.nvim_win_get_buf(win) ~= ev.buf then
        return
      end
      local dir = require("oil").get_current_dir(ev.buf)
      vim.api.nvim_win_call(win, function()
        local alt = vim.fn.bufnr("#")
        if alt > 0 and alt ~= ev.buf and vim.bo[alt].buflisted then
          vim.cmd.buffer(alt)
        else
          vim.cmd.enew()
        end
      end)
      oil_float(dir)
    end)
  end,
  desc = "Open oil as a centered float",
})

-- autosave_changes only covers servers that answer willRenameFiles. Others
-- (vtsls) update imports after the move, from didRenameFiles, by sending a
-- workspace/applyEdit -- which would leave those buffers edited but unsaved.
-- For 5s after an oil move/rename, save the buffers such an edit touches,
-- but only ones that had no unsaved changes of their own.
local rename_until = 0
vim.api.nvim_create_autocmd("User", {
  pattern = "OilActionsPost",
  group = vim.api.nvim_create_augroup("oil-lsp-rename-save", { clear = true }),
  callback = function(ev)
    for _, action in ipairs(ev.data and ev.data.actions or {}) do
      if action.type == "move" then
        rename_until = vim.uv.now() + 5000
        return
      end
    end
  end,
  desc = "Open a window for saving LSP import edits after an oil rename",
})
local apply_edit = vim.lsp.handlers["workspace/applyEdit"]
vim.lsp.handlers["workspace/applyEdit"] = function(err, params, ctx, config)
  if vim.uv.now() > rename_until or not (params and params.edit) then
    return apply_edit(err, params, ctx, config)
  end
  local uris = vim.tbl_keys(params.edit.changes or {})
  for _, change in ipairs(params.edit.documentChanges or {}) do
    if change.textDocument then
      uris[#uris + 1] = change.textDocument.uri
    end
  end
  local clean = {}
  for _, uri in ipairs(uris) do
    local buf = vim.uri_to_bufnr(uri)
    clean[buf] = not (vim.api.nvim_buf_is_loaded(buf) and vim.bo[buf].modified)
  end
  local result = apply_edit(err, params, ctx, config)
  for buf, was_clean in pairs(clean) do
    if was_clean and vim.api.nvim_buf_is_loaded(buf) and vim.bo[buf].modified then
      vim.api.nvim_buf_call(buf, function()
        vim.cmd("silent noautocmd update")
      end)
    end
  end
  return result
end

-- --- Snacks ---------------------------------------

require("snacks").setup({
  picker = {
    enabled = true,
    ui_select = true, -- code actions and other vim.ui.select prompts
    -- Dotfiles are real project files: files and grep include hidden paths.
    -- .git/ itself stays out (snacks excludes it), and so do gitignored files.
    sources = {
      files = { hidden = true },
      grep = { hidden = true },
      grep_word = { hidden = true },
    },
  },
  terminal = { enabled = true },
  notifier = { enabled = true }, -- vim.notify as popups (top right)
  indent = { enabled = true }, -- indent guides + current scope
  statuscolumn = { enabled = true }, -- marks/signs left, folds/git right
  -- Every occurrence of the word under the cursor, from LSP document
  -- highlights (so only in buffers with a language server).
  words = { enabled = true },
  scroll = { enabled = false }, -- no smooth scrolling
})

-- ]] / [[ jump between those occurrences. The go, python, lua, c and markdown
-- ftplugins map ]] / [[ buffer-locally (section motions), which would win over
-- a global map, so they are also set per buffer once an LSP attaches (after
-- the ftplugin ran).
local function words_jump_keys(buf)
  local opts = function(desc)
    return { buffer = buf, desc = desc }
  end
  vim.keymap.set({ "n", "x", "o" }, "]]", function()
    Snacks.words.jump(vim.v.count1)
  end, opts("Next reference"))
  vim.keymap.set({ "n", "x", "o" }, "[[", function()
    Snacks.words.jump(-vim.v.count1)
  end, opts("Previous reference"))
end
words_jump_keys(nil)
vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("snacks-words-keys", { clear = true }),
  callback = function(ev)
    words_jump_keys(ev.buf)
  end,
  desc = "]] / [[ jump between LSP references in this buffer",
})

local P = function(name, opts)
  return function()
    Snacks.picker[name](opts)
  end
end
-- The everyday pickers on short keys:
map("n", "<leader>/", P("grep"), "Live grep (incl. hidden)")
map("n", "<leader>,", P("buffers"), "Open buffers")
map("n", "<leader>'", P("resume"), "Reopen last picker")
map("n", "<leader>xx", P("diagnostics"), "Diagnostics (all buffers)")
map("n", "<leader>xb", P("diagnostics_buffer"), "Diagnostics (this buffer)")
map("n", "<leader>n", P("notifications"), "Notification history")
map("n", "<leader>gh", P("git_diff"), "Git hunks")

map("n", "<leader>ff", P("files"), "Find files")
map("n", "<leader>fg", P("grep"), "Grep (incl. hidden)")
map({ "n", "x" }, "<leader>fc", P("grep_word"), "Grep word/selection")
map("n", "<leader>fb", P("buffers"), "Buffers")
map("n", "<leader>fr", P("recent"), "Recent files")
map("n", "<leader>f/", P("lines"), "Search buffer")
map("n", "<leader>f.", P("resume"), "Resume last picker")
map("n", "<leader>fh", P("help"), "Help tags")
map("n", "<leader>fk", P("keymaps"), "Keymaps")
map("n", "<leader>fd", P("diagnostics"), "Diagnostics")
map("n", "<leader>fs", P("lsp_symbols"), "Document symbols")
map("n", "<leader>fS", P("lsp_workspace_symbols"), "Workspace symbols")
map("n", "<leader>gi", P("gh_issue"), "GitHub issues")
map("n", "<leader>gP", P("gh_pr"), "GitHub pull requests")
map({ "n", "x" }, "<leader>gB", function()
  Snacks.gitbrowse()
end, "Open file/selection on GitHub")
map("n", "<leader>gg", function()
  Snacks.lazygit()
end, "LazyGit")
map({ "n", "t" }, "<C-/>", function()
  Snacks.terminal.toggle(nil, { win = { position = "right", width = 0.4 } })
end, "Terminal (toggle)")

-- --- Gitsigns ---------------------------------------------------------------

-- Blame: <leader>gb a popup for the line (full commit message), <leader>ga
-- the whole file in a side window (<CR> on a line opens its commit), and
-- <leader>ub toggles the end-of-line blame for the cursor line (off at start).
require("gitsigns").setup({
  current_line_blame = false,
  current_line_blame_opts = { delay = 300, virt_text_pos = "eol" },
  current_line_blame_formatter = "<author>, <author_time:%R> • <summary>",
  on_attach = function(bufnr)
    local gs = require("gitsigns")
    local bmap = function(mode, lhs, rhs, desc)
      vim.keymap.set(mode, lhs, rhs, { buffer = bufnr, desc = desc })
    end
    bmap("n", "]h", function()
      gs.nav_hunk("next")
    end, "Next hunk")
    bmap("n", "[h", function()
      gs.nav_hunk("prev")
    end, "Previous hunk")
    bmap("n", "<leader>gs", gs.stage_hunk, "Stage/unstage hunk")
    bmap("n", "<leader>gr", gs.reset_hunk, "Reset hunk")
    bmap("v", "<leader>gs", function()
      gs.stage_hunk({ vim.fn.line("."), vim.fn.line("v") })
    end, "Stage hunk")
    bmap("v", "<leader>gr", function()
      gs.reset_hunk({ vim.fn.line("."), vim.fn.line("v") })
    end, "Reset hunk")
    bmap("n", "<leader>gS", gs.stage_buffer, "Stage buffer")
    bmap("n", "<leader>gR", gs.reset_buffer, "Reset buffer")
    bmap("n", "<leader>gp", gs.preview_hunk, "Preview hunk")
    bmap("n", "<leader>gb", function()
      gs.blame_line({ full = true })
    end, "Blame line")
    bmap("n", "<leader>ga", gs.blame, "Blame file")
    bmap("n", "<leader>ub", gs.toggle_current_line_blame, "Toggle line blame")
    bmap("n", "<leader>gd", gs.diffthis, "Diff against index")
  end,
})

-- --- Built-in optional plugins ----------------------------------------------

-- :Undotree ships with Neovim 0.12 as an opt-in package.
vim.cmd.packadd("nvim.undotree")
map("n", "<leader>uu", "<cmd>Undotree<CR>", "Undo tree")
