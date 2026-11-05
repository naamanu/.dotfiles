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

-- nvim-treesitter's parsers must be rebuilt whenever the plugin changes.
-- Registered before vim.pack.add so it also fires on the first install.
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
  "https://github.com/nvim-lualine/lualine.nvim",
}, { confirm = false })

-- --- Icons ------------------------------------------------------------------

-- Set up before the plugins that draw icons (snacks picker, oil). The mock
-- answers require("nvim-web-devicons") with mini.icons, so plugins that only
-- know devicons get the same icons without installing it. Glyphs come from
-- Symbols Nerd Font Mono, Ghostty's fallback font.
require("mini.icons").setup()
MiniIcons.mock_nvim_web_devicons()

local map = function(mode, lhs, rhs, desc)
  vim.keymap.set(mode, lhs, rhs, { desc = desc })
end

-- --- Treesitter -------------------------------------------------------------

-- The `main` branch: setup() takes only install_dir; parsers are installed
-- explicitly and highlighting starts per buffer via vim.treesitter.start.
local parsers = {
  "c",
  "cpp",
  "python",
  "javascript",
  "typescript",
  "tsx",
  "json",
  "sql",
  "ocaml",
  "ocaml_interface",
  "haskell",
  "go",
  "gomod",
  "gosum",
  "gowork",
}

-- Treesitter indentexpr only where it beats the runtime indent scripts.
local no_ts_indent = { python = true, markdown = true }

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

vim.api.nvim_create_autocmd("FileType", {
  group = vim.api.nvim_create_augroup("treesitter-start", { clear = true }),
  callback = function(ev)
    if not pcall(vim.treesitter.start, ev.buf) then
      return
    end
    -- Folds from the syntax tree (window-local; foldlevel 99 in options.lua).
    vim.wo[0][0].foldmethod = "expr"
    vim.wo[0][0].foldexpr = "v:lua.vim.treesitter.foldexpr()"
    if not no_ts_indent[ev.match] then
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

-- --- Oil --------------------------------------------------------------------

require("oil").setup({
  default_file_explorer = true,
  columns = {},
  view_options = { show_hidden = true },
})
map("n", "-", "<cmd>Oil<CR>", "Open parent directory")

-- --- Snacks: picker and terminal only ---------------------------------------

require("snacks").setup({
  picker = {
    enabled = true,
    ui_select = true, -- code actions and other vim.ui.select prompts
    sources = { files = { hidden = true } },
  },
  terminal = { enabled = true },
})

local P = function(name, opts)
  return function()
    Snacks.picker[name](opts)
  end
end
map("n", "<leader>ff", P("files"), "Find files")
map("n", "<leader>fg", P("grep"), "Grep")
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

require("gitsigns").setup({
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
    bmap("n", "<leader>gd", gs.diffthis, "Diff against index")
  end,
})

-- --- Built-in optional plugins ----------------------------------------------

-- :Undotree ships with Neovim 0.12 as an opt-in package.
vim.cmd.packadd("nvim.undotree")
map("n", "<leader>uu", "<cmd>Undotree<CR>", "Undo tree")
