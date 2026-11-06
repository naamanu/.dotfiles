-- Plugins: the few things stock Neovim cannot do, managed by the built-in
-- vim.pack (revisions pinned in nvim-pack-lock.json at the config root).
-- Update with :lua vim.pack.update(), review, then :write to confirm.
--
-- This file holds the one plugin list plus mason, editing (pairs, text
-- objects, surround), completion and treesitter. The rest is set up by
-- topic: core/ui.lua (icons, cmdline, statusline, oil, snacks), core/git.lua
-- (gitsigns, codediff), core/format.lua (conform), core/dap.lua (nvim-dap).
--
--   nvim-treesitter   parsers for python/ts/js/sql/ocaml/haskell/go (Neovim
--                     ships only c, lua, markdown, vim, vimdoc, query)
--   nvim-treesitter-textobjects  function/class/... queries; ]m [m ]k [k moves
--   mini.ai           a/i text objects: functions, classes, blocks, calls,
--                     arguments, brackets, quotes (af, ic, aa, i( ...)
--   mini.surround     add/change/delete surrounding pairs (gsa, gsd, gsr)
--   mini.pairs        auto-closing brackets and quotes (insert + cmdline)
--   blink.cmp         completion menu (LSP, path, snippets, buffer, cmdline);
--                     pinned to 1.x, its prebuilt fuzzy matcher is fetched
--                     for the release tag
--   mason.nvim        installs language servers, formatters and debuggers
--                     (its bin dir goes first on PATH); mason-tool-installer
--                     keeps the list
--   nvim-lspconfig    default server configs (lsp/*.lua); ours override them
--                     from after/lsp/
--   SchemaStore.nvim  JSON/YAML schema catalog for jsonls and yamlls
--   conform.nvim      format on save (formatters, LSP fallback)
--   nvim-dap (+ nvim-dap-python)  debugging: Go through Delve, Python
--                     through debugpy
--   oil.nvim          directories as editable buffers
--   snacks.nvim       picker, terminals, notifier, input, indent/scope,
--                     big-file guard
--   mini.icons        file/filetype/LSP-kind icons; also stands in for
--                     nvim-web-devicons, so plugins asking for it get these
--   tiny-cmdline.nvim the ui2 cmdline as a centered floating box
--   lualine.nvim      the global statusline
--   incline.nvim      a file-name tag in each window's top-right corner
--   todo-comments.nvim  TODO/FIXME/HACK/... highlighted (needs plenary.nvim)
--   nvim-highlight-colors  color literals painted as their own background
--   gitsigns.nvim     hunks in the sign column, stage/reset/blame
--   codediff.nvim     VS Code-style side-by-side diffs, changed-file explorer,
--                     commit history (downloads its diff library on first use)
--   vim-tmux-navigator  C-h/j/k/l across Neovim windows and tmux panes

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

vim.pack.add({
  { src = "https://github.com/nvim-treesitter/nvim-treesitter", version = "main" },
  { src = "https://github.com/nvim-treesitter/nvim-treesitter-textobjects", version = "main" },
  "https://github.com/stevearc/oil.nvim",
  "https://github.com/folke/snacks.nvim",
  "https://github.com/lewis6991/gitsigns.nvim",
  "https://github.com/christoomey/vim-tmux-navigator",
  "https://github.com/mfussenegger/nvim-dap",
  "https://github.com/mfussenegger/nvim-dap-python",
  "https://github.com/rachartier/tiny-cmdline.nvim",
  "https://github.com/nvim-mini/mini.icons",
  "https://github.com/nvim-mini/mini.pairs",
  "https://github.com/nvim-mini/mini.ai",
  "https://github.com/nvim-mini/mini.surround",
  "https://github.com/stevearc/conform.nvim",
  "https://github.com/esmuellert/codediff.nvim",
  "https://github.com/nvim-lualine/lualine.nvim",
  "https://github.com/b0o/incline.nvim",
  "https://github.com/nvim-lua/plenary.nvim",
  "https://github.com/mason-org/mason.nvim",
  "https://github.com/WhoIsSethDaniel/mason-tool-installer.nvim",
  "https://github.com/neovim/nvim-lspconfig",
  "https://github.com/b0o/SchemaStore.nvim",
  "https://github.com/folke/todo-comments.nvim",
  "https://github.com/brenoprata10/nvim-highlight-colors",
  { src = "https://github.com/saghen/blink.cmp", version = vim.version.range("^1") },
}, { confirm = false })

-- --- Mason -------------------------------------------------------------------

-- Installed on start when missing, never auto-updated (:MasonToolsUpdate or
-- :Mason when you want newer versions). clangd, ocamllsp, hls and jdtls are
-- left to their toolchains (Xcode, opam, ghcup/brew, brew) so they match the
-- compiler.
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
    "bash-language-server",
    "shellcheck", -- bashls runs it for diagnostics
    "json-lsp",
    "yaml-language-server",
    "taplo",
    "marksman",
    "copilot-language-server",
    -- debuggers
    "debugpy",
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

-- --- Pairs ------------------------------------------------------------------

-- Typing an opening bracket or quote inserts its pair; typing the closer
-- steps over it, <BS> deletes both, <CR> between a pair opens a line. On in
-- insert and command-line mode (mini.pairs' default leaves cmdline off).
require("mini.pairs").setup({
  modes = { insert = true, command = true, terminal = false },
})

-- --- Text objects ------------------------------------------------------------

-- mini.ai: a/i text objects in Visual and Operator-pending mode, searching
-- forward when the cursor is not inside one (`ci(` from before the parens).
-- Treesitter ones come from nvim-treesitter-textobjects' queries:
--   f function   c class   o block/conditional/loop   u call   a argument
-- plus mini.ai's brackets, quotes, `t` tag and `?` (asks for the edges).
-- `ii`/`ai` (indent scope) are snacks.scope (core/ui.lua). `an`/`in` stay
-- the built-in syntax-node selection.
local ai = require("mini.ai")
local ts = ai.gen_spec.treesitter
ai.setup({
  n_lines = 500,
  custom_textobjects = {
    f = ts({ a = "@function.outer", i = "@function.inner" }),
    c = ts({ a = "@class.outer", i = "@class.inner" }),
    o = ts({
      a = { "@block.outer", "@conditional.outer", "@loop.outer" },
      i = { "@block.inner", "@conditional.inner", "@loop.inner" },
    }),
    u = ai.gen_spec.function_call(),
  },
})

-- --- Surround --------------------------------------------------------------

-- On gs (stock `gs` only sleeps), so `s` keeps its stock meaning:
--   gsa{motion}{char} add   gsd{char} delete   gsr{old}{new} replace
--   gsf / gsF find right/left   gsh highlight
-- e.g. gsaiw" quotes a word, gsd( strips parens, gsr'" swaps quotes.
require("mini.surround").setup({
  mappings = {
    add = "gsa",
    delete = "gsd",
    find = "gsf",
    find_left = "gsF",
    highlight = "gsh",
    replace = "gsr",
    update_n_lines = "",
  },
})

-- --- Completion (blink.cmp) -------------------------------------------------

-- "enter" preset: <CR> accepts, <C-n>/<C-p> or <Up>/<Down> move, <C-Space>
-- opens the menu or toggles docs, <C-e> closes, <Tab>/<S-Tab> jump snippet
-- fields. <Tab> first accepts an inline (Copilot) suggestion when one is
-- showing (core/lsp.lua). Every language server gets blink's capabilities (core/lsp.lua).
-- Menu entries are highlighted by treesitter (LSP items) and carry the
-- mini.icons LSP-kind glyph and color, the same set as the rest of the UI.
local function kind_icon(ctx)
  local icon, hl = MiniIcons.get("lsp", ctx.kind)
  return icon, hl
end

require("blink.cmp").setup({
  keymap = {
    preset = "enter",
    ["<Tab>"] = {
      function()
        return vim.lsp.inline_completion.get()
      end,
      "snippet_forward",
      "fallback",
    },
  },
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
-- A parser without its highlights query counts as missing: the queries are
-- symlinks into the plugin, and a moved plugin (e.g. the old lazy.nvim path)
-- leaves them dangling -- treesitter then attaches with nothing to highlight.
-- Checked just after startup: scanning every parser's queries costs ~8ms,
-- and the install it may start is asynchronous anyway.
vim.defer_fn(function()
  local installed = require("nvim-treesitter.config").get_installed("parsers")
  local missing = vim.tbl_filter(function(lang)
    return not vim.list_contains(installed, lang)
      or #vim.treesitter.query.get_files(lang, "highlights") == 0
  end, parsers)
  if #missing > 0 then
    require("nvim-treesitter").install(missing, { force = true })
  end
end, 100)

-- ]m / [m next/previous function start, ]M / [M function end, ]k / [k
-- class start, with nvim-treesitter-textobjects (jumps land in the jumplist).
require("nvim-treesitter-textobjects").setup({ move = { set_jumps = true } })

local function textobject_moves(buf)
  local move = require("nvim-treesitter-textobjects.move")
  local function key(lhs, fn, query, desc)
    vim.keymap.set({ "n", "x", "o" }, lhs, function()
      move[fn](query, "textobjects")
    end, { buffer = buf, desc = desc })
  end
  key("]m", "goto_next_start", "@function.outer", "Next function start")
  key("[m", "goto_previous_start", "@function.outer", "Previous function start")
  key("]M", "goto_next_end", "@function.outer", "Next function end")
  key("[M", "goto_previous_end", "@function.outer", "Previous function end")
  key("]k", "goto_next_start", "@class.outer", "Next class")
  key("[k", "goto_previous_start", "@class.outer", "Previous class")
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
    if #vim.treesitter.query.get_files(lang, "textobjects") > 0 then
      -- Scheduled: the python/go/... ftplugins map ]m [m buffer-locally after
      -- this autocmd, and those would win over ours.
      vim.schedule(function()
        if vim.api.nvim_buf_is_valid(ev.buf) then
          textobject_moves(ev.buf)
        end
      end)
    end
  end,
})

local map = function(mode, lhs, rhs, desc)
  vim.keymap.set(mode, lhs, rhs, { desc = desc })
end

-- --- Built-in optional plugins ----------------------------------------------

-- :Undotree ships with Neovim 0.12 as an opt-in package.
vim.cmd.packadd("nvim.undotree")
map("n", "<leader>uu", "<cmd>Undotree<CR>", "Undo tree")
