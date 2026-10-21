-- Plugins: the few things stock Neovim cannot do, managed by the built-in
-- vim.pack (revisions pinned in nvim-pack-lock.json at the config root).
-- Update with :lua vim.pack.update(), review, then :write to confirm.
--
--   nvim-treesitter   parsers for python/ts/js/sql/ocaml/haskell (Neovim
--                     ships only c, lua, markdown, vim, vimdoc, query)
--   oil.nvim          directories as editable buffers
--   snacks.nvim       picker (files, grep, LSP, GitHub) and terminals
--   gitsigns.nvim     hunks in the sign column, stage/reset/blame
--   vim-tmux-navigator  C-h/j/k/l across Neovim windows and tmux panes

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
}, { confirm = false })

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
}

-- Treesitter indentexpr only where it beats the runtime indent scripts.
local no_ts_indent = { python = true, markdown = true }

require("nvim-treesitter").setup({})
local installed = require("nvim-treesitter.config").get_installed("parsers")
local missing = vim.tbl_filter(function(lang)
  return not vim.list_contains(installed, lang)
end, parsers)
if #missing > 0 then
  require("nvim-treesitter").install(missing)
end

vim.api.nvim_create_autocmd("FileType", {
  group = vim.api.nvim_create_augroup("treesitter-start", { clear = true }),
  callback = function(ev)
    if pcall(vim.treesitter.start, ev.buf) and not no_ts_indent[ev.match] then
      vim.bo[ev.buf].indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
    end
  end,
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
