-- Format on save through conform.nvim (core/plugins.lua). Formatters come
-- from mason or the system; a filetype without one, or whose formatters are
-- all missing, is formatted by its language server instead.
--
--   JS/TS/JSX/TSX/JSON/CSS/SCSS/Less/HTML/Markdown/MDX/YAML
--                the first installed of oxfmt, prettierd, prettier
--                (conform's prettier prefers the project's node_modules copy)
--   Lua          stylua (nearest stylua.toml)
--   sh/bash      shfmt (respects .editorconfig)
--   fish         fish_indent
--   SQL          sql-formatter
--   everything else: the language server -- ruff (Python), clangd, ocamllsp,
--                hls (ormolu), gopls (organize imports first, then gofumpt)
--
-- <leader>cf formats on demand; :ToggleFormatter turns format on save off
-- (and back on) for the current buffer.

local M = {}

-- .mdx is not a filetype Neovim knows; give it one, highlighted by the
-- markdown parser.
vim.filetype.add({ extension = { mdx = "mdx" } })
vim.treesitter.language.register("markdown", "mdx")

local web = { "oxfmt", "prettierd", "prettier", stop_after_first = true }

local formatters_by_ft = {
  lua = { "stylua" },
  sh = { "shfmt" },
  bash = { "shfmt" },
  fish = { "fish_indent" },
  sql = { "sql_formatter" },
}
for _, ft in ipairs({
  "javascript",
  "javascriptreact",
  "typescript",
  "typescriptreact",
  "json",
  "css",
  "scss",
  "less",
  "html",
  "markdown",
  "mdx",
  "yaml",
}) do
  formatters_by_ft[ft] = web
end

-- Servers that should never format: basedpyright (ruff does), eslint (fixes
-- go through <leader>cE).
local no_format = { basedpyright = true, eslint = true }

local function lsp_filter(client)
  return not no_format[client.name]
end

-- Apply gopls's organize-imports edit synchronously, so it lands before the
-- write (an async code action would modify the buffer after saving).
local function organize_imports(buf)
  local client = vim.lsp.get_clients({ bufnr = buf, name = "gopls" })[1]
  if not client then
    return
  end
  local params = vim.lsp.util.make_range_params(0, client.offset_encoding)
  params.context = { only = { "source.organizeImports" }, diagnostics = {} }
  local result = client:request_sync("textDocument/codeAction", params, 1000, buf)
  for _, action in pairs(result and result.result or {}) do
    if action.edit then
      vim.lsp.util.apply_workspace_edit(action.edit, client.offset_encoding)
    end
  end
end

local function prepare(buf)
  if vim.bo[buf].filetype == "go" then
    organize_imports(buf)
  end
end

local conform = require("conform")

conform.setup({
  formatters_by_ft = formatters_by_ft,
  format_on_save = function(buf)
    if vim.b[buf].disable_autoformat or vim.bo[buf].buftype ~= "" then
      return
    end
    prepare(buf)
    return { timeout_ms = 1000, lsp_format = "fallback", filter = lsp_filter }
  end,
})

function M.format(buf)
  buf = buf or vim.api.nvim_get_current_buf()
  prepare(buf)
  conform.format({ bufnr = buf, timeout_ms = 1000, lsp_format = "fallback", filter = lsp_filter })
end

vim.keymap.set({ "n", "x" }, "<leader>cf", function()
  M.format()
end, { desc = "Format buffer" })

vim.api.nvim_create_user_command("ToggleFormatter", function()
  local buf = vim.api.nvim_get_current_buf()
  vim.b[buf].disable_autoformat = not vim.b[buf].disable_autoformat
  vim.notify(
    "Format on save " .. (vim.b[buf].disable_autoformat and "off" or "on") .. " for this buffer"
  )
end, { desc = "Toggle format on save for the current buffer" })

return M
