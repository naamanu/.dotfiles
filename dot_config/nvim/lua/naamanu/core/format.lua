-- Format on save without a plugin. Formatters come from mason (plugins.lua).
--
--   JS/TS/JSON/CSS/HTML/YAML/Markdown/GraphQL/Vue
--                oxfmt when the project has an .oxfmtrc; else prettierd (or
--                prettier) when the project uses prettier -- a local install or
--                a prettier config; else the language server (vtsls, cssls)
--   Lua          stylua (nearest stylua.toml)
--   sh/bash      shfmt (respects .editorconfig)
--   SQL          sql-formatter
--   Python       ruff (LSP); C/C++ clangd; OCaml ocamllsp (when the project
--                has an .ocamlformat); Haskell hls (ormolu);
--                Go gopls (organize imports, then gofumpt)
--
-- <leader>cf formats on demand; :FormatToggle turns saving-formats off for
-- the session.

local M = { enabled = true }

local web_fts = {
  javascript = true,
  javascriptreact = true,
  typescript = true,
  typescriptreact = true,
  json = true,
  jsonc = true,
  css = true,
  scss = true,
  less = true,
  html = true,
  yaml = true,
  markdown = true,
  graphql = true,
  vue = true,
}

local oxfmt_configs = { ".oxfmtrc.json", ".oxfmtrc.jsonc" }
local prettier_configs = {
  ".prettierrc",
  ".prettierrc.json",
  ".prettierrc.json5",
  ".prettierrc.yaml",
  ".prettierrc.yml",
  ".prettierrc.toml",
  ".prettierrc.js",
  ".prettierrc.cjs",
  ".prettierrc.mjs",
  "prettier.config.js",
  "prettier.config.cjs",
  "prettier.config.mjs",
  "prettier.config.ts",
}

local function executable(bin)
  return vim.fn.executable(bin) == 1
end

-- The web formatter for FILE: oxfmt if the project configures it, prettier
-- (through prettierd, which uses the project's own copy) if the project uses
-- prettier, nil otherwise.
local function web_formatter(buf, file)
  if vim.fs.root(buf, oxfmt_configs) and executable("oxfmt") then
    return { "oxfmt", "--stdin-filepath=" .. file }
  end
  local local_prettier = require("naamanu.core.tasks").local_package_binary("prettier", file)
  if local_prettier or vim.fs.root(buf, prettier_configs) then
    if executable("prettierd") then
      return { "prettierd", file }
    end
    return { local_prettier or "prettier", "--stdin-filepath", file }
  end
end

-- Servers that should never format: basedpyright (ruff does), eslint (fixes
-- go through <leader>cE).
local no_format = { basedpyright = true, eslint = true }

-- Pipe the buffer through CMD; replace it only when the command succeeds and
-- the text changed, so a syntax error never wipes the buffer.
local function pipe(buf, cmd)
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  local result =
    vim.system(cmd, { stdin = table.concat(lines, "\n") .. "\n", text = true }):wait(5000)
  if result.code ~= 0 then
    vim.notify(vim.trim(result.stderr or "format failed"), vim.log.levels.WARN, { title = cmd[1] })
    return
  end
  local formatted = vim.split(result.stdout:gsub("\n$", ""), "\n", { plain = true })
  if not vim.deep_equal(lines, formatted) then
    local view = vim.fn.winsaveview()
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, formatted)
    vim.fn.winrestview(view)
  end
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
  local result = client:request_sync("textDocument/codeAction", params, 3000, buf)
  for _, action in pairs(result and result.result or {}) do
    if action.edit then
      vim.lsp.util.apply_workspace_edit(action.edit, client.offset_encoding)
    end
  end
end

local function lsp_format(buf)
  vim.lsp.buf.format({
    bufnr = buf,
    timeout_ms = 3000,
    filter = function(client)
      return not no_format[client.name]
    end,
  })
end

function M.format(buf)
  buf = buf or vim.api.nvim_get_current_buf()
  local ft = vim.bo[buf].filetype
  local file = vim.api.nvim_buf_get_name(buf)

  if web_fts[ft] then
    local cmd = web_formatter(buf, file)
    if cmd then
      return pipe(buf, cmd)
    end
  elseif ft == "lua" and executable("stylua") then
    return pipe(buf, { "stylua", "--stdin-filepath", file, "-" })
  elseif (ft == "sh" or ft == "bash") and executable("shfmt") then
    return pipe(buf, { "shfmt", "--filename", file })
  elseif ft == "sql" then
    if executable("sql-formatter") then
      return pipe(buf, { "sql-formatter" })
    end
    return
  elseif ft == "go" then
    organize_imports(buf)
  end

  lsp_format(buf)
end

vim.api.nvim_create_autocmd("BufWritePre", {
  group = vim.api.nvim_create_augroup("format-on-save", { clear = true }),
  callback = function(ev)
    if M.enabled and vim.bo[ev.buf].buftype == "" then
      M.format(ev.buf)
    end
  end,
})

vim.keymap.set({ "n", "x" }, "<leader>cf", function()
  M.format()
end, { desc = "Format buffer" })

vim.api.nvim_create_user_command("FormatToggle", function()
  M.enabled = not M.enabled
  vim.notify("Format on save " .. (M.enabled and "on" or "off"))
end, { desc = "Toggle format on save" })

return M
