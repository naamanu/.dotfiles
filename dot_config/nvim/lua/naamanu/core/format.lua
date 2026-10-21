-- Format on save without a plugin.
--
--   TS/JS/JSON   the project's own prettier (node_modules/.bin), else the
--                language server (vtsls)
--   SQL          sql-formatter on PATH
--   Python       ruff (LSP); C/C++ clangd; OCaml ocamllsp (when the project
--                has an .ocamlformat); Haskell hls (ormolu)
--
-- <leader>cf formats on demand; :FormatToggle turns saving-formats off for
-- the session.

local M = { enabled = true }

local prettier_fts = {
  javascript = true,
  javascriptreact = true,
  typescript = true,
  typescriptreact = true,
  json = true,
  jsonc = true,
}

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

  if prettier_fts[ft] then
    local prettier = require("naamanu.core.tasks").local_package_binary("prettier", file)
    if prettier then
      return pipe(buf, { prettier, "--stdin-filepath", file })
    end
  elseif ft == "sql" then
    if vim.fn.executable("sql-formatter") == 1 then
      return pipe(buf, { "sql-formatter" })
    end
    return
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
