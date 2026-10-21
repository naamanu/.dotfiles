-- Native LSP via vim.lsp.config/enable (no nvim-lspconfig). Server tables
-- live in lsp/<name>.lua at the config root (a runtimepath `lsp/` directory,
-- :h lsp-config); this file keeps what is shared: capabilities, diagnostics,
-- LspAttach keymaps, the Python interpreter helpers, and the gate that enables
-- a server only when its binary is present.
--
-- NOTE: this is the native API — nvim-lspconfig concepts like
-- `on_new_config` and `single_file_support` do not exist and are silently
-- ignored; the native equivalent of single-file support is the default
-- `workspace_required = false`.

local lsp = vim.lsp
local M = {}

local function apply_code_action(kind)
  vim.lsp.buf.code_action({
    apply = true,
    context = {
      only = { kind },
      diagnostics = {},
    },
  })
end

-- Diagnostics: short text at the end of every line but the current one,
-- which gets the full message beneath it instead. Borders come from
-- 'winborder' (options.lua).
local severity = vim.diagnostic.severity
vim.diagnostic.config({
  virtual_text = { spacing = 4, prefix = "●", current_line = false },
  virtual_lines = { current_line = true },
  signs = {
    text = {
      [severity.ERROR] = "",
      [severity.WARN] = "",
      [severity.INFO] = "",
      [severity.HINT] = "",
    },
  },
  underline = true,
  update_in_insert = false,
  severity_sort = true,
  -- Show the diagnostic after ]d / [d (jump.float was deprecated in 0.12).
  jump = {
    on_jump = function(_, bufnr)
      vim.diagnostic.open_float({ bufnr = bufnr, scope = "cursor", focus = false })
    end,
  },
})

-- Python interpreter resolution.
--
-- basedpyright only auto-detects a `.venv` relative to its own process cwd,
-- and Neovim launches servers with `cmd_cwd` defaulting to Neovim's cwd --
-- "not related to root_dir" (:h vim.lsp.ClientConfig).  Start Neovim anywhere
-- but the project root and every third-party import (fastapi, django, ...)
-- goes unresolved.  Resolve the interpreter here and hand it to the server as
-- `python.pythonPath` so the result no longer depends on where Neovim started.
local VENV_DIRS = { ".venv", "venv", ".env" }

local function venv_python(dir)
  for _, name in ipairs(VENV_DIRS) do
    local candidate = dir .. "/" .. name .. "/bin/python"
    if vim.fn.executable(candidate) == 1 then
      return candidate
    end
  end
  return nil
end

-- Search the workspace root and its ancestors, stopping at $HOME: uv
-- workspaces and monorepos keep a single venv above the directory that owns
-- pyproject.toml, so the nearest one upward is the right one.
local function python_path(root)
  local home = vim.env.HOME
  local dir = root
  while dir and dir ~= "" and dir ~= home and dir ~= "/" do
    local found = venv_python(dir)
    if found then
      return found
    end
    local parent = vim.fs.dirname(dir)
    if parent == dir then
      break
    end
    dir = parent
  end

  -- A venv activated in the shell before Neovim started.
  local active = vim.env.VIRTUAL_ENV
  if active and vim.fn.executable(active .. "/bin/python") == 1 then
    return active .. "/bin/python"
  end

  local fallback = vim.fn.exepath("python3")
  return fallback ~= "" and fallback or nil
end

-- lsp/basedpyright.lua calls this from on_init (lazily, so the require does
-- not loop while this module is still loading).
M.python_path = python_path

-- `:PythonEnv` answers "why is this import unresolved?" in one place: an
-- interpreter outside the project (or a `.venv` that was never populated) is
-- the usual cause.
vim.api.nvim_create_user_command("PythonEnv", function()
  local clients = vim.lsp.get_clients({ bufnr = 0, name = "basedpyright" })
  local client = clients[1]
  local root = client and client.root_dir or vim.fn.getcwd()
  local interpreter = client and vim.tbl_get(client.settings or {}, "python", "pythonPath")
    or python_path(root)

  local lines = {
    "root_dir    : " .. tostring(root),
    "interpreter : " .. tostring(interpreter),
    "attached    : " .. (client and "basedpyright" or "no basedpyright client"),
  }

  if interpreter and vim.fn.executable(interpreter) == 1 then
    local version = vim.system({ interpreter, "--version" }, { text = true }):wait()
    lines[#lines + 1] = "version     : " .. vim.trim(version.stdout .. version.stderr)
    local inside = vim.startswith(interpreter, root .. "/")
    lines[#lines + 1] = "in project  : "
      .. (inside and "yes" or "NO -- project deps will not resolve")
  else
    lines[#lines + 1] = "version     : interpreter not executable"
  end

  vim.notify(table.concat(lines, "\n"), vim.log.levels.INFO, { title = "Python environment" })
end, { desc = "Report the Python interpreter the LSP is using" })

-- Enable servers whose binary is present on the shared PATH, so a missing
-- tool degrades silently instead of erroring. { config name, binary, extra check }.
-- Installing a server: lsp/<name>.lua + a row here + the binary in setup.sh.
local servers = {
  { "basedpyright", "basedpyright-langserver" },
  { "ruff", "ruff" },
  { "vtsls", "vtsls" },
  { "eslint", "vscode-eslint-language-server" },
  { "clangd", "clangd" },
  { "ocamllsp", "ocamllsp" },
  { "hls", "haskell-language-server-wrapper" },
}

local enabled = {}
for _, server in ipairs(servers) do
  local name, binary = server[1], server[2]
  if vim.fn.executable(binary) == 1 then
    enabled[#enabled + 1] = name
  end
end
if #enabled > 0 then
  lsp.enable(enabled)
end

-- LspAttach keybindings. grr/gri/grt/gO/K/<C-s> are Neovim defaults (0.11+);
-- only gd/gD and the leader maps are added here.
vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("lsp-attach", { clear = true }),
  callback = function(event)
    local map = function(keys, func, desc)
      vim.keymap.set("n", keys, func, { buffer = event.buf, desc = "LSP: " .. desc })
    end

    map("gd", vim.lsp.buf.definition, "Go to definition")
    map("gD", vim.lsp.buf.declaration, "Go to declaration")
    map("<leader>la", vim.lsp.buf.code_action, "Code action")
    map("<leader>lr", vim.lsp.buf.rename, "Rename")
    map("<leader>ld", vim.diagnostic.open_float, "Line diagnostics")
    map("<leader>ls", vim.lsp.buf.signature_help, "Signature help")
    map("<leader>ci", function()
      apply_code_action("source.organizeImports")
    end, "Organize imports")
    map("<leader>cI", function()
      apply_code_action("source.addMissingImports")
    end, "Add missing imports")
    map("<leader>cu", function()
      apply_code_action("source.removeUnused")
    end, "Remove unused imports")
    map("<leader>cF", function()
      apply_code_action("source.fixAll")
    end, "Fix all auto-fixable issues")
    map("<leader>cE", function()
      apply_code_action("source.fixAll.eslint")
    end, "Fix ESLint issues")
    map("<leader>lR", function()
      local clients = vim.lsp.get_clients({ bufnr = event.buf })
      for _, attached_client in ipairs(clients) do
        attached_client:stop(true)
      end
      vim.defer_fn(function()
        vim.cmd("edit")
      end, 100)
    end, "Restart LSP clients")

    local client = vim.lsp.get_client_by_id(event.data.client_id)

    -- Built-in completion: the popup opens as you type (fuzzy matched,
    -- docs beside it); <C-y> accepts, <C-n>/<C-p> move, <C-Space> forces it.
    -- Snippets expand through vim.snippet; <Tab>/<S-Tab> jump fields.
    if client and client:supports_method("textDocument/completion", event.buf) then
      vim.lsp.completion.enable(true, client.id, event.buf, { autotrigger = true })
      vim.keymap.set(
        "i",
        "<C-Space>",
        vim.lsp.completion.get,
        { buffer = event.buf, desc = "Complete" }
      )
    end

    -- Inlay hints on by default.
    if client and client:supports_method("textDocument/inlayHint", event.buf) then
      vim.lsp.inlay_hint.enable(true, { bufnr = event.buf })
      map("<leader>lh", function()
        vim.lsp.inlay_hint.enable(
          not vim.lsp.inlay_hint.is_enabled({ bufnr = event.buf }),
          { bufnr = event.buf }
        )
      end, "Toggle inlay hints (buffer)")
    end

    -- Code lens
    if client and client:supports_method("textDocument/codeLens", event.buf) then
      map("<leader>lc", vim.lsp.codelens.run, "Run code lens")
      vim.lsp.codelens.enable(true, { bufnr = event.buf })
    end

    if client and client.name == "ruff" then
      client.server_capabilities.hoverProvider = false
    end
  end,
})

return M
