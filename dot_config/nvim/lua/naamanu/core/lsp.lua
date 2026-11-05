-- Language servers through the native API (vim.lsp.config / vim.lsp.enable).
-- nvim-lspconfig supplies each server's defaults (its lsp/<name>.lua: cmd,
-- filetypes, root detection); our overrides live in after/lsp/<name>.lua,
-- which wins (:h lsp-config-merge). mason installs the binaries
-- (core/plugins.lua). This file keeps what is shared: capabilities,
-- diagnostics, LspAttach keymaps, the Python interpreter helpers, and the
-- list of servers.
--
-- NOTE: lspconfig's legacy `require("lspconfig").x.setup()` API is not used;
-- `on_new_config` and `single_file_support` do not exist here.

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

-- Diagnostics: underlined, never refreshed while typing, worst first. Short
-- text at the end of every line but the current one, which gets the full
-- message beneath it; the source ("ruff", "basedpyright") is named only when
-- several sources report on the buffer. Gutter icons are the snacks icon set
-- (the same ones the <leader>xx picker shows). Borders come from 'winborder'.
local severity = vim.diagnostic.severity

local function diagnostic_icons()
  local ok, icons = pcall(function()
    return Snacks.picker.config.get().icons.diagnostics
  end)
  icons = ok and icons or { Error = "", Warn = "", Info = "", Hint = "" }
  return {
    [severity.ERROR] = vim.trim(icons.Error),
    [severity.WARN] = vim.trim(icons.Warn),
    [severity.INFO] = vim.trim(icons.Info),
    [severity.HINT] = vim.trim(icons.Hint),
  }
end

-- "source: message" only when the buffer has diagnostics from more than one
-- source. The built-in source = "if_many" counts per namespace (one per
-- server), so with ruff + basedpyright it labelled one and not the other,
-- and virtual_lines has no source option at all; one format covers all three.
local function several_sources(bufnr)
  local sources = {}
  for _, d in ipairs(vim.diagnostic.get(bufnr)) do
    if d.source then
      sources[d.source] = true
    end
  end
  return vim.tbl_count(sources) > 1
end

local function with_source(diagnostic)
  if diagnostic.source and several_sources(diagnostic.bufnr) then
    return ("%s: %s"):format(diagnostic.source, diagnostic.message)
  end
  return diagnostic.message
end

-- Each server's diagnostics are drawn when that server reports, so the first
-- one to report was drawn while it was still the only source. When a buffer
-- goes from one source to several (or back), redraw all of its diagnostics.
local labelled = {}
vim.api.nvim_create_autocmd("DiagnosticChanged", {
  group = vim.api.nvim_create_augroup("diagnostic-source-labels", { clear = true }),
  callback = function(ev)
    local several = several_sources(ev.buf)
    if labelled[ev.buf] ~= several then
      labelled[ev.buf] = several
      vim.schedule(function()
        if vim.api.nvim_buf_is_valid(ev.buf) then
          vim.diagnostic.show(nil, ev.buf)
        end
      end)
    end
  end,
  desc = "Redraw diagnostics when the number of sources crosses one",
})

vim.diagnostic.config({
  underline = true,
  update_in_insert = false,
  severity_sort = true,
  virtual_text = { spacing = 4, prefix = "●", current_line = false, format = with_source },
  virtual_lines = { current_line = true, format = with_source },
  float = { format = with_source },
  signs = { text = diagnostic_icons() },
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

-- Every server gets blink.cmp's completion capabilities (snippets, resolve,
-- label details; core/plugins.lua) and announces file-rename support, so
-- renames in oil (or any client of workspace/willRenameFiles +
-- didRenameFiles) reach all of them. Without blink (not installed yet) the
-- servers still start with Neovim's defaults.
local capabilities = {
  workspace = { fileOperations = { didRename = true, willRename = true } },
}
local blink_ok, blink = pcall(require, "blink.cmp")
if blink_ok then
  capabilities = blink.get_lsp_capabilities(capabilities)
end
lsp.config("*", { capabilities = capabilities })

-- All servers are on. A server whose binary is missing (mason still
-- installing it, or a toolchain not set up on this machine) is skipped
-- silently by Neovim. Adding one: a name here (lspconfig's config name),
-- the mason package in core/plugins.lua, overrides in after/lsp/<name>.lua.
lsp.enable({
  "basedpyright",
  "ruff",
  "vtsls",
  "eslint",
  "oxlint",
  "tailwindcss",
  "cssls",
  "lua_ls",
  "rust_analyzer",
  "gopls",
  "clangd", -- Xcode / system toolchain
  "ocamllsp", -- opam
  "hls", -- ghcup / brew
})

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

    -- Completion is blink.cmp (core/plugins.lua), not vim.lsp.completion.

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
