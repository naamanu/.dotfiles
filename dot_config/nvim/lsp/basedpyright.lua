-- basedpyright matches the Emacs setup, so both editors report the same
-- Python diagnostics.  ruff owns imports and formatting.
--
-- The interpreter is resolved per client by core/lsp.lua (python_path, also
-- behind :PythonEnv); see the comment there for why basedpyright cannot be
-- left to find the venv itself.
return {
  cmd = { "basedpyright-langserver", "--stdio" },
  filetypes = { "python" },
  -- Nested = equal priority, so the *nearest* Python marker wins rather than
  -- a distant pyproject.toml outranking the manage.py next to the buffer.
  root_markers = {
    {
      "pyproject.toml",
      "setup.py",
      "setup.cfg",
      "requirements.txt",
      "uv.lock",
      "Pipfile",
      "manage.py",
    },
    ".git",
  },
  on_init = function(client)
    local interpreter = require("naamanu.core.lsp").python_path(client.root_dir)
    if not interpreter then
      return
    end
    -- Deep-copy first: `vim.lsp.config` hands the same resolved `settings`
    -- table to every client, and two Python projects open at once each
    -- need their own interpreter.
    client.settings = vim.tbl_deep_extend("force", vim.deepcopy(client.settings or {}), {
      python = { pythonPath = interpreter },
    })
    -- Nvim's automatic didChangeConfiguration fires just before on_init,
    -- so the interpreter has to be pushed again here.  basedpyright also
    -- pulls it back via workspace/configuration, which reads the same
    -- per-client table.
    client:notify("workspace/didChangeConfiguration", { settings = client.settings })
  end,
  settings = {
    basedpyright = {
      disableOrganizeImports = true,
      analysis = {
        autoImportCompletions = true,
        diagnosticMode = "openFilesOnly",
      },
    },
  },
}
