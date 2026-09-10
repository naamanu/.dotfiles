-- ruff owns imports and formatting for Python; basedpyright owns types.
return {
  cmd = { "ruff", "server" },
  filetypes = { "python" },
  root_markers = {
    { "pyproject.toml", "ruff.toml", ".ruff.toml", "uv.lock", "requirements.txt", "manage.py" },
    ".git",
  },
  -- basedpyright only speaks utf-16; offering ruff the same keeps both clients
  -- on one position encoding per buffer (:checkhealth vim.lsp warns otherwise).
  capabilities = { general = { positionEncodings = { "utf-16" } } },
}
