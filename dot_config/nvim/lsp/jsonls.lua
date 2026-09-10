-- schemastore.nvim (plugins/lsp.lua, lazy) holds the schema catalog. It is
-- loaded when the server starts, not at startup: vim.lsp.enable resolves this
-- file eagerly, so a top-level require would put 3 ms back on every launch.
-- The catalog is pushed the same way basedpyright pushes its interpreter.
return {
  cmd = { "vscode-json-language-server", "--stdio" },
  filetypes = { "json", "jsonc" },
  root_markers = { ".git" },
  init_options = { provideFormatter = false },
  on_init = function(client)
    client.settings = vim.tbl_deep_extend("force", vim.deepcopy(client.settings or {}), {
      json = { schemas = require("schemastore").json.schemas() },
    })
    client:notify("workspace/didChangeConfiguration", { settings = client.settings })
  end,
  settings = {
    json = {
      validate = { enable = true },
    },
  },
}
