-- Schemas come from schemastore.nvim, loaded on server start (see jsonls.lua).
return {
  cmd = { "yaml-language-server", "--stdio" },
  filetypes = { "yaml" },
  root_markers = { ".git" },
  on_init = function(client)
    client.settings = vim.tbl_deep_extend("force", vim.deepcopy(client.settings or {}), {
      yaml = { schemas = require("schemastore").yaml.schemas() },
    })
    client:notify("workspace/didChangeConfiguration", { settings = client.settings })
  end,
  settings = {
    yaml = {
      schemaStore = { enable = false, url = "" },
      validate = { enable = true },
    },
  },
}
