-- YAML schemas from SchemaStore.nvim (GitHub workflows, docker-compose,
-- Kubernetes-ish files, pnpm-workspace, ...), matched by file name. The
-- server's own SchemaStore download is off: the plugin's catalog replaces it.
-- Formatting stays with conform (oxfmt/prettierd/prettier).
return {
  before_init = function(_, config)
    config.settings.yaml.schemas = require("schemastore").yaml.schemas()
  end,
  settings = {
    redhat = { telemetry = { enabled = false } },
    yaml = {
      keyOrdering = false,
      validate = true,
      schemaStore = { enable = false, url = "" },
    },
  },
}
