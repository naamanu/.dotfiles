-- JSON schemas from SchemaStore.nvim (package.json, tsconfig, .eslintrc,
-- composer, GitHub workflows' JSON siblings, ...), matched by file name.
-- Formatting stays with conform (oxfmt/prettierd/prettier).
return {
  before_init = function(_, config)
    config.settings.json.schemas = require("schemastore").json.schemas()
  end,
  settings = {
    json = { validate = { enable = true } },
  },
}
