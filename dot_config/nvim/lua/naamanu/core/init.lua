-- Load order: options and keys first, then plugins (vim.pack), the theme,
-- LSP (servers enabled only when their binary is on PATH), format-on-save,
-- and the plugin-free helpers.
require("naamanu.core.options")
require("naamanu.core.keymaps")
require("naamanu.core.autocmds")
require("naamanu.core.plugins")

local theme = require("naamanu.theme")
theme.apply(theme.mode())

require("naamanu.core.lsp")
require("naamanu.core.format")
require("naamanu.core.workflows") -- terminal agent, docs lookup
