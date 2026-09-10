-- Load core modules in order
require("naamanu.core.options")
require("naamanu.core.keymaps")
require("naamanu.core.autocmds")
require("naamanu.core.lazy")
require("naamanu.core.lsp") -- native vim.lsp.config/enable; after lazy so lazy-loaded modules resolve

require("naamanu.core.workflows")
