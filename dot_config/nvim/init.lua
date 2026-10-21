-- Byte-code cache for every Lua module loaded below.
vim.loader.enable()

-- Set leader keys before loading plugins
vim.g.mapleader = " "
vim.g.maplocalleader = ","

-- Bootstrap core configuration
require("naamanu.core")
