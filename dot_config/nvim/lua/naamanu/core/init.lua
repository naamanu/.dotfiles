-- Load order: options and keys first, then plugins (vim.pack, editing and
-- completion), the UI and git plugins, the theme, the tabline, LSP (servers
-- enabled only when their binary is on PATH), format-on-save, debugging and
-- the plugin-free helpers.
--
-- Each module loads in its own pcall: one that fails (a plugin missing on a
-- fresh or offline machine, a typo) cannot stop the ones after it, so the
-- editor always comes up usable. Failures are reported once, after startup,
-- in a single message naming each module.
local failed = {}

local function load(name, fn)
  local ok, err = pcall(fn or require, name)
  if not ok then
    failed[#failed + 1] = ("%s: %s"):format(name, err)
  end
end

load("naamanu.core.options")
load("naamanu.core.keymaps")
load("naamanu.core.autocmds")
load("naamanu.core.plugins")
load("naamanu.core.ui") -- icons, statusline, oil, snacks
load("naamanu.core.git") -- gitsigns, codediff
load("naamanu.theme", function(name)
  local theme = require(name)
  theme.apply(theme.mode())
end)
load("naamanu.core.tabline")
load("naamanu.core.lsp")
load("naamanu.core.format")
load("naamanu.core.dap") -- debug adapters (Delve)
load("naamanu.core.workflows") -- terminal agent, docs lookup

if #failed > 0 then
  vim.schedule(function()
    vim.notify(
      "Config modules failed to load:\n" .. table.concat(failed, "\n"),
      vim.log.levels.ERROR
    )
  end)
end
