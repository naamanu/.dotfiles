-- Which family and variant load is decided by naamanu.theme: the family from
-- Neovim's own state file (<leader>uC), the light/dark half from the shared
-- mode file, so a Neovim started while the desktop is light comes up light.
-- Every theme is eager (~2 ms in total): lazy ones are invisible to
-- `:colorscheme <Tab>` and other colorscheme pickers until first loaded.
return {
  {
    "miikanissi/modus-themes.nvim",
    priority = 1000,
    lazy = false,
    config = function()
      local theme = require("naamanu.theme")
      theme.apply(theme.mode())
    end,
  },
  { "rebelot/kanagawa.nvim", lazy = false },
  { "catppuccin/nvim", name = "catppuccin", lazy = false },
  { "rose-pine/neovim", name = "rose-pine", lazy = false },
  { "folke/tokyonight.nvim", lazy = false },
  { "ellisonleao/gruvbox.nvim", lazy = false },
  { "neanias/everforest-nvim", lazy = false },
  { "savq/melange-nvim", lazy = false },
  { "Mofiqul/vscode.nvim", lazy = false },
  { "EdenEast/nightfox.nvim", lazy = false },
  { "projekt0n/github-nvim-theme", name = "github-theme", lazy = false },
  { "navarasu/onedark.nvim", lazy = false },
}
