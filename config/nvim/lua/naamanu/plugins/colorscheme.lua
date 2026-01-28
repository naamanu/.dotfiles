return {
  "tanvirtin/monokai.nvim",
  lazy = false,
  priority = 1000,
  config = function()
    require("monokai").setup({
      palette = require("monokai").pro,
      custom_hlgroups = {},
    })

    vim.cmd.colorscheme("monokai")
  end,
}
