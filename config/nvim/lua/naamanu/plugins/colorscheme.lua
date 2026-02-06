return {
  {
    "catppuccin/nvim",
    name = "catppuccin",
    lazy = false,
    priority = 1000,
    opts = {
      flavour = "mocha",
      transparent_background = false,
      integrations = {
        cmp = true,
        gitsigns = true,
        harpoon = true,
        illuminate = { enabled = true },
        mason = true,
        neotest = true,
        noice = true,
        notify = true,
        nvimtree = true,
        telescope = { enabled = true },
        treesitter = true,
        treesitter_context = true,
        which_key = true,
        flash = true,
        dap = true,
        dap_ui = true,
        indent_blankline = { enabled = true },
        native_lsp = {
          enabled = true,
          underlines = {
            errors = { "undercurl" },
            hints = { "undercurl" },
            warnings = { "undercurl" },
            information = { "undercurl" },
          },
        },
      },
    },
    config = function(_, opts)
      require("catppuccin").setup(opts)
      vim.cmd.colorscheme("catppuccin")
    end,
  },
  {
    "rebelot/kanagawa.nvim",
    lazy = false,
    opts = {
      compile = true,
      theme = "wave",
      background = { dark = "wave", light = "lotus" },
      overrides = function(colors)
        local theme = colors.theme
        return {
          NormalFloat = { bg = theme.ui.bg_p1 },
          FloatBorder = { bg = theme.ui.bg_p1 },
          FloatTitle = { bg = theme.ui.bg_p1 },
          TelescopeNormal = { bg = theme.ui.bg_p1 },
          TelescopeBorder = { bg = theme.ui.bg_p1 },
        }
      end,
    },
  },
  {
    "tanvirtin/monokai.nvim",
    lazy = false,
  },
}
