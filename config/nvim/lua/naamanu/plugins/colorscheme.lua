return {
  "folke/tokyonight.nvim",
  lazy = false,
  priority = 1000,
  config = function()
    require("tokyonight").setup({
      style = "moon", -- "moon" is lighter than "night"
      transparent = true,
      terminal_colors = true,
      styles = {
        comments = { italic = true },
        keywords = { italic = true },
        functions = {},
        variables = {},
        sidebars = "transparent",
        floats = "transparent",
      },
      sidebars = { "qf", "help", "nvim-tree", "terminal", "Trouble" },
      dim_inactive = false,
      lualine_bold = true,
      on_colors = function(colors)
        -- Lighten background colors
        colors.bg = "NONE"
        colors.bg_dark = "NONE"
        colors.bg_float = "NONE"
        colors.bg_sidebar = "NONE"
      end,
      on_highlights = function(hl, c)
        -- Make all backgrounds transparent
        hl.Normal = { bg = "NONE", fg = c.fg }
        hl.NormalNC = { bg = "NONE", fg = c.fg }
        hl.NormalFloat = { bg = "NONE", fg = c.fg }
        hl.FloatBorder = { bg = "NONE", fg = c.blue }
        hl.SignColumn = { bg = "NONE" }
        hl.NvimTreeNormal = { bg = "NONE" }
        hl.NvimTreeNormalNC = { bg = "NONE" }
        hl.TelescopeNormal = { bg = "NONE" }
        hl.TelescopeBorder = { bg = "NONE", fg = c.blue }
        -- Better line number colors
        hl.LineNr = { fg = c.dark3 }
        hl.CursorLineNr = { fg = c.orange, bold = true }
        hl.CursorLine = { bg = "NONE" }
      end,
    })

    vim.cmd.colorscheme("tokyonight")
  end,
}
