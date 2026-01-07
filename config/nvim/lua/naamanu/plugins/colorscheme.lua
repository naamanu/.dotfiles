return {
  "projekt0n/github-nvim-theme",
  lazy = false,
  priority = 1000,
  config = function()
    require("github-theme").setup({
      options = {
        transparent = false,
        terminal_colors = true,
        styles = {
          comments = "italic",
          keywords = "italic",
          functions = "NONE",
          variables = "NONE",
        },
        darken = {
          sidebars = {
            list = { "qf", "help", "NvimTree", "terminal", "Trouble" },
          },
        },
      },
    })

    vim.cmd.colorscheme("github_light")
  end,
}
