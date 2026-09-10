return {
  "MeanderingProgrammer/render-markdown.nvim",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  ft = { "markdown" },
  opts = {
    heading = {
      enabled = true,
      icons = { "# ", "## ", "### ", "#### ", "##### ", "###### " },
    },
    code = {
      enabled = true,
      sign = false,
      width = "block",
      right_pad = 1,
    },
    checkbox = { enabled = true },
    bullet = { enabled = true },
  },
}
