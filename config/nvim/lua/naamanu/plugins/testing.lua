return {
  "nvim-neotest/neotest",
  dependencies = {
    "nvim-neotest/nvim-nio",
    "nvim-lua/plenary.nvim",
    "antoinemadec/FixCursorHold.nvim",
    "nvim-treesitter/nvim-treesitter",
    -- Adapters
    "nvim-neotest/neotest-go",
    "nvim-neotest/neotest-python",
    "rouge8/neotest-rust",
    "marilari88/neotest-vitest",
    "jfpedroza/neotest-elixir",
  },
  keys = {
    { "<leader>tt", function() require("neotest").run.run() end, desc = "Run Nearest Test" },
    { "<leader>tf", function() require("neotest").run.run(vim.fn.expand("%")) end, desc = "Run File Tests" },
    { "<leader>ts", function() require("neotest").summary.toggle() end, desc = "Toggle Test Summary" },
    { "<leader>to", function() require("neotest").output.open({ enter_on_open = true }) end, desc = "Show Test Output" },
    { "<leader>tp", function() require("neotest").output_panel.toggle() end, desc = "Toggle Output Panel" },
    { "<leader>td", function() require("neotest").run.run({ strategy = "dap" }) end, desc = "Debug Nearest Test" },
    { "<leader>tS", function() require("neotest").run.stop() end, desc = "Stop Test" },
  },
  config = function()
    require("neotest").setup({
      adapters = {
        require("neotest-go"),
        require("neotest-python"),
        require("neotest-rust"),
        require("neotest-vitest"),
        require("neotest-elixir"),
      },
      status = { virtual_text = true },
      output = { open_on_run = true },
    })
  end,
}
