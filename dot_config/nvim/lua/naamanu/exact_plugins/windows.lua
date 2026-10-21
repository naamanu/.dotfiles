-- Docked panels: the tool windows (diagnostics, quickfix, tasks, tests,
-- help) always open in the same slot at the edge instead of splitting
-- whatever window happened to have focus. Terminals and the agent keep their
-- own snacks positions (plugins/snacks.lua, core/workflows.lua).
return {
  "folke/edgy.nvim",
  event = "VeryLazy",
  keys = {
    {
      "<leader>ue",
      function()
        require("edgy").toggle()
      end,
      desc = "Toggle panel docks",
    },
    {
      "<leader>uE",
      function()
        require("edgy").select()
      end,
      desc = "Pick a docked panel",
    },
  },
  opts = {
    animate = { enabled = false }, -- snacks.scroll already animates
    bottom = {
      { ft = "trouble", size = { height = 0.3 } },
      { ft = "qf", title = "Quickfix" },
      {
        ft = "help",
        size = { height = 20 },
        -- Only help buffers, not a markdown file opened with ft=help.
        filter = function(buf)
          return vim.bo[buf].buftype == "help"
        end,
      },
      { ft = "OverseerList", title = "Tasks", size = { height = 15 } },
      { ft = "neotest-output-panel", title = "Test output", size = { height = 15 } },
    },
    right = {
      { ft = "neotest-summary", title = "Tests", size = { width = 0.25 } },
    },
  },
}
