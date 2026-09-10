-- lectern.nvim — rendered markdown pane, and a tutor beside your work.
-- Local checkout, like the Emacs packages in ~/workspace/elisp; the spec is
-- skipped entirely when the checkout is absent so a fresh machine still starts.
local dir = vim.fn.expand("~/workspace/nvim/lectern.nvim")

if vim.fn.isdirectory(dir) == 0 then
  return {}
end

return {
  dir = dir,
  dependencies = { "folke/snacks.nvim", "MeanderingProgrammer/render-markdown.nvim" },
  cmd = {
    "Lectern",
    "LecternFile",
    "LecternBuffer",
    "LecternRefresh",
    "LecternClose",
    "LecternTutor",
    "LecternSkill",
    "LecternAttach",
  },
  opts = {},
  keys = {
    -- Markdown pane
    {
      "<leader>mm",
      function()
        require("lectern").toggle()
      end,
      desc = "Toggle markdown pane",
    },
    {
      "<leader>mb",
      function()
        require("lectern").show_buffer()
      end,
      desc = "Show buffer in pane",
    },
    {
      "<leader>mf",
      function()
        require("lectern").pick_file()
      end,
      desc = "Pick project markdown",
    },
    {
      "<leader>mn",
      function()
        require("lectern").pick_note()
      end,
      desc = "Pick a note",
    },
    {
      "<leader>mF",
      function()
        require("lectern").focus()
      end,
      desc = "Focus pane",
    },
    {
      "<leader>mr",
      function()
        require("lectern").refresh()
      end,
      desc = "Refresh pane",
    },
    {
      "<leader>md",
      function()
        require("lectern").scroll(1, true)
      end,
      desc = "Scroll pane down",
    },
    {
      "<leader>mu",
      function()
        require("lectern").scroll(-1, true)
      end,
      desc = "Scroll pane up",
    },

    -- Tutor
    {
      "<leader>tt",
      function()
        require("lectern").tutor_toggle()
      end,
      desc = "Toggle tutor",
    },
    {
      "<leader>ts",
      function()
        require("lectern").tutor_skill()
      end,
      desc = "Start a skill",
    },
    {
      "<leader>tq",
      function()
        require("lectern").tutor_close()
      end,
      desc = "Close tutor",
    },
    {
      "<leader>tf",
      function()
        require("lectern").tutor_attach("file")
      end,
      desc = "Attach this file",
    },
    {
      "<leader>td",
      function()
        require("lectern").tutor_attach("diagnostic")
      end,
      desc = "Attach diagnostic",
    },
    {
      "<leader>tp",
      function()
        require("lectern").tutor_attach("pane")
      end,
      desc = "Attach pane source",
    },
    {
      "<leader>tv",
      function()
        require("lectern").tutor_attach("selection")
      end,
      mode = "x",
      desc = "Attach selection",
    },
  },
}
