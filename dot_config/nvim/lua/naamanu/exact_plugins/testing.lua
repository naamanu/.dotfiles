-- Test runner UI for the languages with maintained adapters. OCaml, SML and
-- Racket stay on overseer (`dune runtest`, `raco test`) via <leader>on and
-- the watch tasks (<localleader>w / W in core/keymaps.lua).
return {
  "nvim-neotest/neotest",
  dependencies = {
    "nvim-neotest/nvim-nio",
    "nvim-lua/plenary.nvim",
    "nvim-neotest/neotest-python",
    "mrcjkb/neotest-haskell",
    "fredrikaverpil/neotest-golang",
    "rouge8/neotest-rust",
  },
  keys = {
    {
      "<leader>Tt",
      function()
        require("neotest").run.run()
      end,
      desc = "Test nearest",
    },
    {
      "<leader>Tf",
      function()
        require("neotest").run.run(vim.fn.expand("%"))
      end,
      desc = "Test file",
    },
    {
      "<leader>TT",
      function()
        require("neotest").run.run(vim.uv.cwd())
      end,
      desc = "Test project",
    },
    {
      "<leader>Tl",
      function()
        require("neotest").run.run_last()
      end,
      desc = "Re-run last test",
    },
    {
      "<leader>Td",
      function()
        require("neotest").run.run({ strategy = "dap" })
      end,
      desc = "Debug nearest test",
    },
    {
      "<leader>Ts",
      function()
        require("neotest").summary.toggle()
      end,
      desc = "Test summary",
    },
    {
      "<leader>To",
      function()
        require("neotest").output.open({ enter = true, auto_close = true })
      end,
      desc = "Test output",
    },
    {
      "<leader>TO",
      function()
        require("neotest").output_panel.toggle()
      end,
      desc = "Test output panel",
    },
    {
      "<leader>Tw",
      function()
        require("neotest").watch.toggle(vim.fn.expand("%"))
      end,
      desc = "Watch file",
    },
    {
      "<leader>Tx",
      function()
        require("neotest").run.stop()
      end,
      desc = "Stop tests",
    },
  },
  config = function()
    require("neotest").setup({
      adapters = {
        require("neotest-python")({
          -- Same interpreter as basedpyright and dap: the project venv.
          python = function()
            return require("naamanu.core.tasks").python_executable()
          end,
        }),
        require("neotest-haskell"),
        require("neotest-golang"),
        require("neotest-rust"),
      },
      -- Results also land as diagnostics; keep them as signs only.
      diagnostic = { enabled = true, severity = vim.diagnostic.severity.ERROR },
    })
  end,
}
