return {
  "mfussenegger/nvim-lint",
  event = { "BufReadPre", "BufNewFile" },
  config = function()
    local lint = require("lint")

    -- Python is deliberately absent: the ruff LSP already publishes the
    -- same diagnostics; running the CLI too doubles every message.
    lint.linters_by_ft = {
      sh = { "shellcheck" },
      bash = { "shellcheck" },
    }

    local lint_group = vim.api.nvim_create_augroup("nvim-lint", { clear = true })
    vim.api.nvim_create_autocmd({ "BufReadPost", "BufWritePost" }, {
      group = lint_group,
      callback = function()
        lint.try_lint()
      end,
    })

    vim.keymap.set("n", "<leader>ll", function()
      lint.try_lint()
    end, { desc = "Trigger linting" })
  end,
}
