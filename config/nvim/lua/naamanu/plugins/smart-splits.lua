return {
  "mrjones2014/smart-splits.nvim",
  lazy = false,
  config = function()
    local smart_splits = require("smart-splits")

    smart_splits.setup({
      ignored_buftypes = { "nofile", "quickfix", "prompt" },
      ignored_filetypes = { "NvimTree" },
      default_amount = 3,
      at_edge = "wrap",
      cursor_follows_swapped_bufs = true,
    })

    -- Navigation: <C-h/j/k/l> moves between Neovim splits AND tmux panes
    vim.keymap.set("n", "<C-h>", smart_splits.move_cursor_left, { desc = "Move to left split/pane" })
    vim.keymap.set("n", "<C-j>", smart_splits.move_cursor_down, { desc = "Move to below split/pane" })
    vim.keymap.set("n", "<C-k>", smart_splits.move_cursor_up, { desc = "Move to above split/pane" })
    vim.keymap.set("n", "<C-l>", smart_splits.move_cursor_right, { desc = "Move to right split/pane" })

    -- Resizing: <A-h/j/k/l> resizes Neovim splits AND tmux panes
    vim.keymap.set("n", "<A-h>", smart_splits.resize_left, { desc = "Resize split/pane left" })
    vim.keymap.set("n", "<A-j>", smart_splits.resize_down, { desc = "Resize split/pane down" })
    vim.keymap.set("n", "<A-k>", smart_splits.resize_up, { desc = "Resize split/pane up" })
    vim.keymap.set("n", "<A-l>", smart_splits.resize_right, { desc = "Resize split/pane right" })

    -- Swap buffers: <leader><leader>h/j/k/l swaps buffer with adjacent
    vim.keymap.set("n", "<leader><leader>h", smart_splits.swap_buf_left, { desc = "Swap buffer left" })
    vim.keymap.set("n", "<leader><leader>j", smart_splits.swap_buf_down, { desc = "Swap buffer down" })
    vim.keymap.set("n", "<leader><leader>k", smart_splits.swap_buf_up, { desc = "Swap buffer up" })
    vim.keymap.set("n", "<leader><leader>l", smart_splits.swap_buf_right, { desc = "Swap buffer right" })
  end,
}
