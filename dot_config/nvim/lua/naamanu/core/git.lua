-- Git review (plugins installed by core/plugins.lua): gitsigns for hunks
-- and blame in the buffer, codediff for side-by-side diffs and history.

local map = function(mode, lhs, rhs, desc)
  vim.keymap.set(mode, lhs, rhs, { desc = desc })
end

-- --- Gitsigns ---------------------------------------------------------------

-- Blame: <leader>gb a popup for the line (full commit message), <leader>ga
-- the whole file in a side window (<CR> on a line opens its commit), and
-- <leader>ub toggles the end-of-line blame for the cursor line (off at start).
require("gitsigns").setup({
  current_line_blame = false,
  current_line_blame_opts = { delay = 300, virt_text_pos = "eol" },
  current_line_blame_formatter = "<author>, <author_time:%R> • <summary>",
  on_attach = function(bufnr)
    local gs = require("gitsigns")
    local bmap = function(mode, lhs, rhs, desc)
      vim.keymap.set(mode, lhs, rhs, { buffer = bufnr, desc = desc })
    end
    bmap("n", "]h", function()
      gs.nav_hunk("next")
    end, "Next hunk")
    bmap("n", "[h", function()
      gs.nav_hunk("prev")
    end, "Previous hunk")
    bmap("n", "<leader>gs", gs.stage_hunk, "Stage/unstage hunk")
    bmap("n", "<leader>gr", gs.reset_hunk, "Reset hunk")
    bmap("v", "<leader>gs", function()
      gs.stage_hunk({ vim.fn.line("."), vim.fn.line("v") })
    end, "Stage hunk")
    bmap("v", "<leader>gr", function()
      gs.reset_hunk({ vim.fn.line("."), vim.fn.line("v") })
    end, "Reset hunk")
    bmap("n", "<leader>gS", gs.stage_buffer, "Stage buffer")
    bmap("n", "<leader>gR", gs.reset_buffer, "Reset buffer")
    bmap("n", "<leader>gp", gs.preview_hunk, "Preview hunk")
    bmap("n", "<leader>gb", function()
      gs.blame_line({ full = true })
    end, "Blame line")
    bmap("n", "<leader>ga", gs.blame, "Blame file")
    bmap("n", "<leader>ub", gs.toggle_current_line_blame, "Toggle line blame")
    bmap("n", "<leader>gd", gs.diffthis, "Diff against index")
  end,
})

-- --- CodeDiff ---------------------------------------------------------------

-- Side-by-side diffs of the working tree, revisions and history. The explorer
-- is a 50-column tree with +/- line counts per file and folder, and the file
-- under the cursor opens as you move. Compact mode (gc in a diff, off at
-- start) folds everything but the changes and 8 lines around them, with folds
-- opened/closed in both panes together.
-- codediff's plugin/ file already sets its highlights; configuring through
-- codediff.config (not codediff.setup, which also loads the whole UI) keeps
-- the diff view out of startup until the first :CodeDiff.
require("codediff.config").setup({
  diff = {
    layout = "side-by-side",
    compact = false,
    compact_context_lines = 8,
    compact_sync_folds = true,
  },
  explorer = {
    view_mode = "tree",
    width = 50,
    auto_open_on_cursor = true,
    line_stats = { enabled = true },
  },
})
map("n", "<leader>gv", "<cmd>CodeDiff<CR>", "Diff view (changed files)")
map("n", "<leader>gl", "<cmd>CodeDiff history<CR>", "Git history")
map("n", "<leader>gL", "<cmd>CodeDiff history %<CR>", "Git history (this file)")
map("x", "<leader>gL", ":CodeDiff history<CR>", "Git history (selected lines)")
