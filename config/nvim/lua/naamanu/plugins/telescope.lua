return {
  "nvim-telescope/telescope.nvim",
  branch = "0.1.x",
  dependencies = {
    "nvim-lua/plenary.nvim",
    { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
    "nvim-tree/nvim-web-devicons",
    "nvim-telescope/telescope-frecency.nvim",
    "debugloop/telescope-undo.nvim",
    "nvim-telescope/telescope-ui-select.nvim",
  },
  config = function()
    local telescope = require("telescope")
    local actions = require("telescope.actions")

    telescope.setup({
      defaults = {
        path_display = { "truncate" },
        mappings = {
          i = {
            ["<C-k>"] = actions.move_selection_previous,
            ["<C-j>"] = actions.move_selection_next,
            ["<C-q>"] = actions.send_selected_to_qflist + actions.open_qflist,
          },
        },
      },
      pickers = {
        find_files = {
          hidden = true,
        },
      },
      extensions = {
        frecency = {
          show_scores = false,
          show_unindexed = true,
          ignore_patterns = { "*.git/*", "*/tmp/*", "*/node_modules/*" },
        },
        undo = {
          side_by_side = true,
          layout_strategy = "vertical",
          layout_config = {
            preview_height = 0.8,
          },
        },
        ["ui-select"] = {
          require("telescope.themes").get_dropdown(),
        },
      },
    })

    telescope.load_extension("fzf")
    telescope.load_extension("frecency")
    telescope.load_extension("undo")
    telescope.load_extension("ui-select")

    -- Set keymaps
    local keymap = vim.keymap

    keymap.set("n", "<leader>ff", "<cmd>Telescope frecency<CR>", { desc = "Find files (frecency)" })
    keymap.set("n", "<leader>fF", "<cmd>Telescope find_files<CR>", { desc = "Find files (all)" })
    keymap.set("n", "<leader>fr", "<cmd>Telescope oldfiles<CR>", { desc = "Find recent files" })
    keymap.set("n", "<leader>fg", "<cmd>Telescope live_grep<CR>", { desc = "Live grep" })
    keymap.set("n", "<leader>fc", "<cmd>Telescope grep_string<CR>", { desc = "Find string under cursor" })
    keymap.set("n", "<leader>fb", "<cmd>Telescope buffers<CR>", { desc = "Find buffers" })
    keymap.set("n", "<leader>fh", "<cmd>Telescope help_tags<CR>", { desc = "Find help" })
    keymap.set("n", "<leader>fk", "<cmd>Telescope keymaps<CR>", { desc = "Find keymaps" })
    keymap.set("n", "<leader>ft", "<cmd>Telescope colorscheme<CR>", { desc = "Find colorschemes" })
    keymap.set("n", "<leader>fu", "<cmd>Telescope undo<CR>", { desc = "Undo tree" })
  end,
}
