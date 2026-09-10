return {
  -- Oil (file explorer)
  {
    "stevearc/oil.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    -- Not lazy: netrw is disabled (core/lazy.lua), so oil has to be loaded
    -- before `nvim <dir>` opens its first directory buffer.
    lazy = false,
    opts = {
      default_file_explorer = true,
      columns = { "icon" },
      view_options = {
        show_hidden = true,
      },
    },
    keys = {
      { "-", "<cmd>Oil<CR>", desc = "Open parent directory" },
      { "<leader>E", "<cmd>Oil<CR>", desc = "File explorer (Oil)" },
    },
  },

  -- Picker: snacks.picker (fragment merged into the main snacks.nvim spec).
  -- ui_select is on by default once the picker is enabled, so code actions
  -- and other vim.ui.select prompts render as pickers too.
  {
    "folke/snacks.nvim",
    opts = {
      picker = {
        enabled = true,
        ui_select = true,
        sources = {
          files = { hidden = true },
        },
      },
    },
    keys = {
      {
        "<leader>ff",
        function()
          Snacks.picker.files()
        end,
        desc = "Find files",
      },
      {
        "<leader>fp",
        function()
          if vim.fs.root(0, ".git") then
            Snacks.picker.git_files({ untracked = true })
          else
            Snacks.picker.files()
          end
        end,
        desc = "Project files",
      },
      {
        "<leader>fr",
        function()
          Snacks.picker.recent()
        end,
        desc = "Recent files",
      },
      {
        "<leader>fg",
        function()
          Snacks.picker.grep()
        end,
        desc = "Live grep",
      },
      {
        "<leader>f/",
        function()
          Snacks.picker.lines()
        end,
        desc = "Search current buffer",
      },
      {
        "<leader>f.",
        function()
          Snacks.picker.resume()
        end,
        desc = "Resume last picker",
      },
      {
        "<leader>fc",
        function()
          Snacks.picker.grep_word()
        end,
        desc = "Find string under cursor",
        mode = { "n", "x" },
      },
      {
        "<leader>fb",
        function()
          Snacks.picker.buffers()
        end,
        desc = "Find buffers",
      },
      {
        "<leader>fh",
        function()
          Snacks.picker.help()
        end,
        desc = "Help tags",
      },
      {
        "<leader>fk",
        function()
          Snacks.picker.keymaps()
        end,
        desc = "Keymaps",
      },
      {
        "<leader>fd",
        function()
          Snacks.picker.diagnostics()
        end,
        desc = "Diagnostics",
      },
      {
        "<leader>fs",
        function()
          Snacks.picker.lsp_symbols()
        end,
        desc = "Document symbols",
      },
      {
        "<leader>fS",
        function()
          Snacks.picker.lsp_workspace_symbols()
        end,
        desc = "Workspace symbols",
      },
    },
  },

  -- Pane/window navigation shared with tmux. Needs the matching tmux plugin
  -- (declared in ~/.tmux.conf); this half is what hands focus back to tmux
  -- instead of swallowing C-h at the leftmost window.
  {
    "christoomey/vim-tmux-navigator",
    cmd = {
      "TmuxNavigateLeft",
      "TmuxNavigateDown",
      "TmuxNavigateUp",
      "TmuxNavigateRight",
      "TmuxNavigatePrevious",
    },
    keys = {
      { "<C-h>", "<cmd><C-U>TmuxNavigateLeft<cr>", desc = "Move to left window/pane" },
      { "<C-j>", "<cmd><C-U>TmuxNavigateDown<cr>", desc = "Move to lower window/pane" },
      { "<C-k>", "<cmd><C-U>TmuxNavigateUp<cr>", desc = "Move to upper window/pane" },
      { "<C-l>", "<cmd><C-U>TmuxNavigateRight<cr>", desc = "Move to right window/pane" },
      { "<C-\\>", "<cmd><C-U>TmuxNavigatePrevious<cr>", desc = "Move to previous window/pane" },
    },
  },
}
