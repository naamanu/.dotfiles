return {
  -- Autopairs
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    opts = {},
  },

  -- Surround
  {
    "kylechui/nvim-surround",
    version = "*",
    event = "VeryLazy",
    opts = {},
  },

  -- Commenting: builtin gc/gcc (Neovim 0.10+); the runtime ftplugins set
  -- commentstring, including // for JS/TS.

  -- Todo comments
  {
    "folke/todo-comments.nvim",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = { "nvim-lua/plenary.nvim" },
    opts = {},
  },

  -- Flash (quick motions)
  {
    "folke/flash.nvim",
    event = "VeryLazy",
    opts = {},
    keys = {
      {
        "<leader><leader>",
        mode = { "n", "x", "o" },
        function()
          require("flash").jump()
        end,
        desc = "Flash",
      },
      {
        "<leader>fj",
        mode = { "n", "x", "o" },
        function()
          require("flash").treesitter()
        end,
        desc = "Flash Treesitter",
      },
      {
        "<leader>fr",
        mode = "o",
        function()
          require("flash").remote()
        end,
        desc = "Remote Flash",
      },
      {
        "<leader>fR",
        mode = { "o", "x" },
        function()
          require("flash").treesitter_search()
        end,
        desc = "Treesitter Search",
      },
    },
  },

  -- Undotree.  <leader>uu (not <leader>u) so the ui/toggle which-key group
  -- prefix does not collide with a standalone action.
  {
    "mbbill/undotree",
    keys = {
      { "<leader>uu", "<cmd>UndotreeToggle<CR>", desc = "Toggle undotree" },
    },
  },

  -- Zen mode: snacks.zen (<leader>z, bound in snacks.lua).

  -- Treesj (split / join)
  {
    "Wansmer/treesj",
    keys = {
      {
        "<leader>j",
        function()
          require("treesj").toggle()
        end,
        desc = "Split / join under cursor",
      },
    },
    opts = { use_default_keymaps = false, max_join_length = 200 },
  },
}
