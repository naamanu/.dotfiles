return {
  -- Spectre: Search and replace across files
  {
    "nvim-pack/nvim-spectre",
    dependencies = { "nvim-lua/plenary.nvim" },
    cmd = "Spectre",
    opts = {
      open_cmd = "noswapfile vnew",
    },
    keys = {
      { "<leader>sr", function() require("spectre").open() end, desc = "Replace in files (Spectre)" },
      { "<leader>sw", function() require("spectre").open_visual({ select_word = true }) end, desc = "Search current word" },
      { "<leader>sw", function() require("spectre").open_visual() end, mode = "v", desc = "Search selection" },
      { "<leader>sp", function() require("spectre").open_file_search({ select_word = true }) end, desc = "Search in current file" },
    },
  },
}
