return {
  "ocaml-mlx/ocaml_mlx.nvim",
  dependencies = {
    "neovim/nvim-lspconfig",
    "nvim-treesitter/nvim-treesitter",
  },
  event = "VeryLazy",
  config = function()
    -- Register the custom tree-sitter parser
    local parser_configs = require("nvim-treesitter.parsers").get_parser_configs()
    parser_configs.ocaml_mlx = {
      install_info = {
        url = "https://github.com/ocaml-mlx/tree-sitter-mlx",
        files = { "src/scanner.c", "src/parser.c" },
        location = "grammars/mlx",
        branch = "master",
      },
      filetype = "ocaml.mlx",
    }

    require("ocaml_mlx")
  end,
}
