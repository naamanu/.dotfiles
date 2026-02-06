return {
  "nvim-treesitter/nvim-treesitter",
  build = ":TSUpdate",
  event = { "BufReadPost", "BufNewFile" },
  dependencies = {
    "nvim-treesitter/nvim-treesitter-textobjects",
    "nvim-treesitter/nvim-treesitter-context",
    "windwp/nvim-ts-autotag",
  },
  config = function()
    require("treesitter-context").setup({ enable = true })
    require("nvim-ts-autotag").setup()
    require("nvim-treesitter.configs").setup({
      ensure_installed = {
        -- Web
        "typescript",
        "tsx",
        "javascript",
        "html",
        "css",
        -- Systems
        "rust",
        "go",
        "c",
        "cpp",
        -- Scripting
        "lua",
        "vim",
        "vimdoc",
        "bash",
        "python",
        -- Data
        "json",
        "yaml",
        "toml",
        "markdown",
        "markdown_inline",
        -- Functional (ML family)
        "ocaml",
        "ocaml_interface",
        "rescript",
        "haskell",
        "elm",
        "gleam",
        "purescript",
        -- Functional (Lisp family)
        "clojure",
        "racket",
        "scheme",
        "commonlisp",
        -- Functional (BEAM)
        "erlang",
        "elixir",
        "heex",
        -- Functional (Other)
        "scala",
        "nix",
      },
      auto_install = true,
      highlight = {
        enable = true,
        additional_vim_regex_highlighting = false,
      },
      indent = {
        enable = true,
      },
      incremental_selection = {
        enable = true,
        keymaps = {
          init_selection = "<C-space>",
          node_incremental = "<C-space>",
          scope_incremental = false,
          node_decremental = "<bs>",
        },
      },
      textobjects = {
        select = {
          enable = true,
          lookahead = true,
          keymaps = {
            ["af"] = "@function.outer",
            ["if"] = "@function.inner",
            ["ac"] = "@class.outer",
            ["ic"] = "@class.inner",
          },
        },
        move = {
          enable = true,
          set_jumps = true,
          goto_next_start = {
            ["]f"] = "@function.outer",
            ["]c"] = "@class.outer",
          },
          goto_previous_start = {
            ["[f"] = "@function.outer",
            ["[c"] = "@class.outer",
          },
        },
      },
    })
  end,
}
