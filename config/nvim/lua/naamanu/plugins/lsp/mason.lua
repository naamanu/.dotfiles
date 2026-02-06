return {
  "williamboman/mason.nvim",
  dependencies = {
    "williamboman/mason-lspconfig.nvim",
    "WhoIsSethDaniel/mason-tool-installer.nvim",
  },
  config = function()
    local mason = require("mason")
    local mason_lspconfig = require("mason-lspconfig")
    local mason_tool_installer = require("mason-tool-installer")

    mason.setup({
      ui = {
        border = "rounded",
        icons = {
          package_installed = "✓",
          package_pending = "➜",
          package_uninstalled = "✗",
        },
      },
    })

    mason_lspconfig.setup({
      ensure_installed = {
        -- Web
        "ts_ls",          -- TypeScript/JavaScript
        -- Systems
        "rust_analyzer",  -- Rust
        "gopls",          -- Go
        "clangd",         -- C/C++
        -- Scripting
        "lua_ls",         -- Lua
        "bashls",         -- Bash
        "pyright",        -- Python
        -- Data
        "jsonls",         -- JSON
        "yamlls",         -- YAML
        -- Functional (ML family)
        "ocamllsp",       -- OCaml/Reason
        "hls",            -- Haskell
        "elmls",          -- Elm
        "rescriptls",     -- ReScript
        "purescriptls",   -- PureScript
        -- Functional (Lisp family)
        "clojure_lsp",    -- Clojure
        -- Functional (BEAM)
        "elixirls",       -- Elixir
        "erlangls",       -- Erlang
      },
      automatic_installation = true,
    })

    mason_tool_installer.setup({
      ensure_installed = {
        "prettier", -- prettier formatter
        "stylua", -- lua formatter
        "eslint_d", -- js linter
        "gofumpt", -- go formatter
        "goimports", -- go imports formatter
        "golangci-lint", -- go linter
        "ocamlformat", -- ocaml formatter
        "elm-format", -- elm formatter
        "shellcheck", -- shell linter
        "shfmt", -- shell formatter
        "stylelint", -- css linter
      },
    })
  end,
}
