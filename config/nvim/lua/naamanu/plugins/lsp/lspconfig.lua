return {
  "neovim/nvim-lspconfig",
  event = { "BufReadPre", "BufNewFile" },
  dependencies = {
    "hrsh7th/cmp-nvim-lsp",
    { "antosha417/nvim-lsp-file-operations", config = true },
  },
  config = function()
    local cmp_nvim_lsp = require("cmp_nvim_lsp")

    -- Capabilities for autocompletion
    local capabilities = cmp_nvim_lsp.default_capabilities()

    -- Configure diagnostic signs
    local signs = { Error = " ", Warn = " ", Hint = "󰠠 ", Info = " " }
    for type, icon in pairs(signs) do
      local hl = "DiagnosticSign" .. type
      vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = "" })
    end

    -- LSP keybindings via LspAttach autocmd
    vim.api.nvim_create_autocmd("LspAttach", {
      group = vim.api.nvim_create_augroup("UserLspConfig", { clear = true }),
      callback = function(ev)
        local opts = { buffer = ev.buf, silent = true }

        opts.desc = "Show LSP references"
        vim.keymap.set("n", "gr", "<cmd>Telescope lsp_references<CR>", opts)

        opts.desc = "Go to declaration"
        vim.keymap.set("n", "gD", vim.lsp.buf.declaration, opts)

        opts.desc = "Show LSP definitions"
        vim.keymap.set("n", "gd", "<cmd>Telescope lsp_definitions<CR>", opts)

        opts.desc = "Show LSP implementations"
        vim.keymap.set("n", "gi", "<cmd>Telescope lsp_implementations<CR>", opts)

        opts.desc = "Show LSP type definitions"
        vim.keymap.set("n", "gt", "<cmd>Telescope lsp_type_definitions<CR>", opts)

        opts.desc = "See available code actions"
        vim.keymap.set({ "n", "v" }, "<leader>la", vim.lsp.buf.code_action, opts)

        opts.desc = "Smart rename"
        vim.keymap.set("n", "<leader>lr", vim.lsp.buf.rename, opts)

        opts.desc = "Show buffer diagnostics"
        vim.keymap.set("n", "<leader>ld", "<cmd>Telescope diagnostics bufnr=0<CR>", opts)

        opts.desc = "Show line diagnostics"
        vim.keymap.set("n", "<leader>lD", vim.diagnostic.open_float, opts)

        opts.desc = "Go to previous diagnostic"
        vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, opts)

        opts.desc = "Go to next diagnostic"
        vim.keymap.set("n", "]d", vim.diagnostic.goto_next, opts)

        opts.desc = "Show documentation for what is under cursor"
        vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)

        opts.desc = "Restart LSP"
        vim.keymap.set("n", "<leader>ls", "<cmd>LspRestart<CR>", opts)

        -- Disable hover for ruff in favor of other Python LSPs
        local client = vim.lsp.get_client_by_id(ev.data.client_id)
        if client and client.name == "ruff" then
          client.server_capabilities.hoverProvider = false
        end
      end,
    })

    -- Server configurations
    local servers = {
      -- Python (Ruff)
      ruff = {},

      -- TypeScript/JavaScript
      ts_ls = {
        filetypes = { "typescript", "typescriptreact", "javascript", "javascriptreact" },
      },

      -- Rust
      rust_analyzer = {
        settings = {
          ["rust-analyzer"] = {
            checkOnSave = true,
            cargo = {
              allFeatures = true,
            },
          },
        },
      },

      -- Go
      gopls = {
        settings = {
          gopls = {
            analyses = {
              unusedparams = true,
            },
            staticcheck = true,
            gofumpt = true,
          },
        },
      },

      -- C/C++
      clangd = {
        cmd = {
          "clangd",
          "--background-index",
          "--clang-tidy",
          "--header-insertion=iwyu",
          "--completion-style=detailed",
          "--function-arg-placeholders",
        },
      },

      -- Lua
      lua_ls = {
        settings = {
          Lua = {
            diagnostics = {
              globals = { "vim" },
            },
            workspace = {
              library = {
                [vim.fn.expand("$VIMRUNTIME/lua")] = true,
                [vim.fn.stdpath("config") .. "/lua"] = true,
              },
            },
          },
        },
      },

      -- Bash
      bashls = {},

      -- JSON
      jsonls = {},

      -- YAML
      yamlls = {},

      -- OCaml
      ocamllsp = {
        filetypes = { "ocaml", "ocaml.menhir", "ocaml.interface", "ocamllex", "reason" },
      },

      -- Haskell
      hls = {
        settings = {
          haskell = {
            formattingProvider = "ormolu",
          },
        },
      },

      -- Elm
      elmls = {},

      -- ReScript
      rescriptls = {
        cmd = { "rescript-language-server", "--stdio" },
      },

      -- PureScript
      purescriptls = {
        settings = {
          purescript = {
            addSpagoSources = true,
            formatter = "purs-tidy",
          },
        },
      },

      -- Clojure
      clojure_lsp = {},

      -- Elixir
      elixirls = {
        cmd = { "elixir-ls" },
      },

      -- Erlang
      erlangls = {},

      -- Racket
      racket_langserver = {},
    }

    -- Configure and enable all servers using vim.lsp.config
    for server, config in pairs(servers) do
      config.capabilities = vim.tbl_deep_extend("force", {}, capabilities, config.capabilities or {})
      vim.lsp.config(server, config)
      vim.lsp.enable(server)
    end
  end,
}
