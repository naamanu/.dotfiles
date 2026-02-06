return {
  "stevearc/conform.nvim",
  event = { "BufReadPre", "BufNewFile" },
  config = function()
    local conform = require("conform")

    conform.setup({
      formatters_by_ft = {
        javascript = { "prettier" },
        typescript = { "prettier" },
        javascriptreact = { "prettier" },
        typescriptreact = { "prettier" },
        svelte = { "prettier" },
        css = { "prettier" },
        html = { "prettier" },
        json = { "prettier" },
        yaml = { "prettier" },
        markdown = { "prettier" },
        graphql = { "prettier" },
        liquid = { "prettier" },
        lua = { "stylua" },
        python = { "ruff_fix", "ruff_format" },
        go = { "gofumpt", "goimports" },
        rust = { "rustfmt" },
        ocaml = { "ocamlformat" },
        ocaml_interface = { "ocamlformat" },
        elm = { "elm_format" },
        sh = { "shfmt" },
        elixir = { "mix" },
        erlang = { "erlfmt" },
        clojure = { "zprint" },
      },
      formatters = {
        ocamlformat = {
          prepend_args = function()
            local ext = vim.fn.expand("%:e")
            if ext == "mli" then
              return { "--intf" }
            end
            return { "--impl" }
          end,
        },
      },
      format_on_save = {
        lsp_fallback = true,
        async = false,
        timeout_ms = 1000,
      },
    })

    vim.keymap.set({ "n", "v" }, "<leader>mp", function()
      conform.format({
        lsp_fallback = true,
        async = false,
        timeout_ms = 1000,
      })
    end, { desc = "Format file or range (in visual mode)" })
  end,
}
