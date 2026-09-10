return {
  "saghen/blink.cmp",
  version = "*",
  dependencies = {
    "rafamadriz/friendly-snippets",
  },
  event = "InsertEnter",
  opts = {
    keymap = {
      preset = "none",
      ["<C-j>"] = { "select_next", "fallback" },
      ["<C-k>"] = { "select_prev", "fallback" },
      ["<Tab>"] = { "snippet_forward", "select_next", "fallback" },
      ["<S-Tab>"] = { "snippet_backward", "select_prev", "fallback" },
      ["<CR>"] = { "accept", "fallback" },
      ["<C-Space>"] = { "show", "fallback" },
      ["<C-e>"] = { "cancel", "fallback" },
      ["<C-b>"] = { "scroll_documentation_up", "fallback" },
      ["<C-f>"] = { "scroll_documentation_down", "fallback" },
    },
    snippets = {
      -- Some servers send insertTextFormat=Snippet with an unescaped `$`,
      -- which the LSP spec requires to be written `\$`. HLS does this for
      -- Haskell's operator family -- `($)`, `<$>`, `($!)`, `f $ x` -- and
      -- vim.snippet's parser correctly rejects the body, which surfaced as
      -- "snippet parsing failed" on every such completion. Escape the stray
      -- dollars and retry, then fall back to plain text, so a malformed
      -- snippet degrades instead of erroring out of the completion.
      expand = function(snippet)
        if pcall(vim.snippet.expand, snippet) then
          return
        end
        -- A `$` is only meaningful before a digit, `{`, or a variable name.
        local escaped = snippet:gsub("%$([^%w_{])", "\\$%1"):gsub("%$$", "\\$")
        if pcall(vim.snippet.expand, escaped) then
          return
        end
        local literal = snippet:gsub("%${%d+:([^}]*)}", "%1"):gsub("%${%d+}", ""):gsub("%$%d+", "")
        vim.api.nvim_put(vim.split(literal, "\n", { plain = true }), "c", false, true)
      end,
      active = function(filter)
        if filter and filter.direction then
          return vim.snippet.active({ direction = filter.direction })
        end
        return vim.snippet.active()
      end,
      jump = function(direction)
        vim.snippet.jump(direction)
      end,
    },
    sources = {
      default = { "lazydev", "lsp", "path", "snippets", "buffer" },
      providers = {
        lazydev = {
          name = "LazyDev",
          module = "lazydev.integrations.blink",
          score_offset = 100,
        },
      },
    },
    completion = {
      documentation = {
        auto_show = true,
        auto_show_delay_ms = 200,
      },
      list = {
        selection = { preselect = true, auto_insert = false },
      },
    },
    signature = {
      enabled = true,
    },
  },
}
