-- which-key groups. Never bind a bare key that is also a group prefix.
local groups = {
  { "<leader>a", group = "agent" },
  { "<leader>b", group = "buffer" },
  { "<leader>c", group = "code" },
  { "<leader>d", group = "debug" },
  { "<leader>f", group = "find" },
  { "<leader>g", group = "git" },
  { "<leader>l", group = "lsp" },
  { "<leader>n", group = "notes" },
  { "<leader>o", group = "overseer" },
  { "<leader>r", group = "rename" },
  { "<leader>R", group = "REST/HTTP" },
  { "<leader>s", group = "split" },
  { "<leader>u", group = "ui/toggle" },
  { "<leader>x", group = "trouble" },
  { "<leader><tab>", group = "tabs/sessions" },
  { "<localleader>", group = "repl" },
}

-- lectern.nvim groups only exist when its checkout does (plugins/lectern.lua).
if vim.fn.isdirectory(vim.fn.expand("~/workspace/nvim/lectern.nvim")) == 1 then
  table.insert(groups, { "<leader>m", group = "markdown pane" })
  table.insert(groups, { "<leader>t", group = "tutor" })
end

return {
  -- Lualine
  {
    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    event = "VeryLazy",
    opts = {
      options = {
        theme = "auto",
        component_separators = { left = "|", right = "|" },
        section_separators = { left = "", right = "" },
        globalstatus = true,
      },
      sections = {
        lualine_a = { "mode" },
        lualine_b = { "branch", "diff", "diagnostics" },
        lualine_c = { { "filename", path = 1 } },
        lualine_x = { "encoding", "fileformat", "filetype" },
        lualine_y = { "progress" },
        lualine_z = { "location" },
      },
    },
  },

  -- Which-key
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    opts = {
      spec = groups,
    },
  },

  -- Breadcrumbs in the winbar: project › dir › file › symbol (Emacs: breadcrumb)
  {
    "Bekaboo/dropbar.nvim",
    event = "VeryLazy",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    keys = {
      {
        "<leader>;",
        function()
          require("dropbar.api").pick()
        end,
        desc = "Breadcrumb pick",
      },
    },
    opts = {},
  },

  -- Rainbow delimiters (Emacs: rainbow-delimiters). Only where nesting depth is
  -- the reading problem: Lisps and the ML family (queries/ocaml/ is local).
  {
    "HiPhish/rainbow-delimiters.nvim",
    ft = {
      "racket",
      "scheme",
      "lisp",
      "clojure",
      "fennel",
      "query",
      "ocaml",
      "ocaml_interface",
      "haskell",
    },
    config = function()
      require("rainbow-delimiters.setup").setup({})
    end,
  },

  -- Trouble
  {
    "folke/trouble.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    cmd = "Trouble",
    keys = {
      { "<leader>xx", "<cmd>Trouble diagnostics toggle<CR>", desc = "Diagnostics (Trouble)" },
      {
        "<leader>xX",
        "<cmd>Trouble diagnostics toggle filter.buf=0<CR>",
        desc = "Buffer diagnostics (Trouble)",
      },
      { "<leader>xQ", "<cmd>Trouble qflist toggle<CR>", desc = "Quickfix list (Trouble)" },
      { "<leader>cs", "<cmd>Trouble symbols toggle focus=false<CR>", desc = "Symbols (Trouble)" },
      {
        "<leader>cl",
        "<cmd>Trouble lsp toggle focus=false win.position=right<CR>",
        desc = "LSP refs/defs (Trouble)",
      },
    },
    opts = {},
  },
}
