-- Diagnostic display toggles. The initial tables come from core/lsp.lua;
-- remember them on disable so re-enabling restores the same layout.
local function diagnostic_toggle(key, name)
  local saved
  return Snacks.toggle({
    name = name,
    get = function()
      local value = vim.diagnostic.config()[key]
      return value ~= nil and value ~= false
    end,
    set = function(state)
      if state then
        vim.diagnostic.config({ [key] = saved or true })
      else
        saved = vim.diagnostic.config()[key]
        vim.diagnostic.config({ [key] = false })
      end
    end,
  })
end

return {
  "folke/snacks.nvim",
  priority = 1000,
  lazy = false,
  opts = {
    bigfile = { enabled = true },
    notifier = { enabled = true, timeout = 3000 },
    quickfile = { enabled = true },
    words = { enabled = true },
    indent = { enabled = true },
    input = { enabled = true },
    scope = { enabled = true },
    dashboard = { enabled = true },
    scratch = { enabled = true },
    -- Emacs parity: dirvish-side sidebar, pixel-scroll, spacious gutters.
    explorer = { enabled = true, replace_netrw = false },
    scroll = { enabled = true },
    statuscolumn = { enabled = true },
    terminal = { enabled = true },
  },
  config = function(_, opts)
    require("snacks").setup(opts)

    -- <leader>u toggles (which-key shows the current state). <leader>ud/uh/uu/uc
    -- are taken by notifier dismiss/history, undotree and cloak.
    Snacks.toggle.option("wrap", { name = "Wrap" }):map("<leader>uw")
    Snacks.toggle.option("spell", { name = "Spelling" }):map("<leader>us")
    Snacks.toggle.option("relativenumber", { name = "Relative number" }):map("<leader>ur")
    Snacks.toggle.line_number():map("<leader>ul")
    Snacks.toggle.diagnostics():map("<leader>uD")
    Snacks.toggle.inlay_hints():map("<leader>ui")
    Snacks.toggle.treesitter():map("<leader>ut")
    Snacks.toggle.zoom():map("<leader>uz")
    diagnostic_toggle("virtual_text", "Diagnostic virtual text"):map("<leader>uv")
    diagnostic_toggle("virtual_lines", "Diagnostic virtual lines"):map("<leader>uV")
  end,
  keys = {
    -- <leader>uh (not <leader>n): the bare key is the notify group prefix.
    {
      "<leader>uh",
      function()
        Snacks.notifier.show_history()
      end,
      desc = "Notification history",
    },
    {
      "<leader>ud",
      function()
        Snacks.notifier.hide()
      end,
      desc = "Dismiss notifications",
    },
    {
      "<leader>z",
      function()
        Snacks.zen()
      end,
      desc = "Zen mode",
    },
    -- Sidebar and terminal toggles mirror Emacs SPC o / SPC ' (C-c ').
    {
      "<leader>e",
      function()
        Snacks.explorer()
      end,
      desc = "Explorer sidebar (toggle)",
    },
    {
      "<leader>'",
      function()
        Snacks.terminal.toggle(nil, { win = { position = "right", width = 0.4 } })
      end,
      desc = "Terminal (toggle, right)",
    },
    {
      "<C-/>",
      function()
        Snacks.terminal.toggle(nil, { win = { position = "right", width = 0.4 } })
      end,
      desc = "Terminal (toggle, right)",
      mode = { "n", "t" },
    },
    {
      "<C-_>",
      function()
        Snacks.terminal.toggle(nil, { win = { position = "right", width = 0.4 } })
      end,
      desc = "which_key_ignore",
      mode = { "n", "t" },
    },
    {
      "<leader>gg",
      function()
        Snacks.lazygit()
      end,
      desc = "LazyGit",
    },
    {
      "<leader>rf",
      function()
        Snacks.rename.rename_file()
      end,
      desc = "Rename file",
    },
    {
      "<leader>.",
      function()
        Snacks.scratch()
      end,
      desc = "Toggle scratch buffer",
    },
    {
      "<leader>S",
      function()
        Snacks.scratch.select()
      end,
      desc = "Select scratch buffer",
    },
    -- ]r / [r (not ]] / [[): those are Vim's section motions.
    {
      "]r",
      function()
        Snacks.words.jump(vim.v.count1)
      end,
      desc = "Next reference",
      mode = { "n", "t" },
    },
    {
      "[r",
      function()
        Snacks.words.jump(-vim.v.count1)
      end,
      desc = "Prev reference",
      mode = { "n", "t" },
    },
  },
}
