-- Papers and documents: LaTeX through vimtex (latexmk, Skim forward/inverse
-- search) and Typst live preview. LSP for both is native (lsp/texlab.lua,
-- lsp/tinymist.lua); citations come from core/notes.lua (<leader>nc).
return {
  {
    "lervag/vimtex",
    -- vimtex must not be lazy-loaded: it sets up its filetype detection and
    -- inverse-search hooks at startup (:h vimtex-faq-lazy). Its startup cost is
    -- an ftdetect file; everything else loads on a tex buffer.
    lazy = false,
    init = function()
      vim.g.vimtex_view_method = vim.fn.isdirectory("/Applications/Skim.app") == 1 and "skim"
        or "general"
      vim.g.vimtex_compiler_method = "latexmk"
      vim.g.vimtex_quickfix_open_on_warning = 0
      -- texlab already provides completion; keep vimtex's out of blink's way.
      vim.g.vimtex_complete_enabled = 0
    end,
  },

  {
    "chomosuke/typst-preview.nvim",
    ft = "typst",
    version = "1.*",
    opts = {
      -- Use the tinymist on PATH (setup.sh) instead of a downloaded copy.
      dependencies_bin = { tinymist = "tinymist" },
    },
    keys = {
      { "<leader>cp", "<cmd>TypstPreviewToggle<CR>", ft = "typst", desc = "Typst preview" },
    },
  },
}
