-- Typst. Formatting is conform's typstyle (plugins/formatting.lua); live
-- preview is typst-preview.nvim (plugins/writing.lua).
return {
  cmd = { "tinymist" },
  filetypes = { "typst" },
  root_markers = { "typst.toml", ".git" },
  settings = {
    exportPdf = "onSave",
  },
}
