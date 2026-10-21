-- LaTeX completion, references and citation keys. Building and the viewer
-- are vimtex's (plugins/writing.lua), so texlab's own build is left off.
return {
  cmd = { "texlab" },
  filetypes = { "tex", "plaintex", "bib" },
  root_markers = { ".latexmkrc", "latexmkrc", ".texlabroot", "texlabroot", ".git" },
  settings = {
    texlab = {
      build = { onSave = false },
      chktex = { onOpenAndSave = false },
    },
  },
}
