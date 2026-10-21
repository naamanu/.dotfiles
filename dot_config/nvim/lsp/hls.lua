return {
  cmd = { "haskell-language-server-wrapper", "--lsp" },
  filetypes = { "haskell", "lhaskell", "cabal" },
  root_markers = { "hie.yaml", "stack.yaml", "cabal.project", "package.yaml", ".git" },
  settings = {
    haskell = {
      formattingProvider = "ormolu",
    },
  },
}
