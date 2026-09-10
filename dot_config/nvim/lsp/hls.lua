return {
  cmd = { "haskell-language-server-wrapper" },
  filetypes = { "haskell", "lhaskell", "cabal" },
  root_markers = { "hie.yaml", "stack.yaml", "cabal.project", "package.yaml", ".git" },
  settings = {
    haskell = {
      formattingProvider = "ormolu",
    },
  },
}
