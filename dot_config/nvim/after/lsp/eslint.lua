-- nvim-lspconfig starts eslint only in projects with an ESLint config.
return {
  settings = {
    run = "onSave",
    quiet = true,
    format = false,
    codeActionOnSave = {
      enable = false,
      mode = "all",
    },
    workingDirectory = { mode = "auto" },
  },
}
