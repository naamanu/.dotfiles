local inlay_hints = {
  parameterNames = { enabled = "all", suppressWhenArgumentMatchesName = false },
  parameterTypes = { enabled = true },
  variableTypes = { enabled = true },
  propertyDeclarationTypes = { enabled = true },
  functionLikeReturnTypes = { enabled = true },
  enumMemberValues = { enabled = true },
}

return {
  cmd = { "vtsls", "--stdio" },
  filetypes = {
    "javascript",
    "javascriptreact",
    "typescript",
    "typescriptreact",
  },
  root_markers = { "tsconfig.json", "jsconfig.json", "package.json", ".git" },
  settings = {
    vtsls = {
      -- Use the project's own typescript from node_modules when present,
      -- so editor diagnostics agree with `tsc`.
      autoUseWorkspaceTsdk = true,
      experimental = {
        completion = { enableServerSideFuzzyMatch = true },
      },
      tsserver = { globalPlugins = {} },
    },
    typescript = {
      updateImportsOnFileMove = { enabled = "always" },
      inlayHints = inlay_hints,
    },
    javascript = {
      updateImportsOnFileMove = { enabled = "always" },
      inlayHints = inlay_hints,
    },
  },
}
