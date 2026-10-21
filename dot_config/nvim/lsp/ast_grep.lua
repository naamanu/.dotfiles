-- ast-grep: structural lint rules and rewrites as diagnostics/code actions.
-- Only in projects that define rules (sgconfig.yml); elsewhere it has
-- nothing to report, so it must not attach.
return {
  cmd = { "ast-grep", "lsp" },
  filetypes = {
    "c",
    "cpp",
    "go",
    "haskell",
    "java",
    "javascript",
    "javascriptreact",
    "lua",
    "python",
    "rust",
    "typescript",
    "typescriptreact",
  },
  root_markers = { "sgconfig.yml", "sgconfig.yaml" },
  workspace_required = true,
}
