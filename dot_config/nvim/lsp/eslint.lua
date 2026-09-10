return {
  cmd = { "vscode-eslint-language-server", "--stdio" },
  filetypes = {
    "javascript",
    "javascriptreact",
    "typescript",
    "typescriptreact",
    "vue",
  },
  root_markers = {
    "eslint.config.js",
    "eslint.config.cjs",
    "eslint.config.mjs",
    "eslint.config.ts",
    ".eslintrc",
    ".eslintrc.js",
    ".eslintrc.cjs",
    ".eslintrc.json",
  },
  -- Without a config file there is nothing for the server to do; the
  -- native default (workspace_required = false) would still spawn it with
  -- root_dir = nil in every JS/TS project that lacks ESLint.
  workspace_required = true,
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
