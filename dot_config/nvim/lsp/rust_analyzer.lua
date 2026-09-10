return {
  cmd = { "rust-analyzer" },
  filetypes = { "rust" },
  root_markers = { "Cargo.toml", ".git" },
  settings = {
    ["rust-analyzer"] = {
      checkOnSave = true,
      check = { command = "clippy" },
      cargo = { allFeatures = true },
      inlayHints = {
        bindingModeHints = { enable = true },
        closureReturnTypeHints = { enable = "always" },
        lifetimeElisionHints = { enable = "always" },
      },
    },
  },
}
