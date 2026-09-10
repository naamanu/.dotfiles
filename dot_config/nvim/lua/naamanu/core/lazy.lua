-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.uv.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- Setup lazy.nvim
require("lazy").setup({
  -- Import plugins from lua/naamanu/plugins
  spec = {
    { import = "naamanu.plugins" },
  },
  -- Updates are a deliberate `:Lazy sync` (then chezmoi add lazy-lock.json),
  -- not a background fetch of 40 repos on every start.
  checker = { enabled = false },
  -- Spec files are edited from the chezmoi loop, not live; skip the watcher.
  change_detection = { enabled = false },
  -- lazy passes its border explicitly, so 'winborder' does not reach it.
  ui = {
    border = "rounded",
  },
  rocks = {
    enabled = false,
  },
  performance = {
    rtp = {
      -- netrw: oil.nvim is the directory browser (default_file_explorer).
      -- matchparen stays: it is the cursor-adjacent bracket aid for Racket/OCaml.
      disabled_plugins = {
        "gzip",
        "matchit",
        "netrwPlugin",
        "rplugin",
        "spellfile",
        "tarPlugin",
        "tohtml",
        "tutor",
        "zipPlugin",
      },
    },
  },
})
