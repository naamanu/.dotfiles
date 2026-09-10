-- Shared light/dark mode.
--
-- The mode lives in $XDG_STATE_HOME/theme-mode so Neovim, Emacs, fish, tmux,
-- Ghostty and starship all agree on it. theme-mode(1) writes that file and then
-- pushes the change into running sessions; this module is both halves for
-- Neovim -- the startup read, and the entry point that push targets.

local M = {}

local function state_file()
  local base = vim.env.XDG_STATE_HOME
  if not base or base == "" then
    base = vim.env.HOME .. "/.local/state"
  end
  return base .. "/theme-mode"
end

--- Current mode, defaulting to dark when the file is missing or unreadable.
function M.mode()
  local f = io.open(state_file(), "r")
  if not f then
    return "dark"
  end
  local line = f:read("l") or ""
  f:close()
  return vim.trim(line) == "light" and "light" or "dark"
end

--- Apply MODE. Called at startup and by `theme-mode` over the RPC socket.
function M.apply(mode)
  mode = (mode == "light") and "light" or "dark"
  local scheme = (mode == "light") and "modus_operandi" or "modus_vivendi"
  require("modus-themes").setup({
    style = scheme,
    variants = {
      modus_vivendi = "tinted", -- matches Emacs (modus-vivendi-tinted)
    },
    dim_inactive = false,
    styles = {
      comments = { italic = true },
      keywords = { italic = true },
    },
  })
  vim.o.background = mode
  vim.cmd.colorscheme(scheme)
  return mode
end

-- theme-mode pushes over the RPC socket; a session that was detached, suspended
-- or started without a server socket misses the push. Re-read the state file
-- when focus returns so the mismatch heals itself.
vim.api.nvim_create_autocmd({ "FocusGained", "VimResume" }, {
  group = vim.api.nvim_create_augroup("theme-mode-resync", { clear = true }),
  callback = function()
    local mode = M.mode()
    if vim.o.background ~= mode then
      M.apply(mode)
    end
  end,
  desc = "Re-apply the shared light/dark mode after a missed push",
})

return M
