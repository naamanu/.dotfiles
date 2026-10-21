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

-- Neovim-only theme family; every family has a light and a dark half so the
-- shared mode still picks between them. Other tools stay on modus.
M.families = {
  modus = {
    dark = "modus_vivendi",
    light = "modus_operandi",
    setup = function(scheme)
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
    end,
  },
  kanagawa = { dark = "kanagawa-wave", light = "kanagawa-lotus" },
  catppuccin = { dark = "catppuccin-mocha", light = "catppuccin-latte" },
  ["rose-pine"] = { dark = "rose-pine-main", light = "rose-pine-dawn" },
  tokyonight = { dark = "tokyonight-night", light = "tokyonight-day" },
  -- Schemes with one name for both halves pick the variant from 'background'.
  gruvbox = { dark = "gruvbox", light = "gruvbox" },
  everforest = { dark = "everforest", light = "everforest" },
  melange = { dark = "melange", light = "melange" },
  vscode = { dark = "vscode", light = "vscode" },
  nightfox = { dark = "nightfox", light = "dayfox" },
  carbonfox = { dark = "carbonfox", light = "dawnfox" },
  github = { dark = "github_dark_dimmed", light = "github_light" },
  onedark = {
    dark = "onedark",
    light = "onedark",
    setup = function(_, mode)
      require("onedark").setup({ style = mode == "light" and "light" or "dark" })
    end,
  },
  ["kanagawa-dragon"] = { dark = "kanagawa-dragon", light = "kanagawa-lotus" },
  ["catppuccin-macchiato"] = { dark = "catppuccin-macchiato", light = "catppuccin-latte" },
  ["rose-pine-moon"] = { dark = "rose-pine-moon", light = "rose-pine-dawn" },
  ["tokyonight-storm"] = { dark = "tokyonight-storm", light = "tokyonight-day" },
}

-- One-line descriptions for the picker.
M.descriptions = {
  modus = "WCAG-AAA contrast, matches Emacs",
  kanagawa = "muted ink painting (wave / lotus)",
  ["kanagawa-dragon"] = "kanagawa, darker and earthier",
  catppuccin = "soft pastels (mocha / latte)",
  ["catppuccin-macchiato"] = "catppuccin, medium dark",
  ["rose-pine"] = "calm, low contrast (main / dawn)",
  ["rose-pine-moon"] = "rose-pine, cooler dark",
  tokyonight = "vivid neon (night / day)",
  ["tokyonight-storm"] = "tokyonight, softer dark",
  gruvbox = "retro warm, high character",
  everforest = "green, soft, very easy on the eyes",
  melange = "warm, cozy, earth tones",
  vscode = "VS Code Dark+ / Light+",
  nightfox = "balanced blues (nightfox / dayfox)",
  carbonfox = "IBM Carbon-inspired dark (dawnfox light)",
  github = "GitHub dimmed / light",
  onedark = "Atom One Dark / Light",
}

local function family_file()
  return vim.fn.stdpath("state") .. "/theme-family"
end

-- The state file holds `family` (follow theme-mode) or `family:dark` /
-- `family:light` (Neovim pinned to that half whatever the desktop does).
local function read_selection()
  local f = io.open(family_file(), "r")
  local line = f and vim.trim(f:read("l") or "") or ""
  if f then
    f:close()
  end
  local name, pin = line:match("^([^:]+):?(%a*)$")
  if not (name and M.families[name]) then
    return "modus", nil
  end
  return name, (pin == "dark" or pin == "light") and pin or nil
end

--- Chosen family, defaulting to modus when unset or unknown.
function M.family()
  return (read_selection())
end

--- "dark"/"light" when Neovim is pinned, nil when it follows theme-mode.
function M.pin()
  local _, pin = read_selection()
  return pin
end

--- The half Neovim should show: the pin if there is one, else the shared mode.
function M.effective_mode()
  return M.pin() or M.mode()
end

--- Apply MODE with FAMILY. Without FAMILY (startup, the theme-mode push) the
--- saved family is used and a pin overrides MODE; the preview picker passes
--- both explicitly.
function M.apply(mode, family_name)
  if not family_name then
    mode = M.pin() or mode
  end
  mode = (mode == "light") and "light" or "dark"
  local family = M.families[family_name or M.family()] or M.families.modus
  local scheme = family[mode]
  vim.o.background = mode
  if family.setup then
    pcall(family.setup, scheme, mode)
  end
  -- A family whose plugin is missing (not yet synced) falls back to modus.
  if not pcall(vim.cmd.colorscheme, scheme) then
    scheme = M.families.modus[mode]
    M.families.modus.setup(scheme, mode)
    vim.cmd.colorscheme(scheme)
  end
  return mode
end

--- Save FAMILY with an optional PIN ("dark"/"light"; nil follows theme-mode)
--- and apply it.
function M.set_family(name, pin)
  local f = io.open(family_file(), "w")
  if f then
    f:write(name, pin and (":" .. pin) or "", "\n")
    f:close()
  end
  M.apply(M.mode())
end

-- Shown in the preview when the current buffer is not a real file.
local SAMPLE = {
  "(* Theme preview *)",
  "type shape = Circle of float | Rect of { w : float; h : float }",
  "",
  "let area = function",
  "  | Circle r -> Float.pi *. r *. r",
  "  | Rect { w; h } -> w *. h",
  "",
  "let rec sum = function [] -> 0 | x :: xs -> x + sum xs  (* TODO: tail-recursive *)",
  "",
  "let () =",
  "  let shapes = [ Circle 1.0; Rect { w = 2.0; h = 3.5 } ] in",
  '  List.iter (fun s -> Printf.printf "%.2f\\n" (area s)) shapes;',
  "  assert (sum [ 1; 2; 3 ] = 6)",
}

--- Picker with live preview over every variant: moving the cursor applies
--- it to the whole UI, <CR> pins Neovim to it, "follow theme-mode" keeps the
--- current family but drops the pin, and <Esc> restores what was there before.
function M.pick()
  local original, original_pin = read_selection()
  local confirmed = false

  -- Preview the buffer you were in (real colors on real code) when possible.
  local buf = vim.api.nvim_get_current_buf()
  local lines, ft = SAMPLE, "ocaml"
  if vim.bo[buf].buftype == "" and vim.bo[buf].filetype ~= "" then
    local row = vim.api.nvim_win_get_cursor(0)[1]
    local first = math.max(0, row - 15)
    lines = vim.api.nvim_buf_get_lines(buf, first, first + 40, false)
    ft = vim.bo[buf].filetype
  end

  local items = {
    {
      text = "follow theme-mode",
      family = original,
      desc = original .. ", light/dark from the desktop (now " .. M.mode() .. ")",
    },
  }
  local names = vim.tbl_keys(M.families)
  table.sort(names)
  for _, name in ipairs(names) do
    for _, mode in ipairs({ "dark", "light" }) do
      items[#items + 1] = {
        text = name .. " " .. mode,
        family = name,
        mode = mode,
        scheme = M.families[name][mode],
        desc = M.descriptions[name] or "",
      }
    end
  end
  local current = 1
  for i, item in ipairs(items) do
    if original_pin and item.family == original and item.mode == original_pin then
      current = i
    end
  end

  local function is_current(item)
    if item.mode then
      return original_pin ~= nil and item.family == original and item.mode == original_pin
    end
    return original_pin == nil
  end

  Snacks.picker.pick({
    title = "Theme",
    items = items,
    layout = { preset = "vertical" },
    format = function(item)
      local name_hl = is_current(item) and "SnacksPickerSelected" or "SnacksPickerFile"
      if not item.mode then
        return { { item.text, name_hl }, { "  " .. item.desc, "SnacksPickerComment" } }
      end
      return {
        { ("%-22s"):format(item.family), name_hl },
        {
          ("%-6s"):format(item.mode),
          item.mode == "dark" and "SnacksPickerSpecial" or "SnacksPickerLabel",
        },
        { ("%-22s"):format(item.scheme), "SnacksPickerDir" },
        { item.desc, "SnacksPickerComment" },
      }
    end,
    on_show = function(picker)
      picker.list:view(current)
    end,
    preview = function(ctx)
      local mode = ctx.item.mode or M.mode()
      ctx.preview:set_lines(lines)
      ctx.preview:highlight({ ft = ft })
      ctx.preview:set_title(ctx.item.family .. " · " .. mode)
      vim.schedule(function()
        M.apply(mode, ctx.item.family)
      end)
    end,
    confirm = function(picker, item)
      confirmed = item ~= nil
      picker:close()
      if item then
        M.set_family(item.family, item.mode)
      end
    end,
    on_close = function()
      if not confirmed then
        vim.schedule(function()
          M.apply(M.mode())
        end)
      end
    end,
  })
end

vim.keymap.set("n", "<leader>uC", M.pick, { desc = "Theme picker (live preview)" })

-- theme-mode pushes over the RPC socket; a session that was detached, suspended
-- or started without a server socket misses the push. Re-read the state file
-- when focus returns so the mismatch heals itself.
vim.api.nvim_create_autocmd({ "FocusGained", "VimResume" }, {
  group = vim.api.nvim_create_augroup("theme-mode-resync", { clear = true }),
  callback = function()
    if vim.o.background ~= M.effective_mode() then
      M.apply(M.mode())
    end
  end,
  desc = "Re-apply the shared light/dark mode after a missed push",
})

return M
